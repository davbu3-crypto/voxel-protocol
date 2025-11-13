;; Voxel Protocol - Temporal Orchestration Smart Contract
;; A simplified implementation of temporal voxel scheduling

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-TIME (err u101))
(define-constant ERR-SLOT-TAKEN (err u102))
(define-constant ERR-INSUFFICIENT-STAKE (err u103))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u104))
(define-constant ERR-INVALID-SCHEDULE-TYPE (err u105))
(define-constant ERR-VALIDATOR-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-VALIDATOR (err u107))
(define-constant ERR-INSUFFICIENT-VALIDATOR-STAKE (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-STAKE u1000000) ;; 1 STX in micro-STX
(define-constant MIN-VALIDATOR-STAKE u5000000) ;; 5 STX in micro-STX
(define-constant EXECUTION-FEE-PERCENTAGE u5) ;; 5% fee for executors
(define-constant SCHEDULE-TYPE-ONE-TIME u1)
(define-constant SCHEDULE-TYPE-RECURRING u2)

;; Data variables
(define-data-var next-schedule-id uint u0)
(define-data-var next-validator-id uint u0)
(define-data-var protocol-treasury uint u0)

;; Data maps
(define-map schedules
    uint
    {
        creator: principal,
        execution-time: uint,
        stake-amount: uint,
        executed: bool,
        status: (string-ascii 20),
        schedule-type: uint,
        recurring-interval: uint,
        execution-count: uint
    }
)

(define-map time-slots
    uint
    principal
)

(define-map validators
    uint
    {
        validator-address: principal,
        stake-amount: uint,
        active: bool,
        reputation-score: uint,
        successful-executions: uint,
        failed-executions: uint
    }
)

(define-map validator-by-address
    principal
    uint
)

(define-map user-schedules
    principal
    (list 100 uint)
)

;; Read-only functions
(define-read-only (get-schedule (schedule-id uint))
    (map-get? schedules schedule-id)
)

(define-read-only (get-time-slot (time-slot uint))
    (map-get? time-slots time-slot)
)

(define-read-only (get-next-schedule-id)
    (ok (var-get next-schedule-id))
)

(define-read-only (get-validator (validator-id uint))
    (map-get? validators validator-id)
)

(define-read-only (get-validator-by-address (address principal))
    (match (map-get? validator-by-address address)
        validator-id (map-get? validators validator-id)
        none
    )
)

(define-read-only (get-user-schedules (user principal))
    (default-to (list) (map-get? user-schedules user))
)

(define-read-only (get-protocol-treasury)
    (ok (var-get protocol-treasury))
)

(define-read-only (calculate-execution-fee (stake-amount uint))
    (ok (/ (* stake-amount EXECUTION-FEE-PERCENTAGE) u100))
)

;; Public functions
(define-public (create-schedule (execution-time uint) (stake-amount uint) (schedule-type uint) (recurring-interval uint))
    (let
        (
            (schedule-id (var-get next-schedule-id))
            (current-block block-height)
            (user-schedule-list (default-to (list) (map-get? user-schedules tx-sender)))
        )
        ;; Validate execution time is in the future
        (asserts! (> execution-time current-block) ERR-INVALID-TIME)

        ;; Validate minimum stake
        (asserts! (>= stake-amount MIN-STAKE) ERR-INSUFFICIENT-STAKE)

        ;; Validate schedule type
        (asserts! (or (is-eq schedule-type SCHEDULE-TYPE-ONE-TIME)
                      (is-eq schedule-type SCHEDULE-TYPE-RECURRING))
                  ERR-INVALID-SCHEDULE-TYPE)

        ;; Check if time slot is available
        (asserts! (is-none (map-get? time-slots execution-time)) ERR-SLOT-TAKEN)

        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))

        ;; Create schedule
        (map-set schedules schedule-id {
            creator: tx-sender,
            execution-time: execution-time,
            stake-amount: stake-amount,
            executed: false,
            status: "pending",
            schedule-type: schedule-type,
            recurring-interval: recurring-interval,
            execution-count: u0
        })

        ;; Reserve time slot
        (map-set time-slots execution-time tx-sender)

        ;; Add to user's schedule list
        (map-set user-schedules tx-sender (unwrap-panic (as-max-len? (append user-schedule-list schedule-id) u100)))

        ;; Increment schedule ID
        (var-set next-schedule-id (+ schedule-id u1))

        (ok schedule-id)
    )
)

(define-public (execute-schedule (schedule-id uint))
    (let
        (
            (schedule (unwrap! (map-get? schedules schedule-id) ERR-SCHEDULE-NOT-FOUND))
            (current-block block-height)
            (execution-fee (unwrap-panic (calculate-execution-fee (get stake-amount schedule))))
            (return-amount (- (get stake-amount schedule) execution-fee))
        )
        ;; Only creator can execute
        (asserts! (is-eq tx-sender (get creator schedule)) ERR-NOT-AUTHORIZED)

        ;; Check if execution time has been reached
        (asserts! (>= current-block (get execution-time schedule)) ERR-INVALID-TIME)

        ;; Check if not already executed (for one-time schedules)
        (if (is-eq (get schedule-type schedule) SCHEDULE-TYPE-ONE-TIME)
            (asserts! (not (get executed schedule)) ERR-NOT-AUTHORIZED)
            true
        )

        ;; Update schedule status
        (if (is-eq (get schedule-type schedule) SCHEDULE-TYPE-ONE-TIME)
            (map-set schedules schedule-id (merge schedule {
                executed: true,
                status: "executed",
                execution-count: (+ (get execution-count schedule) u1)
            }))
            (map-set schedules schedule-id (merge schedule {
                execution-time: (+ (get execution-time schedule) (get recurring-interval schedule)),
                execution-count: (+ (get execution-count schedule) u1),
                status: "recurring-active"
            }))
        )

        ;; Add execution fee to protocol treasury
        (var-set protocol-treasury (+ (var-get protocol-treasury) execution-fee))

        ;; Return stake minus fee to creator
        (try! (as-contract (stx-transfer? return-amount tx-sender (get creator schedule))))

        (ok true)
    )
)

(define-public (cancel-schedule (schedule-id uint))
    (let
        (
            (schedule (unwrap! (map-get? schedules schedule-id) ERR-SCHEDULE-NOT-FOUND))
        )
        ;; Only creator can cancel
        (asserts! (is-eq tx-sender (get creator schedule)) ERR-NOT-AUTHORIZED)

        ;; Check if not already executed
        (asserts! (not (get executed schedule)) ERR-NOT-AUTHORIZED)

        ;; Update schedule status
        (map-set schedules schedule-id (merge schedule {
            status: "cancelled"
        }))

        ;; Remove time slot reservation
        (map-delete time-slots (get execution-time schedule))

        ;; Return stake to creator
        (try! (as-contract (stx-transfer? (get stake-amount schedule) tx-sender (get creator schedule))))

        (ok true)
    )
)

;; Validator management functions
(define-public (register-validator (stake-amount uint))
    (let
        (
            (validator-id (var-get next-validator-id))
            (existing-validator (map-get? validator-by-address tx-sender))
        )
        ;; Check if not already a validator
        (asserts! (is-none existing-validator) ERR-ALREADY-VALIDATOR)

        ;; Validate minimum validator stake
        (asserts! (>= stake-amount MIN-VALIDATOR-STAKE) ERR-INSUFFICIENT-VALIDATOR-STAKE)

        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))

        ;; Register validator
        (map-set validators validator-id {
            validator-address: tx-sender,
            stake-amount: stake-amount,
            active: true,
            reputation-score: u100,
            successful-executions: u0,
            failed-executions: u0
        })

        ;; Map address to validator ID
        (map-set validator-by-address tx-sender validator-id)

        ;; Increment validator ID
        (var-set next-validator-id (+ validator-id u1))

        (ok validator-id)
    )
)

(define-public (deactivate-validator)
    (let
        (
            (validator-id (unwrap! (map-get? validator-by-address tx-sender) ERR-VALIDATOR-NOT-FOUND))
            (validator (unwrap! (map-get? validators validator-id) ERR-VALIDATOR-NOT-FOUND))
        )
        ;; Check if validator is active
        (asserts! (get active validator) ERR-NOT-AUTHORIZED)

        ;; Deactivate validator
        (map-set validators validator-id (merge validator {
            active: false
        }))

        ;; Return stake
        (try! (as-contract (stx-transfer? (get stake-amount validator) tx-sender tx-sender)))

        (ok true)
    )
)

(define-public (update-validator-reputation (validator-id uint) (success bool))
    (let
        (
            (validator (unwrap! (map-get? validators validator-id) ERR-VALIDATOR-NOT-FOUND))
        )
        ;; Update reputation based on execution result
        (if success
            (map-set validators validator-id (merge validator {
                successful-executions: (+ (get successful-executions validator) u1),
                reputation-score: (+ (get reputation-score validator) u10)
            }))
            (map-set validators validator-id (merge validator {
                failed-executions: (+ (get failed-executions validator) u1),
                reputation-score: (if (> (get reputation-score validator) u10)
                                     (- (get reputation-score validator) u10)
                                     u0)
            }))
        )

        (ok true)
    )
)

;; Administrative functions
(define-public (withdraw-from-treasury (amount uint) (recipient principal))
    (begin
        ;; Only contract owner can withdraw
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

        ;; Check sufficient treasury balance
        (asserts! (>= (var-get protocol-treasury) amount) ERR-INSUFFICIENT-STAKE)

        ;; Update treasury
        (var-set protocol-treasury (- (var-get protocol-treasury) amount))

        ;; Transfer to recipient
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))

        (ok true)
    )
)
