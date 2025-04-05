;; Trial Criteria Contract
;; Defines eligibility requirements for clinical trials

;; Define data variables for the contract
(define-data-var admin principal tx-sender)

;; Define data map for clinical trials
(define-map clinical-trials
  { trial-id: uint }
  {
    sponsor: principal,
    name: (string-ascii 100),
    condition-targets: (list 10 (string-ascii 64)),
    min-age: uint,
    max-age: uint,
    gender-criteria: (string-ascii 10), ;; "any", "male", "female"
    exclusion-medications: (list 10 (string-ascii 64)),
    is-active: bool,
    max-participants: uint,
    current-participants: uint
  }
)

;; Keep track of the next trial ID
(define-data-var next-trial-id uint u1)

;; Function to register a new clinical trial
(define-public (register-trial
                (name (string-ascii 100))
                (condition-targets (list 10 (string-ascii 64)))
                (min-age uint)
                (max-age uint)
                (gender-criteria (string-ascii 10))
                (exclusion-medications (list 10 (string-ascii 64)))
                (max-participants uint))
  (let ((trial-id (var-get next-trial-id))
        (sponsor tx-sender))
    (begin
      (map-set clinical-trials
        { trial-id: trial-id }
        {
          sponsor: sponsor,
          name: name,
          condition-targets: condition-targets,
          min-age: min-age,
          max-age: max-age,
          gender-criteria: gender-criteria,
          exclusion-medications: exclusion-medications,
          is-active: true,
          max-participants: max-participants,
          current-participants: u0
        }
      )
      (var-set next-trial-id (+ trial-id u1))
      (ok trial-id)
    )
  )
)

;; Function to update a clinical trial
(define-public (update-trial
                (trial-id uint)
                (name (string-ascii 100))
                (condition-targets (list 10 (string-ascii 64)))
                (min-age uint)
                (max-age uint)
                (gender-criteria (string-ascii 10))
                (exclusion-medications (list 10 (string-ascii 64)))
                (max-participants uint))
  (let ((trial (unwrap! (map-get? clinical-trials { trial-id: trial-id }) (err u1))))
    (begin
      ;; Only the sponsor can update the trial
      (asserts! (is-eq tx-sender (get sponsor trial)) (err u403))

      (map-set clinical-trials
        { trial-id: trial-id }
        {
          sponsor: (get sponsor trial),
          name: name,
          condition-targets: condition-targets,
          min-age: min-age,
          max-age: max-age,
          gender-criteria: gender-criteria,
          exclusion-medications: exclusion-medications,
          is-active: (get is-active trial),
          max-participants: max-participants,
          current-participants: (get current-participants trial)
        }
      )
      (ok true)
    )
  )
)

;; Function to change trial active status
(define-public (set-trial-status (trial-id uint) (active bool))
  (let ((trial (unwrap! (map-get? clinical-trials { trial-id: trial-id }) (err u1))))
    (begin
      ;; Only the sponsor can change status
      (asserts! (is-eq tx-sender (get sponsor trial)) (err u403))

      (map-set clinical-trials
        { trial-id: trial-id }
        (merge trial { is-active: active })
      )
      (ok true)
    )
  )
)

;; Function to increment participant count
(define-public (increment-participant-count (trial-id uint))
  (let ((trial (unwrap! (map-get? clinical-trials { trial-id: trial-id }) (err u1))))
    (begin
      ;; Check if trial is active and has space
      (asserts! (get is-active trial) (err u2))
      (asserts! (< (get current-participants trial) (get max-participants trial)) (err u3))

      (map-set clinical-trials
        { trial-id: trial-id }
        (merge trial { current-participants: (+ (get current-participants trial) u1) })
      )
      (ok true)
    )
  )
)

;; Function to decrement participant count (if patient withdraws)
(define-public (decrement-participant-count (trial-id uint))
  (let ((trial (unwrap! (map-get? clinical-trials { trial-id: trial-id }) (err u1))))
    (begin
      ;; Check if there are participants to decrement
      (asserts! (> (get current-participants trial) u0) (err u2))

      (map-set clinical-trials
        { trial-id: trial-id }
        (merge trial { current-participants: (- (get current-participants trial) u1) })
      )
      (ok true)
    )
  )
)

;; Getter for trial information - this is public information
(define-read-only (get-trial-info (trial-id uint))
  (map-get? clinical-trials { trial-id: trial-id })
)

;; Getter for trial by ID
(define-read-only (get-trial-by-id (id uint))
  (map-get? clinical-trials { trial-id: id })
)

;; Get the next trial ID
(define-read-only (get-next-trial-id)
  (var-get next-trial-id)
)

