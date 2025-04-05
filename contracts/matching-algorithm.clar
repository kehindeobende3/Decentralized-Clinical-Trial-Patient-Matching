;; Matching Algorithm Contract
;; Connects suitable patients with appropriate trials

;; Define data variables for the contract
(define-data-var admin principal tx-sender)

;; Define data map for match records
(define-map match-records
  { patient-id: principal, trial-id: uint }
  {
    match-score: uint,
    match-status: (string-ascii 20), ;; "potential", "invited", "enrolled", "completed", "withdrawn"
    match-date: uint
  }
)

;; Define a map to track all matches for a patient
(define-map patient-matches
  { patient-id: principal }
  { trial-ids: (list 20 uint) }
)

;; Define a map to track all matched patients for a trial
(define-map trial-matches
  { trial-id: uint }
  { patient-ids: (list 100 principal) }
)

;; Function to manually record a match
(define-public (record-match (patient-id principal) (trial-id uint))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (patient-match-list (default-to { trial-ids: (list) }
                                      (map-get? patient-matches { patient-id: patient-id })))
        (trial-match-list (default-to { patient-ids: (list) }
                                     (map-get? trial-matches { trial-id: trial-id }))))
    (begin
      ;; Record the match details
      (map-set match-records
        { patient-id: patient-id, trial-id: trial-id }
        {
          match-score: u100, ;; Simplified scoring for this example
          match-status: "potential",
          match-date: current-time
        }
      )

      ;; Update patient's trial list
      (map-set patient-matches
        { patient-id: patient-id }
        { trial-ids: (unwrap-panic (as-max-len?
                                    (append (get trial-ids patient-match-list) trial-id)
                                    u20)) }
      )

      ;; Update trial's patient list
      (map-set trial-matches
        { trial-id: trial-id }
        { patient-ids: (unwrap-panic (as-max-len?
                                     (append (get patient-ids trial-match-list) patient-id)
                                     u100)) }
      )

      (ok true)
    )
  )
)

;; Function to update match status (used when patient consents or withdraws)
(define-public (update-match-status (patient-id principal) (trial-id uint) (new-status (string-ascii 20)))
  (let ((match-record (unwrap! (map-get? match-records { patient-id: patient-id, trial-id: trial-id }) (err u1))))
    (begin
      ;; Update the match status
      (map-set match-records
        { patient-id: patient-id, trial-id: trial-id }
        (merge match-record { match-status: new-status })
      )

      (ok true)
    )
  )
)

;; Getter for match details
(define-read-only (get-match (patient-id principal) (trial-id uint))
  (map-get? match-records { patient-id: patient-id, trial-id: trial-id })
)

;; Getter for patient matches
(define-read-only (get-patient-matches (patient-id principal))
  (map-get? patient-matches { patient-id: patient-id })
)

;; Getter for trial matches
(define-read-only (get-trial-matches (trial-id uint))
  (map-get? trial-matches { trial-id: trial-id })
)

