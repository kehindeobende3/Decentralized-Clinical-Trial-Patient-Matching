;; Consent Management Contract
;; Tracks patient approvals for participating in trials

;; Define data variables for the contract
(define-data-var admin principal tx-sender)

;; Define data map for consent records
(define-map consent-records
  { patient-id: principal, trial-id: uint }
  {
    has-consented: bool,
    consent-date: uint,
    consent-version: (string-ascii 20),
    consent-document-hash: (buff 32),
    revoked: bool,
    revoke-date: (optional uint)
  }
)

;; Function to record consent for a trial
(define-public (give-consent
                (trial-id uint)
                (consent-version (string-ascii 20))
                (consent-document-hash (buff 32)))
  (let ((patient-id tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (begin
      ;; Record the consent
      (map-set consent-records
        { patient-id: patient-id, trial-id: trial-id }
        {
          has-consented: true,
          consent-date: current-time,
          consent-version: consent-version,
          consent-document-hash: consent-document-hash,
          revoked: false,
          revoke-date: none
        }
      )

      (ok true)
    )
  )
)

;; Function to revoke consent for a trial
(define-public (revoke-consent (trial-id uint))
  (let ((patient-id tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (consent (unwrap! (map-get? consent-records { patient-id: patient-id, trial-id: trial-id }) (err u1))))
    (begin
      ;; Verify consent exists and is not already revoked
      (asserts! (get has-consented consent) (err u2))
      (asserts! (not (get revoked consent)) (err u3))

      ;; Update the consent record
      (map-set consent-records
        { patient-id: patient-id, trial-id: trial-id }
        (merge consent {
          revoked: true,
          revoke-date: (some current-time)
        })
      )

      (ok true)
    )
  )
)

;; Getter for consent status
(define-read-only (check-consent-status (patient-id principal) (trial-id uint))
  (map-get? consent-records { patient-id: patient-id, trial-id: trial-id })
)

;; Getter for validity of consent
(define-read-only (is-consent-valid (patient-id principal) (trial-id uint))
  (match (map-get? consent-records { patient-id: patient-id, trial-id: trial-id })
    consent (and (get has-consented consent) (not (get revoked consent)))
    false
  )
)

;; Function for trial sponsors to verify consent
(define-public (verify-consent (patient-id principal) (trial-id uint) (sponsor principal))
  (let ((consent (unwrap! (map-get? consent-records { patient-id: patient-id, trial-id: trial-id }) (err u2))))
    (begin
      ;; Only the trial sponsor can verify
      (asserts! (is-eq tx-sender sponsor) (err u403))

      ;; Return consent validity
      (ok (and (get has-consented consent) (not (get revoked consent))))
    )
  )
)

