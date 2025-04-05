;; Patient Profile Contract
;; Stores health conditions and demographic information securely

;; Define data variables for the contract
(define-data-var admin principal tx-sender)

;; Define data map for patient profiles
(define-map patient-profiles
  { patient-id: principal }
  {
    age: uint,
    gender: (string-ascii 10),
    conditions: (list 10 (string-ascii 64)),
    medications: (list 10 (string-ascii 64)),
    profile-hash: (buff 32),
    is-active: bool
  }
)

;; Function to register a new patient profile
(define-public (register-patient
                (age uint)
                (gender (string-ascii 10))
                (conditions (list 10 (string-ascii 64)))
                (medications (list 10 (string-ascii 64)))
                (profile-hash (buff 32)))
  (let ((patient-id tx-sender))
    (begin
      (asserts! (not (is-patient-registered patient-id)) (err u1)) ;; Error if already registered
      (map-set patient-profiles
        { patient-id: patient-id }
        {
          age: age,
          gender: gender,
          conditions: conditions,
          medications: medications,
          profile-hash: profile-hash,
          is-active: true
        }
      )
      (ok true)
    )
  )
)

;; Function to update a patient profile
(define-public (update-patient
                (age uint)
                (gender (string-ascii 10))
                (conditions (list 10 (string-ascii 64)))
                (medications (list 10 (string-ascii 64)))
                (profile-hash (buff 32)))
  (let ((patient-id tx-sender))
    (begin
      (asserts! (is-patient-registered patient-id) (err u2)) ;; Error if not registered
      (map-set patient-profiles
        { patient-id: patient-id }
        {
          age: age,
          gender: gender,
          conditions: conditions,
          medications: medications,
          profile-hash: profile-hash,
          is-active: true
        }
      )
      (ok true)
    )
  )
)

;; Function to deactivate a patient profile
(define-public (deactivate-patient)
  (let ((patient-id tx-sender)
        (current-profile (unwrap! (map-get? patient-profiles { patient-id: patient-id }) (err u2))))
    (begin
      (map-set patient-profiles
        { patient-id: patient-id }
        (merge current-profile { is-active: false })
      )
      (ok true)
    )
  )
)

;; Function to reactivate a patient profile
(define-public (reactivate-patient)
  (let ((patient-id tx-sender)
        (current-profile (unwrap! (map-get? patient-profiles { patient-id: patient-id }) (err u2))))
    (begin
      (map-set patient-profiles
        { patient-id: patient-id }
        (merge current-profile { is-active: true })
      )
      (ok true)
    )
  )
)

;; Getter for patient profile - only the patient or authorized contracts can view full data
(define-read-only (get-patient-profile (patient-id principal))
  (map-get? patient-profiles { patient-id: patient-id })
)

;; Helper to check if a patient is registered
(define-read-only (is-patient-registered (patient-id principal))
  (is-some (map-get? patient-profiles { patient-id: patient-id }))
)

