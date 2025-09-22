;; ClinicalChain: Clinical Trial Management & Patient Enrollment Protocol
;; Version: 1.0.0

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-TRIAL-NOT-FOUND (err u2))
(define-constant ERR-INVALID-COMPENSATION (err u3))
(define-constant ERR-INVALID-DURATION (err u4))
(define-constant ERR-INVALID-TITLE (err u5))
(define-constant ERR-INVALID-PROTOCOL (err u6))
(define-constant ERR-TRIAL-INACTIVE (err u7))
(define-constant ERR-ALREADY-ENROLLED (err u8))
(define-constant ERR-NOT-ENROLLED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-TRIAL-NOT-COMPLETED (err u11))
(define-constant ERR-ALREADY-REPORTED (err u12))
(define-constant ERR-INVALID-PHASE (err u13))
(define-constant ERR-INVALID-CONDITION (err u14))
(define-constant ERR-ENROLLMENT-EXPIRED (err u15))
(define-constant ERR-INVALID-ADHERENCE (err u16))

;; Constants
(define-constant MIN-COMPENSATION u2000000) ;; 2 STX minimum
(define-constant MAX-COMPENSATION u200000000000) ;; 200k STX maximum
(define-constant MIN-DURATION u1209600) ;; 2 weeks minimum
(define-constant MAX-DURATION u63072000) ;; 2 years maximum
(define-constant PLATFORM-FEE-PERCENT u4) ;; 4% platform fee
(define-constant COMPLETION-THRESHOLD u85) ;; 85% minimum adherence for completion

;; Data variables
(define-data-var next-trial-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var clinical-treasury principal tx-sender)
(define-data-var total-clinical-fees uint u0)

;; Clinical trial structure
(define-map clinical-trials
  uint
  {
    principal-investigator: principal,
    trial-title: (string-utf8 100),
    trial-protocol: (string-utf8 500),
    trial-phase: (string-utf8 10),
    medical-condition: (string-utf8 25),
    patient-compensation: uint,
    safety-bond: uint,
    trial-duration: uint,
    is-active: bool,
    total-patients: uint,
    total-completions: uint,
    created-at: uint
  })

;; Patient enrollment structure
(define-map patient-enrollments
  uint
  {
    patient: principal,
    trial-id: uint,
    enrolled-at: uint,
    expires-at: uint,
    adherence-score: uint,
    is-completed: bool,
    is-reported: bool,
    bond-secured: uint
  })

;; Patient trial access mapping
(define-map patient-trial-access
  { patient: principal, trial-id: uint }
  uint)

;; Clinical outcome records
(define-map clinical-outcomes
  { patient: principal, trial-id: uint }
  {
    reported-at: uint,
    final-adherence: uint,
    outcome-hash: (string-utf8 64)
  })

;; Private validation functions
(define-private (validate-phase (trial-phase (string-utf8 10)))
  (or 
    (is-eq trial-phase u"Phase I")
    (is-eq trial-phase u"Phase II")
    (is-eq trial-phase u"Phase III")
    (is-eq trial-phase u"Phase IV")
    (is-eq trial-phase u"Pilot")
  ))

(define-private (validate-condition (medical-condition (string-utf8 25)))
  (or 
    (is-eq medical-condition u"Cardiovascular")
    (is-eq medical-condition u"Oncology")
    (is-eq medical-condition u"Neurological")
    (is-eq medical-condition u"Infectious Disease")
    (is-eq medical-condition u"Metabolic")
    (is-eq medical-condition u"Respiratory")
    (is-eq medical-condition u"Immunological")
    (is-eq medical-condition u"Psychiatric")
  ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
  (let 
    (
      (text-length (len text))
    )
    (and 
      (>= text-length min-length)
      (<= text-length max-length)
    )
  ))

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM-FEE-PERCENT) u100))

(define-private (calculate-investigator-amount (amount uint))
  (- amount (calculate-platform-fee amount)))

(define-private (validate-bond-amount (bond-amount uint))
  (and (>= bond-amount u0) (<= bond-amount u25000000000))) ;; Max 25k STX bond

(define-private (validate-outcome-hash (outcome-hash (string-utf8 64)))
  (and (>= (len outcome-hash) u32) (<= (len outcome-hash) u64)))

;; Public functions

;; Create a new clinical trial
(define-public (create-clinical-trial 
  (trial-title (string-utf8 100))
  (trial-protocol (string-utf8 500))
  (trial-phase (string-utf8 10))
  (medical-condition (string-utf8 25))
  (patient-compensation uint)
  (safety-bond uint)
  (trial-duration uint))
  (let
    (
      (trial-id (var-get next-trial-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Validate inputs
    (asserts! (validate-text-length trial-title u15 u100) ERR-INVALID-TITLE)
    (asserts! (validate-text-length trial-protocol u100 u500) ERR-INVALID-PROTOCOL)
    (asserts! (validate-phase trial-phase) ERR-INVALID-PHASE)
    (asserts! (validate-condition medical-condition) ERR-INVALID-CONDITION)
    (asserts! (and (>= patient-compensation MIN-COMPENSATION) (<= patient-compensation MAX-COMPENSATION)) ERR-INVALID-COMPENSATION)
    (asserts! (and (>= trial-duration MIN-DURATION) (<= trial-duration MAX-DURATION)) ERR-INVALID-DURATION)
    (asserts! (validate-bond-amount safety-bond) ERR-INVALID-COMPENSATION)
    
    ;; Create trial
    (map-set clinical-trials trial-id {
      principal-investigator: tx-sender,
      trial-title: trial-title,
      trial-protocol: trial-protocol,
      trial-phase: trial-phase,
      medical-condition: medical-condition,
      patient-compensation: patient-compensation,
      safety-bond: safety-bond,
      trial-duration: trial-duration,
      is-active: true,
      total-patients: u0,
      total-completions: u0,
      created-at: current-time
    })
    
    (var-set next-trial-id (+ trial-id u1))
    (ok trial-id)
  ))

;; Enroll in clinical trial with safety bond
(define-public (enroll-trial (trial-id uint))
  (let
    (
      (trial (unwrap! (map-get? clinical-trials trial-id) ERR-TRIAL-NOT-FOUND))
      (enrollment-id (var-get next-enrollment-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (expires-at (+ current-time (get trial-duration trial)))
      (total-cost (+ (get patient-compensation trial) (get safety-bond trial)))
      (platform-fee (calculate-platform-fee (get patient-compensation trial)))
      (investigator-amount (calculate-investigator-amount (get patient-compensation trial)))
    )
    ;; Validate trial is active
    (asserts! (get is-active trial) ERR-TRIAL-INACTIVE)
    
    ;; Check if already enrolled
    (asserts! (is-none (map-get? patient-trial-access { patient: tx-sender, trial-id: trial-id })) ERR-ALREADY-ENROLLED)
    
    ;; Transfer payment to investigator and platform fee
    (try! (stx-transfer? investigator-amount tx-sender (get principal-investigator trial)))
    (try! (stx-transfer? platform-fee tx-sender (var-get clinical-treasury)))
    
    ;; Lock safety bond (simulated by requiring balance)
    (asserts! (>= (stx-get-balance tx-sender) (get safety-bond trial)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create enrollment record
    (map-set patient-enrollments enrollment-id {
      patient: tx-sender,
      trial-id: trial-id,
      enrolled-at: current-time,
      expires-at: expires-at,
      adherence-score: u0,
      is-completed: false,
      is-reported: false,
      bond-secured: (get safety-bond trial)
    })
    
    ;; Map patient to access
    (map-set patient-trial-access { patient: tx-sender, trial-id: trial-id } enrollment-id)
    
    ;; Update trial stats
    (map-set clinical-trials trial-id (merge trial { total-patients: (+ (get total-patients trial) u1) }))
    
    ;; Update clinical fees
    (var-set total-clinical-fees (+ (var-get total-clinical-fees) platform-fee))
    (var-set next-enrollment-id (+ enrollment-id u1))
    
    (ok enrollment-id)
  ))

;; Update adherence progress
(define-public (update-adherence (trial-id uint) (adherence-score uint))
  (let
    (
      (enrollment-id (unwrap! (map-get? patient-trial-access { patient: tx-sender, trial-id: trial-id }) ERR-NOT-ENROLLED))
      (enrollment-record (unwrap! (map-get? patient-enrollments enrollment-id) ERR-NOT-ENROLLED))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Validate enrollment is active
    (asserts! (< current-time (get expires-at enrollment-record)) ERR-ENROLLMENT-EXPIRED)
    (asserts! (<= adherence-score u100) ERR-INVALID-ADHERENCE)
    (asserts! (>= adherence-score (get adherence-score enrollment-record)) ERR-INVALID-ADHERENCE)
    
    ;; Update adherence
    (map-set patient-enrollments enrollment-id (merge enrollment-record { 
      adherence-score: adherence-score,
      is-completed: (>= adherence-score u100)
    }))
    
    (ok true)
  ))

;; Report clinical outcomes
(define-public (report-outcomes (trial-id uint) (outcome-hash (string-utf8 64)))
  (let
    (
      (enrollment-id (unwrap! (map-get? patient-trial-access { patient: tx-sender, trial-id: trial-id }) ERR-NOT-ENROLLED))
      (enrollment-record (unwrap! (map-get? patient-enrollments enrollment-id) ERR-NOT-ENROLLED))
      (trial (unwrap! (map-get? clinical-trials trial-id) ERR-TRIAL-NOT-FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (validated-trial-id (get trial-id enrollment-record))
      (validated-hash outcome-hash)
    )
    ;; Additional validations
    (asserts! (validate-outcome-hash outcome-hash) ERR-INVALID-PROTOCOL)
    (asserts! (is-eq trial-id validated-trial-id) ERR-TRIAL-NOT-FOUND)
    
    ;; Validate completion and score
    (asserts! (get is-completed enrollment-record) ERR-TRIAL-NOT-COMPLETED)
    (asserts! (>= (get adherence-score enrollment-record) COMPLETION-THRESHOLD) ERR-TRIAL-NOT-COMPLETED)
    (asserts! (not (get is-reported enrollment-record)) ERR-ALREADY-REPORTED)
    
    ;; Report outcomes
    (map-set clinical-outcomes { patient: tx-sender, trial-id: validated-trial-id } {
      reported-at: current-time,
      final-adherence: (get adherence-score enrollment-record),
      outcome-hash: validated-hash
    })
    
    ;; Update enrollment record
    (map-set patient-enrollments enrollment-id (merge enrollment-record { is-reported: true }))
    
    ;; Update trial stats
    (map-set clinical-trials validated-trial-id (merge trial { total-completions: (+ (get total-completions trial) u1) }))
    
    ;; Return bond to patient (simulated)
    (ok true)
  ))

;; Deactivate trial (principal investigator only)
(define-public (deactivate-trial (trial-id uint))
  (let
    (
      (trial (unwrap! (map-get? clinical-trials trial-id) ERR-TRIAL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get principal-investigator trial)) ERR-NOT-AUTHORIZED)
    (map-set clinical-trials trial-id (merge trial { is-active: false }))
    (ok true)
  ))

;; Read-only functions
(define-read-only (get-clinical-trial (trial-id uint))
  (map-get? clinical-trials trial-id))

(define-read-only (get-patient-enrollment (enrollment-id uint))
  (map-get? patient-enrollments enrollment-id))

(define-read-only (get-patient-access (patient principal) (trial-id uint))
  (match (map-get? patient-trial-access { patient: patient, trial-id: trial-id })
    enrollment-id (map-get? patient-enrollments enrollment-id)
    none
  ))

(define-read-only (get-clinical-outcome (patient principal) (trial-id uint))
  (map-get? clinical-outcomes { patient: patient, trial-id: trial-id }))

(define-read-only (is-patient-reported (patient principal) (trial-id uint))
  (is-some (map-get? clinical-outcomes { patient: patient, trial-id: trial-id })))

(define-read-only (get-trial-stats (trial-id uint))
  (match (map-get? clinical-trials trial-id)
    trial {
      total-patients: (get total-patients trial),
      total-completions: (get total-completions trial),
      completion-rate: (if (> (get total-patients trial) u0)
        (/ (* (get total-completions trial) u100) (get total-patients trial))
        u0
      )
    }
    { total-patients: u0, total-completions: u0, completion-rate: u0 }
  ))

(define-read-only (get-platform-stats)
  {
    total-trials: (- (var-get next-trial-id) u1),
    total-enrollments: (- (var-get next-enrollment-id) u1),
    total-clinical-fees: (var-get total-clinical-fees),
    clinical-treasury: (var-get clinical-treasury)
  })

(define-read-only (calculate-trial-cost (trial-id uint))
  (match (map-get? clinical-trials trial-id)
    trial {
      compensation: (get patient-compensation trial),
      bond: (get safety-bond trial),
      total: (+ (get patient-compensation trial) (get safety-bond trial)),
      platform-fee: (calculate-platform-fee (get patient-compensation trial)),
      investigator-amount: (calculate-investigator-amount (get patient-compensation trial))
    }
    { compensation: u0, bond: u0, total: u0, platform-fee: u0, investigator-amount: u0 }
  ))