;; Resettlement Matching Contract
;; Connects refugees with host communities

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_NOT_FOUND (err u301))
(define-constant ERR_INVALID_INPUT (err u302))
(define-constant ERR_ALREADY_MATCHED (err u303))
(define-constant ERR_INSUFFICIENT_CAPACITY (err u304))

;; Data Variables
(define-data-var next-host-id uint u1)
(define-data-var next-match-id uint u1)

;; Data Maps
(define-map host-communities
  { host-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    capacity: uint,
    available-capacity: uint,
    host-type: (string-ascii 50),
    languages: (string-ascii 200),
    cultural-preferences: (string-ascii 200),
    contact-person: principal,
    registered-at: uint,
    status: (string-ascii 20)
  }
)

(define-map refugee-preferences
  { refugee-id: uint }
  {
    preferred-location: (string-ascii 100),
    language-requirements: (string-ascii 200),
    cultural-needs: (string-ascii 200),
    family-size: uint,
    special-requirements: (string-ascii 500),
    updated-at: uint
  }
)

(define-map matches
  { match-id: uint }
  {
    refugee-id: uint,
    host-id: uint,
    match-score: uint,
    status: (string-ascii 20),
    matched-by: principal,
    matched-at: uint,
    confirmed-at: (optional uint),
    notes: (string-ascii 500)
  }
)

(define-map refugee-matches
  { refugee-id: uint }
  { match-id: uint }
)

(define-map host-matches
  { host-id: uint, refugee-id: uint }
  { match-id: uint }
)

(define-map authorized-coordinators principal bool)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-authorized-coordinator)
  (default-to false (map-get? authorized-coordinators tx-sender))
)

;; Admin Functions
(define-public (add-coordinator (coordinator principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (ok (map-set authorized-coordinators coordinator true))
  )
)

(define-public (remove-coordinator (coordinator principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (ok (map-delete authorized-coordinators coordinator))
  )
)

;; Core Functions
(define-public (register-host-community
  (name (string-ascii 100))
  (location (string-ascii 100))
  (capacity uint)
  (host-type (string-ascii 50))
  (languages (string-ascii 200))
  (cultural-preferences (string-ascii 200))
)
  (let
    (
      (host-id (var-get next-host-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    (asserts! (> (len location) u0) ERR_INVALID_INPUT)
    (asserts! (> capacity u0) ERR_INVALID_INPUT)

    (map-set host-communities
      { host-id: host-id }
      {
        name: name,
        location: location,
        capacity: capacity,
        available-capacity: capacity,
        host-type: host-type,
        languages: languages,
        cultural-preferences: cultural-preferences,
        contact-person: tx-sender,
        registered-at: current-time,
        status: "active"
      }
    )

    (var-set next-host-id (+ host-id u1))
    (ok host-id)
  )
)

(define-public (set-refugee-preferences
  (refugee-id uint)
  (preferred-location (string-ascii 100))
  (language-requirements (string-ascii 200))
  (cultural-needs (string-ascii 200))
  (family-size uint)
  (special-requirements (string-ascii 500))
)
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-authorized-coordinator) ERR_UNAUTHORIZED)
    (asserts! (> family-size u0) ERR_INVALID_INPUT)

    (map-set refugee-preferences
      { refugee-id: refugee-id }
      {
        preferred-location: preferred-location,
        language-requirements: language-requirements,
        cultural-needs: cultural-needs,
        family-size: family-size,
        special-requirements: special-requirements,
        updated-at: current-time
      }
    )
    (ok true)
  )
)

(define-public (create-match
  (refugee-id uint)
  (host-id uint)
  (match-score uint)
  (notes (string-ascii 500))
)
  (let
    (
      (host-data (unwrap! (map-get? host-communities { host-id: host-id }) ERR_NOT_FOUND))
      (refugee-prefs (map-get? refugee-preferences { refugee-id: refugee-id }))
      (match-id (var-get next-match-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (family-size (default-to u1 (get family-size refugee-prefs)))
    )
    (asserts! (is-authorized-coordinator) ERR_UNAUTHORIZED)
    (asserts! (>= (get available-capacity host-data) family-size) ERR_INSUFFICIENT_CAPACITY)
    (asserts! (is-none (map-get? refugee-matches { refugee-id: refugee-id })) ERR_ALREADY_MATCHED)

    ;; Update host capacity
    (map-set host-communities
      { host-id: host-id }
      (merge host-data {
        available-capacity: (- (get available-capacity host-data) family-size)
      })
    )

    ;; Create match record
    (map-set matches
      { match-id: match-id }
      {
        refugee-id: refugee-id,
        host-id: host-id,
        match-score: match-score,
        status: "pending",
        matched-by: tx-sender,
        matched-at: current-time,
        confirmed-at: none,
        notes: notes
      }
    )

    ;; Create lookup mappings
    (map-set refugee-matches
      { refugee-id: refugee-id }
      { match-id: match-id }
    )

    (map-set host-matches
      { host-id: host-id, refugee-id: refugee-id }
      { match-id: match-id }
    )

    (var-set next-match-id (+ match-id u1))
    (ok match-id)
  )
)

(define-public (confirm-match (match-id uint))
  (let
    (
      (match-data (unwrap! (map-get? matches { match-id: match-id }) ERR_NOT_FOUND))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-authorized-coordinator) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status match-data) "pending") ERR_INVALID_INPUT)

    (map-set matches
      { match-id: match-id }
      (merge match-data {
        status: "confirmed",
        confirmed-at: (some current-time)
      })
    )
    (ok true)
  )
)

(define-public (cancel-match (match-id uint))
  (let
    (
      (match-data (unwrap! (map-get? matches { match-id: match-id }) ERR_NOT_FOUND))
      (host-data (unwrap! (map-get? host-communities { host-id: (get host-id match-data) }) ERR_NOT_FOUND))
      (refugee-prefs (map-get? refugee-preferences { refugee-id: (get refugee-id match-data) }))
      (family-size (default-to u1 (get family-size refugee-prefs)))
    )
    (asserts! (is-authorized-coordinator) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status match-data) "confirmed")) ERR_INVALID_INPUT)

    ;; Return capacity to host
    (map-set host-communities
      { host-id: (get host-id match-data) }
      (merge host-data {
        available-capacity: (+ (get available-capacity host-data) family-size)
      })
    )

    ;; Update match status
    (map-set matches
      { match-id: match-id }
      (merge match-data {
        status: "cancelled"
      })
    )

    ;; Remove lookup mappings
    (map-delete refugee-matches { refugee-id: (get refugee-id match-data) })
    (map-delete host-matches { host-id: (get host-id match-data), refugee-id: (get refugee-id match-data) })

    (ok true)
  )
)

(define-public (update-host-capacity (host-id uint) (new-capacity uint))
  (let
    (
      (host-data (unwrap! (map-get? host-communities { host-id: host-id }) ERR_NOT_FOUND))
      (used-capacity (- (get capacity host-data) (get available-capacity host-data)))
    )
    (asserts! (or (is-contract-owner) (is-eq tx-sender (get contact-person host-data))) ERR_UNAUTHORIZED)
    (asserts! (>= new-capacity used-capacity) ERR_INSUFFICIENT_CAPACITY)

    (map-set host-communities
      { host-id: host-id }
      (merge host-data {
        capacity: new-capacity,
        available-capacity: (- new-capacity used-capacity)
      })
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-host-community (host-id uint))
  (map-get? host-communities { host-id: host-id })
)

(define-read-only (get-refugee-preferences (refugee-id uint))
  (map-get? refugee-preferences { refugee-id: refugee-id })
)

(define-read-only (get-match (match-id uint))
  (map-get? matches { match-id: match-id })
)

(define-read-only (get-refugee-match (refugee-id uint))
  (match (map-get? refugee-matches { refugee-id: refugee-id })
    lookup-result (map-get? matches { match-id: (get match-id lookup-result) })
    none
  )
)

(define-read-only (get-host-match (host-id uint) (refugee-id uint))
  (match (map-get? host-matches { host-id: host-id, refugee-id: refugee-id })
    lookup-result (map-get? matches { match-id: (get match-id lookup-result) })
    none
  )
)

(define-read-only (get-host-availability (host-id uint))
  (match (map-get? host-communities { host-id: host-id })
    host-data (get available-capacity host-data)
    u0
  )
)

(define-read-only (get-next-host-id)
  (var-get next-host-id)
)

(define-read-only (get-next-match-id)
  (var-get next-match-id)
)
