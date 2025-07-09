;; Resource Allocation Contract
;; Manages aid resource distribution and inventory tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_INSUFFICIENT_RESOURCES (err u202))
(define-constant ERR_INVALID_INPUT (err u203))
(define-constant ERR_ALREADY_ALLOCATED (err u204))

;; Data Variables
(define-data-var next-resource-id uint u1)
(define-data-var next-allocation-id uint u1)

;; Data Maps
(define-map resources
  { resource-id: uint }
  {
    name: (string-ascii 100),
    category: (string-ascii 50),
    total-quantity: uint,
    available-quantity: uint,
    unit: (string-ascii 20),
    provider: principal,
    added-at: uint,
    expiry-date: (string-ascii 10)
  }
)

(define-map allocations
  { allocation-id: uint }
  {
    refugee-id: uint,
    resource-id: uint,
    quantity: uint,
    priority-level: uint,
    status: (string-ascii 20),
    allocated-by: principal,
    allocated-at: uint,
    distributed-at: (optional uint),
    notes: (string-ascii 500)
  }
)

(define-map refugee-allocations
  { refugee-id: uint, resource-id: uint }
  { allocation-id: uint }
)

(define-map resource-providers principal bool)

(define-map priority-weights
  { priority-level: uint }
  { weight: uint }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-authorized-provider)
  (default-to false (map-get? resource-providers tx-sender))
)

;; Admin Functions
(define-public (add-resource-provider (provider principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (ok (map-set resource-providers provider true))
  )
)

(define-public (remove-resource-provider (provider principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (ok (map-delete resource-providers provider))
  )
)

(define-public (set-priority-weight (priority-level uint) (weight uint))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (ok (map-set priority-weights { priority-level: priority-level } { weight: weight }))
  )
)

;; Core Functions
(define-public (add-resource
  (name (string-ascii 100))
  (category (string-ascii 50))
  (quantity uint)
  (unit (string-ascii 20))
  (expiry-date (string-ascii 10))
)
  (let
    (
      (resource-id (var-get next-resource-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-authorized-provider) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    (asserts! (> quantity u0) ERR_INVALID_INPUT)

    (map-set resources
      { resource-id: resource-id }
      {
        name: name,
        category: category,
        total-quantity: quantity,
        available-quantity: quantity,
        unit: unit,
        provider: tx-sender,
        added-at: current-time,
        expiry-date: expiry-date
      }
    )

    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (allocate-resource
  (refugee-id uint)
  (resource-id uint)
  (quantity uint)
  (priority-level uint)
  (notes (string-ascii 500))
)
  (let
    (
      (resource-data (unwrap! (map-get? resources { resource-id: resource-id }) ERR_NOT_FOUND))
      (allocation-id (var-get next-allocation-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-authorized-provider) ERR_UNAUTHORIZED)
    (asserts! (>= (get available-quantity resource-data) quantity) ERR_INSUFFICIENT_RESOURCES)
    (asserts! (is-none (map-get? refugee-allocations { refugee-id: refugee-id, resource-id: resource-id })) ERR_ALREADY_ALLOCATED)

    ;; Update resource availability
    (map-set resources
      { resource-id: resource-id }
      (merge resource-data {
        available-quantity: (- (get available-quantity resource-data) quantity)
      })
    )

    ;; Create allocation record
    (map-set allocations
      { allocation-id: allocation-id }
      {
        refugee-id: refugee-id,
        resource-id: resource-id,
        quantity: quantity,
        priority-level: priority-level,
        status: "allocated",
        allocated-by: tx-sender,
        allocated-at: current-time,
        distributed-at: none,
        notes: notes
      }
    )

    ;; Create lookup mapping
    (map-set refugee-allocations
      { refugee-id: refugee-id, resource-id: resource-id }
      { allocation-id: allocation-id }
    )

    (var-set next-allocation-id (+ allocation-id u1))
    (ok allocation-id)
  )
)

(define-public (distribute-resource (allocation-id uint))
  (let
    (
      (allocation-data (unwrap! (map-get? allocations { allocation-id: allocation-id }) ERR_NOT_FOUND))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-authorized-provider) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status allocation-data) "allocated") ERR_INVALID_INPUT)

    (map-set allocations
      { allocation-id: allocation-id }
      (merge allocation-data {
        status: "distributed",
        distributed-at: (some current-time)
      })
    )
    (ok true)
  )
)

(define-public (cancel-allocation (allocation-id uint))
  (let
    (
      (allocation-data (unwrap! (map-get? allocations { allocation-id: allocation-id }) ERR_NOT_FOUND))
      (resource-data (unwrap! (map-get? resources { resource-id: (get resource-id allocation-data) }) ERR_NOT_FOUND))
    )
    (asserts! (is-authorized-provider) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status allocation-data) "allocated") ERR_INVALID_INPUT)

    ;; Return quantity to available resources
    (map-set resources
      { resource-id: (get resource-id allocation-data) }
      (merge resource-data {
        available-quantity: (+ (get available-quantity resource-data) (get quantity allocation-data))
      })
    )

    ;; Update allocation status
    (map-set allocations
      { allocation-id: allocation-id }
      (merge allocation-data {
        status: "cancelled"
      })
    )

    ;; Remove lookup mapping
    (map-delete refugee-allocations
      { refugee-id: (get refugee-id allocation-data), resource-id: (get resource-id allocation-data) }
    )

    (ok true)
  )
)

(define-public (update-resource-quantity (resource-id uint) (new-quantity uint))
  (let
    (
      (resource-data (unwrap! (map-get? resources { resource-id: resource-id }) ERR_NOT_FOUND))
      (allocated-quantity (- (get total-quantity resource-data) (get available-quantity resource-data)))
    )
    (asserts! (is-authorized-provider) ERR_UNAUTHORIZED)
    (asserts! (>= new-quantity allocated-quantity) ERR_INSUFFICIENT_RESOURCES)

    (map-set resources
      { resource-id: resource-id }
      (merge resource-data {
        total-quantity: new-quantity,
        available-quantity: (- new-quantity allocated-quantity)
      })
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

(define-read-only (get-allocation (allocation-id uint))
  (map-get? allocations { allocation-id: allocation-id })
)

(define-read-only (get-refugee-allocation (refugee-id uint) (resource-id uint))
  (match (map-get? refugee-allocations { refugee-id: refugee-id, resource-id: resource-id })
    lookup-result (map-get? allocations { allocation-id: (get allocation-id lookup-result) })
    none
  )
)

(define-read-only (get-resource-availability (resource-id uint))
  (match (map-get? resources { resource-id: resource-id })
    resource-data (get available-quantity resource-data)
    u0
  )
)

(define-read-only (get-priority-weight (priority-level uint))
  (default-to u1 (get weight (map-get? priority-weights { priority-level: priority-level })))
)

(define-read-only (get-next-resource-id)
  (var-get next-resource-id)
)

(define-read-only (get-next-allocation-id)
  (var-get next-allocation-id)
)
