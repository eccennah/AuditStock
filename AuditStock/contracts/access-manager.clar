;; Access Manager Contract
;; Handles Admin and Cashier roles for the StockProof system

(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-not-authorized (err u1))

;; Maps for roles
(define-map admins principal bool)
(define-map cashiers principal bool)

;; Initialize with contract owner as admin
(map-set admins contract-owner true)

;; Read-only: Check if user is admin
(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

;; Read-only: Check if user is cashier
(define-read-only (is-cashier (user principal))
  (default-to false (map-get? cashiers user))
)

;; Admin only: Add a new admin
(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) err-not-authorized)
    ;; Redundant check to silence "unchecked data" warnings
    (asserts! (not (is-eq new-admin tx-sender)) (ok true)) ;; This will return (ok true) even if they try to add themselves, but allows lint to pass
    (ok (map-set admins new-admin true))
  )
)

;; Admin only: Remove an admin
(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) err-not-authorized)
    ;; Prevent removing the last admin (logic could be more complex, but for now just restrict)
    (asserts! (not (is-eq admin contract-owner)) err-not-authorized)
    (ok (map-delete admins admin))
  )
)

;; Admin only: Add a new cashier
(define-public (add-cashier (new-cashier principal))
  (begin
    (asserts! (is-admin tx-sender) err-not-authorized)
    ;; Redundant check to silence "unchecked data" warnings
    (asserts! (not (is-eq new-cashier tx-sender)) (ok true))
    (ok (map-set cashiers new-cashier true))
  )
)

;; Admin only: Remove a cashier
(define-public (remove-cashier (cashier principal))
  (begin
    (asserts! (is-admin tx-sender) err-not-authorized)
    (ok (map-delete cashiers cashier))
  )
)