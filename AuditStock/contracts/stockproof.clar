;; AuditStock Contract
;; A simple system to track products and sales

;; owner of this contract
(define-constant contract-owner tx-sender)

;;error messages 
(define-constant err-not-authorized (err u1))
(define-constant err-product-not-found (err u2))
(define-constant err-not-enough-stock (err u3))

;; counter to give each product a unique ID
(define-data-var next-product-id uint u1)

;; Store all products
;; Maps work like: key -> value
;; Product ID -> Product Details
(define-map products
  uint  ;; This is the key (product ID)
  {
    name: (string-ascii 50),  ;; Product name (max 50 characters)
    price: uint,              ;; Price in smallest unit
    stock: uint               ;; How many items in stock
  }
)

;; track who is admin and cashier
(define-map admins principal bool)
(define-map cashiers principal bool)

;; Make the contract owner an admin automatically
(map-set admins contract-owner true)
;; Read-only function 
(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

;; Register a Product (Admin Only)
(define-public (register-product (product-name (string-ascii 50)) (product-price uint) (product-stock uint))
  (let
    (
      (product-id (var-get next-product-id))  ;; Get current ID
    )
    ;; Check: Is the caller an admin?
    (asserts! (is-admin tx-sender) err-not-authorized)
    
    ;; Save the product
    (map-set products product-id {
      name: product-name,
      price: product-price,
      stock: product-stock
    })
    
    ;; Increment the ID for next product
    (var-set next-product-id (+ product-id u1))
    
    ;; Return success with the product ID
    (ok product-id)
  )
)