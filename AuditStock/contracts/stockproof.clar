;; AuditStock Contract
;; A simple system to track products and sales

;; owner of this contract
(define-constant contract-owner tx-sender)

;;error messages 
(define-constant err-not-authorized (err u1))
(define-constant err-product-not-found (err u2))
(define-constant err-not-enough-stock (err u3))
(define-constant err-order-not-found (err u4))
(define-constant err-already-finalized (err u5))
(define-constant err-invalid-quantity (err u6))

;; counter to give each product a unique ID
(define-data-var next-product-id uint u1)
(define-data-var next-order-id uint u1)

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

;; Store orders
(define-map orders
  uint
  {
    product-id: uint,
    quantity: uint,
    cashier: principal,
    timestamp: uint,
    finalized: bool
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
;; Check if someone is a cashier
(define-read-only (is-cashier (user principal))
  (default-to false (map-get? cashiers user))
)

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products product-id)
)

;; Get order details
(define-read-only (get-order (order-id uint))
  (map-get? orders order-id)
)

;; Register a Product (Admin Only)
(define-public (register-product (product-name (string-ascii 50)) (product-price uint) (product-stock uint))
  (let
    (
      (product-id (var-get next-product-id))
    )
    ;; Only admins can add products
    (asserts! (is-admin tx-sender) err-not-authorized)
    
    ;; Save the product
    (map-set products product-id {
      name: product-name,
      price: product-price,
      stock: product-stock
    })
    
    ;; Increment ID counter
    (var-set next-product-id (+ product-id u1))
    
    ;; Print event for audit
    (print {
      event: "product-registered",
      product-id: product-id,
      name: product-name,
      price: product-price,
      stock: product-stock
    })
    
    (ok product-id)
  )
)

;; Add stock to existing product
(define-public (restock-product (product-id uint) (quantity uint))
  (let
    (
      (product (unwrap! (map-get? products product-id) err-product-not-found))
      (new-stock (+ (get stock product) quantity))
    )
    ;; Only admins can restock
    (asserts! (is-admin tx-sender) err-not-authorized)
    (asserts! (> quantity u0) err-invalid-quantity)
    
    ;; Update stock
    (map-set products product-id (merge product { stock: new-stock }))
    
    ;; Print event
    (print {
      event: "stock-added",
      product-id: product-id,
      quantity: quantity,
      new-stock: new-stock
    })
    
    (ok new-stock)
  )
)

;; Add a cashier
(define-public (add-cashier (new-cashier principal))
  (begin
    ;; Only admins can add cashiers
    (asserts! (is-admin tx-sender) err-not-authorized)
    
    (map-set cashiers new-cashier true)
    
    (ok true)
  )
)


;; ===================================
;; PUBLIC FUNCTIONS - CASHIER
;; ===================================

;; Create an order (doesn't decrease stock yet)
(define-public (create-order (product-id uint) (quantity uint))
  (let
    (
      (order-id (var-get next-order-id))
      (product (unwrap! (map-get? products product-id) err-product-not-found))
    )
    ;; Only cashiers can create orders
    (asserts! (is-cashier tx-sender) err-not-authorized)
    
    ;; Check if enough stock available
    (asserts! (>= (get stock product) quantity) err-not-enough-stock)
    (asserts! (> quantity u0) err-invalid-quantity)
    
    ;; Save the order (not finalized yet)
    (map-set orders order-id {
      product-id: product-id,
      quantity: quantity,
      cashier: tx-sender,
      timestamp: block-height,
      finalized: false
    })
    
    ;; Increment order ID
    (var-set next-order-id (+ order-id u1))
    
    ;; Print event
    (print {
      event: "order-created",
      order-id: order-id,
      product-id: product-id,
      quantity: quantity,
      cashier: tx-sender
    })
    
    (ok order-id)
  )
)

;; Finalize order - THIS DECREASES STOCK!
(define-public (finalize-order (order-id uint))
  (let
    (
      (order (unwrap! (map-get? orders order-id) err-order-not-found))
      (product (unwrap! (map-get? products (get product-id order)) err-product-not-found))
      (new-stock (- (get stock product) (get quantity order)))
    )
    ;; Only cashiers can finalize
    (asserts! (is-cashier tx-sender) err-not-authorized)
    
    ;; Order must not be finalized already
    (asserts! (is-eq (get finalized order) false) err-already-finalized)
    
    ;; Check stock again (in case it changed)
    (asserts! (>= (get stock product) (get quantity order)) err-not-enough-stock)
    
    ;; Update product stock (DECREASE!)
    (map-set products (get product-id order) 
      (merge product { stock: new-stock })
    )
    
    ;; Mark order as finalized
    (map-set orders order-id 
      (merge order { finalized: true })
    )
    
    ;; Print event
    (print {
      event: "order-finalized",
      order-id: order-id,
      product-id: (get product-id order),
      quantity: (get quantity order),
      new-stock: new-stock
    })
    
    (ok true)
  )
)