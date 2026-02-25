;; StockProof Contract
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
(define-constant err-empty-batch (err u7))
(define-constant err-batch-not-found (err u8))
(define-constant err-batch-already-finalized (err u9))
(define-constant max-batch-items u20)  ;; Max 20 items per batch

;; counter to give each product a unique ID
(define-data-var next-product-id uint u1)
(define-data-var next-order-id uint u1)
;; Counter for batch orders
(define-data-var next-batch-id uint u1)


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
;; Store batch order header
(define-map batch-orders
  uint  ;; batch-id
  {
    cashier: principal,
    timestamp: uint,
    finalized: bool,
    total-items: uint
  }
)
;; Store individual items in the batch
(define-map batch-items
  {batch-id: uint, item-index: uint}  ;; Composite key
  {
    product-id: uint,
    quantity: uint
  }
)


;; track who is admin and cashier (DELEGATED to access-manager)

;; Read-only function 
(define-read-only (is-admin (user principal))
  (contract-call? .access-manager is-admin user)
)

;; Check if someone is a cashier
(define-read-only (is-cashier (user principal))
  (contract-call? .access-manager is-cashier user)
)
;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products product-id)
)
;; Get order details
(define-read-only (get-order (order-id uint))
  (map-get? orders order-id)
)
;; Get batch order details
(define-read-only (get-batch-order (batch-id uint))
  (map-get? batch-orders batch-id)
)
(define-read-only (get-batch-item (batch-id uint) (item-index uint))
  (map-get? batch-items {batch-id: batch-id, item-index: item-index})
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

;; Note: add-cashier has been moved to access-manager.clar


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
      timestamp: burn-block-height,
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


;; Check if all items in batch have enough stock
(define-private (check-batch-stock (items (list 20 {product-id: uint, quantity: uint})))
  (fold check-single-item-stock items true)
)

(define-private (check-single-item-stock 
  (item {product-id: uint, quantity: uint})
  (prev-result bool))
  (match (map-get? products (get product-id item))
    product
      (and prev-result 
           (>= (get stock product) (get quantity item))
           (> (get quantity item) u0))
    false
  )
)

;; Store all items in batch
(define-private (store-batch-items 
  (batch-id uint)
  (items (list 20 {product-id: uint, quantity: uint}))
  (start-index uint))
  (fold store-single-item items start-index)
)

(define-private (store-single-item
  (item {product-id: uint, quantity: uint})
  (index uint))
  (begin
    (map-set batch-items
      {batch-id: (var-get next-batch-id), item-index: index}
      {
        product-id: (get product-id item),
        quantity: (get quantity item)
      }
    )
    (+ index u1)
  )
)


;; Create a batch order with multiple products
(define-public (create-batch-order (items (list 20 {product-id: uint, quantity: uint})))
  (let
    (
      (batch-id (var-get next-batch-id))
      (items-count (len items))
    )
    ;; Only cashiers can create batch orders
    (asserts! (is-cashier tx-sender) err-not-authorized)
    
    ;; Batch must not be empty
    (asserts! (> items-count u0) err-empty-batch)
    
    ;; Check all items have valid stock before creating batch
    (asserts! (check-batch-stock items) err-not-enough-stock)
    
    ;; Create batch header
    (map-set batch-orders batch-id {
      cashier: tx-sender,
      timestamp: burn-block-height,
      finalized: false,
      total-items: items-count
    })
    
    ;; Store each item in the batch
    (store-batch-items batch-id items u0)
    
    ;; Increment batch ID
    (var-set next-batch-id (+ batch-id u1))
    
    ;; Print event
    (print {
      event: "batch-created",
      batch-id: batch-id,
      total-items: items-count,
      cashier: tx-sender
    })
    
    (ok batch-id)
  )
)

;; Finalize batch order - decreases stock for ALL items
(define-public (finalize-batch-order (batch-id uint))
  (let
    (
      (batch (unwrap! (map-get? batch-orders batch-id) err-batch-not-found))
    )
    ;; Only cashiers can finalize
    (asserts! (is-cashier tx-sender) err-not-authorized)
    
    ;; Batch must not be finalized already
    (asserts! (is-eq (get finalized batch) false) err-batch-already-finalized)
    
    ;; Process all items (decrease stock for each)
    (var-set current-processing-batch-id batch-id)
    (asserts! (is-ok (process-batch-items (get total-items batch))) err-not-enough-stock)
    
    ;; Mark batch as finalized
    (map-set batch-orders batch-id 
      (merge batch {finalized: true})
    )
    
    ;; Print event
    (print {
      event: "batch-finalized",
      batch-id: batch-id,
      total-items: (get total-items batch),
      cashier: tx-sender
    })
    
    (ok true)
  )
)

(define-data-var current-processing-batch-id uint u0)

;; Process all items in batch using fold to avoid circular dependencies
(define-private (process-batch-items (total-items uint))
  (fold process-single-batch-item 
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
    (ok true))
)

(define-private (process-single-batch-item (index uint) (previous-result (response bool uint)))
  (if (is-err previous-result)
    previous-result
    (let
      (
        (batch-id (var-get current-processing-batch-id))
        (batch (unwrap! (map-get? batch-orders batch-id) (err u8)))
      )
      (if (< index (get total-items batch))
        (let
          (
            (item (unwrap! (map-get? batch-items {batch-id: batch-id, item-index: index}) (err u8)))
            (product (unwrap! (map-get? products (get product-id item)) (err u2)))
            (new-stock (- (get stock product) (get quantity item)))
          )
          (if (>= (get stock product) (get quantity item))
            (begin
              (map-set products (get product-id item) (merge product {stock: new-stock}))
              (ok true)
            )
            (err u3)
          )
        )
        (ok true) ;; Index out of range for this batch, skip
      )
    )
  )
)