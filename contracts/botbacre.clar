;; Bitcoin-Backed Real Estate Tokenization Contract 
;; Enables fractional ownership of real estate backed by Bitcoin collateral 
;; with automated rental income distribution 
 
;; Error constants 
(define-constant ERR-NOT-AUTHORIZED (err u100)) 
(define-constant ERR-PROPERTY-NOT-FOUND (err u101)) 
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u102)) 
(define-constant ERR-INSUFFICIENT-TOKENS (err u103)) 
(define-constant ERR-PROPERTY-ALREADY-EXISTS (err u104)) 
(define-constant ERR-INVALID-AMOUNT (err u105)) 
(define-constant ERR-RENTAL-DISTRIBUTION-FAILED (err u106)) 
 
;; Contract constants 
(define-constant CONTRACT-OWNER tx-sender) 
(define-constant MIN-COLLATERAL-RATIO u150) ;; 150% collateralization required 
 
;; Data maps and variables 
(define-map properties  
  { property-id: uint } 
  { 
    owner: principal, 
    total-tokens: uint, 
    token-price: uint, 
    bitcoin-collateral: uint, 
    rental-income-pool: uint, 
    property-value: uint, 
    is-active: bool, 
    created-at: uint 
  } 
) 
 
(define-map token-balances 
  { property-id: uint, holder: principal } 
  { balance: uint } 
) 
 
(define-map rental-distributions 
  { property-id: uint, distribution-id: uint } 
  { 
    total-amount: uint, 
    per-token-amount: uint, 
    distributed-at: uint 
  } 
) 
 
(define-map user-claimed-distributions 
  { property-id: uint, holder: principal, distribution-id: uint } 
  { claimed: bool } 
) 
 
(define-data-var next-property-id uint u1) 
(define-data-var next-distribution-id uint u1) 
;; Private helper functions 
(define-private (calculate-collateral-requirement (property-value uint)) 
  (* property-value MIN-COLLATERAL-RATIO)) 
 
(define-private (get-token-balance-or-zero (property-id uint) (holder principal)) 
  (default-to u0 (get balance (map-get? token-balances { property-id: property-id, holder: 
holder }))))
;; Property tokenization functions 
(define-public (create-property (property-value uint) (total-tokens uint) (bitcoin-collateral uint)) 
  (let ((property-id (var-get next-property-id)) 
        (required-collateral (/ (calculate-collateral-requirement property-value) u100))) 
    (asserts! (>= bitcoin-collateral required-collateral) ERR-INSUFFICIENT-COLLATERAL) 
    (asserts! (> total-tokens u0) ERR-INVALID-AMOUNT) 
    (asserts! (> property-value u0) ERR-INVALID-AMOUNT) 
     
    (map-set properties  
      { property-id: property-id } 
      { 
        owner: tx-sender, 
        total-tokens: total-tokens, 
        token-price: (/ property-value total-tokens), 
        bitcoin-collateral: bitcoin-collateral, 
        rental-income-pool: u0, 
        property-value: property-value, 
        is-active: true, 
        created-at: block-height 
      }) 
     
    (map-set token-balances  
      { property-id: property-id, holder: tx-sender } 
      { balance: total-tokens }) 
     
    (var-set next-property-id (+ property-id u1)) 
    (ok property-id))) 
 
(define-public (transfer-tokens (property-id uint) (amount uint) (recipient principal)) 
  (let ((sender-balance (get-token-balance-or-zero property-id tx-sender)) 
        (recipient-balance (get-token-balance-or-zero property-id recipient))) 
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-TOKENS) 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
     (map-set token-balances  
      { property-id: property-id, holder: tx-sender } 
      { balance: (- sender-balance amount) }) 
     
    (map-set token-balances  
      { property-id: property-id, holder: recipient } 
      { balance: (+ recipient-balance amount) }) 
     
    (ok true))) 
 
(define-public (buy-tokens (property-id uint) (amount uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND)) 
        (property-owner (get owner property-data)) 
        (token-price (get token-price property-data)) 
        (total-cost (* amount token-price)) 
        (owner-balance (get-token-balance-or-zero property-id property-owner))) 
    (asserts! (>= owner-balance amount) ERR-INSUFFICIENT-TOKENS) 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT) 
    (asserts! (get is-active property-data) ERR-PROPERTY-NOT-FOUND) 
     
    (try! (stx-transfer? total-cost tx-sender property-owner)) 
     
    (let ((current-owner-balance (get-token-balance-or-zero property-id property-owner)) 
          (buyer-balance (get-token-balance-or-zero property-id tx-sender))) 
      (map-set token-balances  
        { property-id: property-id, holder: property-owner } 
        { balance: (- current-owner-balance amount) }) 
            (map-set token-balances  
        { property-id: property-id, holder: tx-sender } 
        { balance: (+ buyer-balance amount) })) 
     
    (ok true)))
;; Bitcoin collateral management 
(define-public (add-collateral (property-id uint) (additional-collateral uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! (is-eq tx-sender (get owner property-data)) ERR-NOT-AUTHORIZED) 
    (asserts! (> additional-collateral u0) ERR-INVALID-AMOUNT) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { bitcoin-collateral: (+ (get bitcoin-collateral property-data) 
additional-collateral) })) 
    (ok true))) 
 
(define-public (remove-collateral (property-id uint) (amount uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND)) 
        (current-collateral (get bitcoin-collateral property-data)) 
        (required-collateral (/ (calculate-collateral-requirement (get property-value 
property-data)) u100))) 
    (asserts! (is-eq tx-sender (get owner property-data)) ERR-NOT-AUTHORIZED) 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT) 
    (asserts! (>= current-collateral amount) ERR-INSUFFICIENT-COLLATERAL) 
    (asserts! (>= (- current-collateral amount) required-collateral) 
ERR-INSUFFICIENT-COLLATERAL) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { bitcoin-collateral: (- current-collateral amount) })) 
    (ok true))) 
 
(define-read-only (get-collateral-ratio (property-id uint))
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (ok (/ (* (get bitcoin-collateral property-data) u100) (get property-value property-data))))) 
 
(define-read-only (is-undercollateralized (property-id uint)) 
  (let ((collateral-ratio (unwrap! (get-collateral-ratio property-id) 
ERR-PROPERTY-NOT-FOUND))) 
    (ok (< collateral-ratio MIN-COLLATERAL-RATIO)))) 
 
;; Rental income distribution mechanism 
(define-public (deposit-rental-income (property-id uint) (amount uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! (is-eq tx-sender (get owner property-data)) ERR-NOT-AUTHORIZED) 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT) 
     
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender))) 
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { rental-income-pool: (+ (get rental-income-pool property-data) 
amount) })) 
    (ok true))) 
 
(define-public (distribute-rental-income (property-id uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND)) 
        (distribution-id (var-get next-distribution-id)) 
        (rental-pool (get rental-income-pool property-data)) 
        (total-tokens (get total-tokens property-data))) 
  (asserts! (is-eq tx-sender (get owner property-data)) ERR-NOT-AUTHORIZED) 
    (asserts! (> rental-pool u0) ERR-INVALID-AMOUNT) 
    (asserts! (> total-tokens u0) ERR-RENTAL-DISTRIBUTION-FAILED) 
     
    (let ((per-token-amount (/ rental-pool total-tokens))) 
      (map-set rental-distributions 
        { property-id: property-id, distribution-id: distribution-id } 
        { 
          total-amount: rental-pool, 
          per-token-amount: per-token-amount, 
          distributed-at: block-height 
        }) 
       
      (map-set properties  
        { property-id: property-id } 
        (merge property-data { rental-income-pool: u0 })) 
       
      (var-set next-distribution-id (+ distribution-id u1)) 
      (ok distribution-id)))) 
 
(define-public (claim-rental-income (property-id uint) (distribution-id uint)) 
  (let ((distribution-data (unwrap! (map-get? rental-distributions { property-id: property-id, 
distribution-id: distribution-id }) ERR-PROPERTY-NOT-FOUND)) 
        (user-balance (get-token-balance-or-zero property-id tx-sender)) 
        (already-claimed (default-to false (get claimed (map-get? user-claimed-distributions { 
property-id: property-id, holder: tx-sender, distribution-id: distribution-id }))))) 
    (asserts! (not already-claimed) ERR-NOT-AUTHORIZED) 
    (asserts! (> user-balance u0) ERR-INSUFFICIENT-TOKENS) 
     
    (let ((claim-amount (* user-balance (get per-token-amount distribution-data)))) 
      (asserts! (> claim-amount u0) ERR-INVALID-AMOUNT)
       (map-set user-claimed-distributions 
        { property-id: property-id, holder: tx-sender, distribution-id: distribution-id } 
        { claimed: true }) 
      (ok claim-amount)))) 
      ;; Security controls and access management 
(define-public (emergency-pause-property (property-id uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get owner 
property-data))) ERR-NOT-AUTHORIZED) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { is-active: false })) 
    (ok true))) 
 
(define-public (resume-property (property-id uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get owner 
property-data))) ERR-NOT-AUTHORIZED) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { is-active: true })) 
    (ok true))) 
 
(define-public (transfer-property-ownership (property-id uint) (new-owner principal)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND)) 
        (current-owner (get owner property-data)) 
        (owner-balance (get-token-balance-or-zero property-id current-owner))) 
    (asserts! (is-eq tx-sender current-owner) ERR-NOT-AUTHORIZED)
        (map-set properties  
      { property-id: property-id } 
      (merge property-data { owner: new-owner })) 
     
    (if (> owner-balance u0) 
      (begin 
        (map-set token-balances  
          { property-id: property-id, holder: current-owner } 
          { balance: u0 }) 
        (map-set token-balances  
          { property-id: property-id, holder: new-owner } 
          { balance: owner-balance })) 
      true) 
    (ok true)))
    ;; Property valuation and liquidation functions 
(define-public (update-property-value (property-id uint) (new-value uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) 
    (asserts! (> new-value u0) ERR-INVALID-AMOUNT) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data {  
        property-value: new-value, 
        token-price: (/ new-value (get total-tokens property-data)) 
      })) 
    (ok true))) 
 
(define-public (liquidate-property (property-id uint)) 
  (let ((property-data (unwrap! (map-get? properties { property-id: property-id }) 
ERR-PROPERTY-NOT-FOUND)) 
        (is-under-collateralized (unwrap! (is-undercollateralized property-id) 
ERR-PROPERTY-NOT-FOUND))) 
    (asserts! is-under-collateralized ERR-INSUFFICIENT-COLLATERAL) 
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get owner 
property-data))) ERR-NOT-AUTHORIZED) 
     
    (map-set properties  
      { property-id: property-id } 
      (merge property-data { is-active: false })) 
    (ok true)))
    ;; Read-only functions for querying state 
(define-read-only (get-property (property-id uint)) 
  (map-get? properties { property-id: property-id })) 
 
(define-read-only (get-token-balance (property-id uint) (holder principal)) 
  (default-to u0 (get balance (map-get? token-balances { property-id: property-id, holder: 
holder })))) 
 
(define-read-only (get-rental-distribution (property-id uint) (distribution-id uint)) 
  (map-get? rental-distributions { property-id: property-id, distribution-id: distribution-id })) 
 
(define-read-only (has-claimed-distribution (property-id uint) (holder principal) (distribution-id 
uint)) 
  (default-to false (get claimed (map-get? user-claimed-distributions { property-id: property-id, 
holder: holder, distribution-id: distribution-id })))) 
 
(define-read-only (get-next-property-id) 
  (var-get next-property-id)) 
 
(define-read-only (get-next-distribution-id) 
  (var-get next-distribution-id))