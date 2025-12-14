;; VaultX Perpetual Futures Protocol - Clarity 4 Implementation
;; A decentralized perpetual futures trading platform for the Stacks blockchain
;; Enables leveraged long/short positions with automated liquidation mechanics

;; -----------------------------
;; VaultX Error Definitions
;; -----------------------------

;; VaultX access control errors
(define-constant ERR-NOT-AUTHORIZED (err u100))
;; VaultX invalid trade parameter errors
(define-constant ERR-INVALID-TRADE-PARAMS (err u101))
;; VaultX insufficient funds errors
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
;; VaultX position lookup errors
(define-constant ERR-POSITION-NOT-FOUND (err u103))
;; VaultX margin requirement errors
(define-constant ERR-INSUFFICIENT-MARGIN (err u104))

;; VaultX minimum margin ratio requirement (150%)
(define-constant VAULTX-MIN-MARGIN-RATIO u150)

;; VaultX position direction constants
(define-constant VAULTX-DIRECTION-LONG u1)
(define-constant VAULTX-DIRECTION-SHORT u2)

;; -----------------------------
;; VaultX Data Storage
;; -----------------------------

;; VaultX trader account balances tracking
(define-map vaultx-trader-accounts
  principal
  { stx-deposited: uint }
)

;; VaultX active trading positions registry
(define-map vaultx-trading-positions
  uint
  {
    trader: principal,
    direction: uint,
    contract-size: uint,
    entry-market-price: uint,
    leverage-multiplier: uint,
    margin-amount: uint,
    liquidation-trigger-price: uint,
    position-opened-time: uint,
  }
)

;; VaultX position identifier counter
(define-data-var vaultx-position-id-counter uint u0)

;; VaultX protocol administrator
(define-data-var vaultx-admin principal tx-sender)

;; VaultX market price feed (production would use oracle)
(define-data-var vaultx-market-price uint u0)

;; -----------------------------
;; VaultX Read-Only Query Functions
;; -----------------------------

;; Query VaultX trader account balance
(define-read-only (get-vaultx-trader-balance (trader principal))
  (default-to { stx-deposited: u0 } (map-get? vaultx-trader-accounts trader))
)

;; Query VaultX trading position details
(define-read-only (get-vaultx-position (position-id uint))
  (map-get? vaultx-trading-positions position-id)
)

;; Query current VaultX market price
(define-read-only (get-vaultx-market-price)
  (ok (var-get vaultx-market-price))
)

;; Calculate VaultX liquidation trigger price
(define-read-only (calculate-vaultx-liquidation-price
    (entry-price uint)
    (direction uint)
    (leverage uint)
  )
  (if (is-eq direction VAULTX-DIRECTION-LONG)
    ;; VaultX long position liquidation calculation
    (ok (/ (* entry-price (- u100 (/ u100 leverage))) u100))
    ;; VaultX short position liquidation calculation
    (ok (/ (* entry-price (+ u100 (/ u100 leverage))) u100))
  )
)

;; Get VaultX position direction as readable string (Clarity 4 feature)
(define-read-only (get-vaultx-direction-label (direction uint))
  (if (is-eq direction VAULTX-DIRECTION-LONG)
    (ok "LONG")
    (if (is-eq direction VAULTX-DIRECTION-SHORT)
      (ok "SHORT")
      (err "UNKNOWN")
    )
  )
)

;; Get VaultX position analytics including duration (Clarity 4 feature)
(define-read-only (get-vaultx-position-analytics (position-id uint))
  (match (get-vaultx-position position-id)
    position (let ((duration (- stacks-block-time (get position-opened-time position))))
      (ok {
        trader-address: (unwrap-panic (to-ascii? (get trader position))),
        direction-label: (unwrap-panic (get-vaultx-direction-label (get direction position))),
        position-duration: duration,
      })
    )
    (err "VaultX position not found")
  )
)

;; Check if VaultX position faces liquidation risk (Clarity 4 feature)
(define-read-only (is-vaultx-position-at-risk (position-id uint))
  (match (get-vaultx-position position-id)
    position (let (
        (current-market-price (var-get vaultx-market-price))
        (risk-detected (if (is-eq (get direction position) VAULTX-DIRECTION-LONG)
          (<= current-market-price
            (/ (* (get liquidation-trigger-price position) u105) u100)
          )
          (>= current-market-price
            (/ (* (get liquidation-trigger-price position) u95) u100)
          )
        ))
      )
      (ok {
        liquidation-risk: risk-detected,
        risk-status-text: (unwrap-panic (to-ascii? risk-detected)),
      })
    )
    (err "VaultX position not found")
  )
)

;; -----------------------------
;; VaultX Public Trading Functions
;; -----------------------------

;; Deposit funds into VaultX trader account
(define-public (vaultx-deposit-margin (amount uint))
  (let ((current-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender))))
    (ok (map-set vaultx-trader-accounts tx-sender { stx-deposited: (+ current-deposit amount) }))
  )
)

;; Withdraw funds from VaultX trader account
(define-public (vaultx-withdraw-margin (amount uint))
  (let ((current-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender))))
    (asserts! (>= current-deposit amount) ERR-INSUFFICIENT-FUNDS)
    (ok (map-set vaultx-trader-accounts tx-sender { stx-deposited: (- current-deposit amount) }))
  )
)

;; Open new VaultX trading position
(define-public (vaultx-open-trade
    (direction uint)
    (size uint)
    (leverage uint)
  )
  (let (
      (margin-required (/ (* size (var-get vaultx-market-price)) leverage))
      (trader-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender)))
      (new-position-id (+ (var-get vaultx-position-id-counter) u1))
      (market-entry-price (var-get vaultx-market-price))
    )
    ;; VaultX position validation checks
    (asserts!
      (or
        (is-eq direction VAULTX-DIRECTION-LONG)
        (is-eq direction VAULTX-DIRECTION-SHORT)
      )
      ERR-INVALID-TRADE-PARAMS
    )
    (asserts! (>= trader-deposit margin-required) ERR-INSUFFICIENT-MARGIN)

    ;; Calculate VaultX liquidation trigger
    (let ((liquidation-price (unwrap!
        (calculate-vaultx-liquidation-price market-entry-price direction leverage)
        ERR-INVALID-TRADE-PARAMS
      )))
      ;; Register new VaultX position
      (map-set vaultx-trading-positions new-position-id {
        trader: tx-sender,
        direction: direction,
        contract-size: size,
        entry-market-price: market-entry-price,
        leverage-multiplier: leverage,
        margin-amount: margin-required,
        liquidation-trigger-price: liquidation-price,
        position-opened-time: stacks-block-time,
      })

      ;; Update VaultX trader balance
      (map-set vaultx-trader-accounts tx-sender { stx-deposited: (- trader-deposit margin-required) })

      ;; Increment VaultX position counter
      (var-set vaultx-position-id-counter new-position-id)
      (ok new-position-id)
    )
  )
)

;; Close VaultX trading position
(define-public (vaultx-close-trade (position-id uint))
  (let ((position (unwrap! (get-vaultx-position position-id) ERR-POSITION-NOT-FOUND)))
    ;; Verify VaultX position ownership
    (asserts! (is-eq (get trader position) tx-sender) ERR-NOT-AUTHORIZED)

    ;; Calculate VaultX profit/loss
    (let (
        (profit-loss (vaultx-calculate-pnl position))
        (trader-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender)))
      )
      ;; Return VaultX margin + P&L to trader
      (map-set vaultx-trader-accounts tx-sender { stx-deposited: (+ trader-deposit (+ (get margin-amount position) profit-loss)) })

      ;; Remove VaultX position from registry
      (map-delete vaultx-trading-positions position-id)
      (ok true)
    )
  )
)

;; -----------------------------
;; VaultX Internal Functions
;; -----------------------------

;; Calculate VaultX position profit/loss
(define-private (vaultx-calculate-pnl (position {
  trader: principal,
  direction: uint,
  contract-size: uint,
  entry-market-price: uint,
  leverage-multiplier: uint,
  margin-amount: uint,
  liquidation-trigger-price: uint,
  position-opened-time: uint,
}))
  (let (
      (current-market-price (var-get vaultx-market-price))
      (price-movement (if (is-eq (get direction position) VAULTX-DIRECTION-LONG)
        (- current-market-price (get entry-market-price position))
        (- (get entry-market-price position) current-market-price)
      ))
    )
    (* price-movement (get contract-size position))
  )
)

;; -----------------------------
;; VaultX Admin Functions
;; -----------------------------

;; Update VaultX market price (oracle integration in production)
(define-public (vaultx-update-oracle-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get vaultx-admin)) ERR-NOT-AUTHORIZED)
    (var-set vaultx-market-price new-price)
    (ok true)
  )
)

;; Transfer VaultX admin privileges
(define-public (vaultx-transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get vaultx-admin)) ERR-NOT-AUTHORIZED)
    (var-set vaultx-admin new-admin)
    (ok true)
  )
)
