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
;; VaultX contract paused error
(define-constant ERR-CONTRACT-PAUSED (err u105))
;; VaultX position size limit error
(define-constant ERR-POSITION-TOO-LARGE (err u106))
;; VaultX liquidation not required error
(define-constant ERR-NOT-LIQUIDATABLE (err u107))
;; VaultX oracle price stale error
(define-constant ERR-STALE-PRICE (err u108))
;; VaultX transfer failed error
(define-constant ERR-TRANSFER-FAILED (err u109))

;; VaultX minimum margin ratio requirement (150%)
(define-constant VAULTX-MIN-MARGIN-RATIO u150)

;; VaultX position direction constants
(define-constant VAULTX-DIRECTION-LONG u1)
(define-constant VAULTX-DIRECTION-SHORT u2)

;; VaultX protocol limits
(define-constant VAULTX-MAX-LEVERAGE u10)
(define-constant VAULTX-MAX-POSITION-SIZE u1000000000000) ;; 1M STX in microstacks
(define-constant VAULTX-ORACLE-STALE-THRESHOLD u3600) ;; 1 hour in seconds
(define-constant VAULTX-LIQUIDATION-FEE-BPS u500) ;; 5% liquidation fee
(define-constant VAULTX-PROTOCOL-FEE-BPS u10) ;; 0.1% protocol fee

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
    is-liquidated: bool,
  }
)

;; VaultX position identifier counter
(define-data-var vaultx-position-id-counter uint u0)

;; VaultX protocol administrator
(define-data-var vaultx-admin principal tx-sender)

;; VaultX market price feed (production would use oracle)
(define-data-var vaultx-market-price uint u0)

;; VaultX oracle last update timestamp
(define-data-var vaultx-price-updated-at uint u0)

;; VaultX protocol paused state
(define-data-var vaultx-paused bool false)

;; VaultX protocol treasury for fees
(define-data-var vaultx-treasury principal tx-sender)

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
    (asserts! (not (var-get vaultx-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-TRADE-PARAMS)
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok (map-set vaultx-trader-accounts tx-sender { stx-deposited: (+ current-deposit amount) }))
  )
)

;; Withdraw funds from VaultX trader account
(define-public (vaultx-withdraw-margin (amount uint))
  (let ((current-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender))))
    (asserts! (not (var-get vaultx-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-TRADE-PARAMS)
    (asserts! (>= current-deposit amount) ERR-INSUFFICIENT-FUNDS)
    ;; Transfer STX from contract to user
    (try! (as-contract (stx-transfer? amount tx-sender (unwrap! (principal-of? tx-sender) ERR-TRANSFER-FAILED))))
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
      (market-entry-price (var-get vaultx-market-price))
      (margin-required (/ (* size market-entry-price) leverage))
      (protocol-fee (/ (* margin-required VAULTX-PROTOCOL-FEE-BPS) u10000))
      (trader-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender)))
      (new-position-id (+ (var-get vaultx-position-id-counter) u1))
      (price-age (- stacks-block-time (var-get vaultx-price-updated-at)))
    )
    ;; VaultX protocol safety checks
    (asserts! (not (var-get vaultx-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (<= price-age VAULTX-ORACLE-STALE-THRESHOLD) ERR-STALE-PRICE)
    (asserts! (> market-entry-price u0) ERR-INVALID-TRADE-PARAMS)
    
    ;; VaultX position validation checks
    (asserts!
      (or
        (is-eq direction VAULTX-DIRECTION-LONG)
        (is-eq direction VAULTX-DIRECTION-SHORT)
      )
      ERR-INVALID-TRADE-PARAMS
    )
    (asserts! (<= leverage VAULTX-MAX-LEVERAGE) ERR-INVALID-TRADE-PARAMS)
    (asserts! (<= size VAULTX-MAX-POSITION-SIZE) ERR-POSITION-TOO-LARGE)
    (asserts! (>= trader-deposit (+ margin-required protocol-fee)) ERR-INSUFFICIENT-MARGIN)

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
        is-liquidated: false,
      })

      ;; Update VaultX trader balance
      (map-set vaultx-trader-accounts tx-sender { stx-deposited: (- trader-deposit (+ margin-required protocol-fee)) })
      
      ;; Transfer protocol fee to treasury
      (try! (as-contract (stx-transfer? protocol-fee tx-sender (var-get vaultx-treasury))))

      ;; Increment VaultX position counter
      (var-set vaultx-position-id-counter new-position-id)
      (ok new-position-id)
    )
  )
)

;; Close VaultX trading position
(define-public (vaultx-close-trade (position-id uint))
  (let ((position (unwrap! (get-vaultx-position position-id) ERR-POSITION-NOT-FOUND)))
    ;; Verify VaultX position ownership and status
    (asserts! (is-eq (get trader position) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-liquidated position)) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get vaultx-paused)) ERR-CONTRACT-PAUSED)

    ;; Calculate VaultX profit/loss
    (let (
        (profit-loss (vaultx-calculate-pnl position))
        (trader-deposit (get stx-deposited (get-vaultx-trader-balance tx-sender)))
        (margin-amount (get margin-amount position))
        (protocol-fee (/ (* margin-amount VAULTX-PROTOCOL-FEE-BPS) u10000))
        ;; Calculate final amount (margin + PnL - fee), ensuring non-negative
        (final-amount (if (>= profit-loss (to-int u0))
          (- (+ margin-amount (to-uint profit-loss)) protocol-fee)
          (if (>= margin-amount (+ (to-uint (- profit-loss)) protocol-fee))
            (- margin-amount (+ (to-uint (- profit-loss)) protocol-fee))
            u0
          )
        ))
      )
      ;; Transfer protocol fee if trader has profit or sufficient margin
      (if (> protocol-fee u0)
        (try! (as-contract (stx-transfer? protocol-fee tx-sender (var-get vaultx-treasury))))
        true
      )
      
      ;; Return VaultX margin + P&L to trader
      (map-set vaultx-trader-accounts tx-sender { stx-deposited: (+ trader-deposit final-amount) })

      ;; Remove VaultX position from registry
      (map-delete vaultx-trading-positions position-id)
      (ok true)
    )
  )
)

;; -----------------------------
;; VaultX Internal Functions
;; -----------------------------

;; Calculate VaultX position profit/loss (returns int to handle negative values)
(define-private (vaultx-calculate-pnl (position {
  trader: principal,
  direction: uint,
  contract-size: uint,
  entry-market-price: uint,
  leverage-multiplier: uint,
  margin-amount: uint,
  liquidation-trigger-price: uint,
  position-opened-time: uint,
  is-liquidated: bool,
}))
  (let (
      (current-market-price (var-get vaultx-market-price))
      (price-movement (if (is-eq (get direction position) VAULTX-DIRECTION-LONG)
        (- (to-int current-market-price) (to-int (get entry-market-price position)))
        (- (to-int (get entry-market-price position)) (to-int current-market-price))
      ))
      (contract-size-int (to-int (get contract-size position)))
    )
    (* price-movement contract-size-int)
  )
)

;; -----------------------------
;; VaultX Admin Functions
;; -----------------------------

;; Liquidate VaultX position at risk
(define-public (vaultx-liquidate-position (position-id uint))
  (let ((position (unwrap! (get-vaultx-position position-id) ERR-POSITION-NOT-FOUND)))
    ;; Verify position not already liquidated
    (asserts! (not (get is-liquidated position)) ERR-NOT-AUTHORIZED)
    
    ;; Check if position is liquidatable
    (let (
        (current-market-price (var-get vaultx-market-price))
        (is-liquidatable (if (is-eq (get direction position) VAULTX-DIRECTION-LONG)
          (<= current-market-price (get liquidation-trigger-price position))
          (>= current-market-price (get liquidation-trigger-price position))
        ))
      )
      (asserts! is-liquidatable ERR-NOT-LIQUIDATABLE)
      
      ;; Calculate liquidation fee for liquidator
      (let (
          (liquidation-fee (/ (* (get margin-amount position) VAULTX-LIQUIDATION-FEE-BPS) u10000))
          (remaining-margin (- (get margin-amount position) liquidation-fee))
          (trader-deposit (get stx-deposited (get-vaultx-trader-balance (get trader position))))
        )
        ;; Mark position as liquidated
        (map-set vaultx-trading-positions position-id (merge position { is-liquidated: true }))
        
        ;; Transfer liquidation fee to liquidator
        (try! (as-contract (stx-transfer? liquidation-fee tx-sender tx-sender)))
        
        ;; Return remaining margin to trader
        (map-set vaultx-trader-accounts (get trader position) { stx-deposited: (+ trader-deposit remaining-margin) })
        
        (ok true)
      )
    )
  )
)

;; Update VaultX market price (oracle integration in production)
(define-public (vaultx-update-oracle-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get vaultx-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (> new-price u0) ERR-INVALID-TRADE-PARAMS)
    (var-set vaultx-market-price new-price)
    (var-set vaultx-price-updated-at stacks-block-time)
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

;; Pause VaultX protocol (emergency only)
(define-public (vaultx-set-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender (var-get vaultx-admin)) ERR-NOT-AUTHORIZED)
    (var-set vaultx-paused paused)
    (ok true)
  )
)

;; Update VaultX treasury address
(define-public (vaultx-set-treasury (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender (var-get vaultx-admin)) ERR-NOT-AUTHORIZED)
    (var-set vaultx-treasury new-treasury)
    (ok true)
  )
)

;; Get VaultX contract status
(define-read-only (get-vaultx-status)
  (ok {
    paused: (var-get vaultx-paused),
    admin: (var-get vaultx-admin),
    treasury: (var-get vaultx-treasury),
    market-price: (var-get vaultx-market-price),
    price-updated-at: (var-get vaultx-price-updated-at),
    total-positions: (var-get vaultx-position-id-counter),
  })
)
