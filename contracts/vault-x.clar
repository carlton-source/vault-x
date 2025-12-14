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