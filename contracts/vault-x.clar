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