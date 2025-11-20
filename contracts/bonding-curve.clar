;; -------------------------------------------------------------
;; Contract: bonding-curve.clar
;; Description:
;; Linear bonding-curve token:
;;  price(i) = base + slope * i
;;  When minting n tokens starting from supply S:
;;    cost = n * base + slope * ( n * S + n*(n-1)/2 )
;;  When burning n tokens from supply S:
;;    proceeds = n * base + slope * ( n*(S-1) - n*(n-1)/2 )
;; All amounts in micro-STX (1 STX = 1_000_000 micro-STX)
;; -------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ZERO u101)
(define-constant ERR-INVALID-AMOUNT u102)
(define-constant ERR-INSUFFICIENT-PAYMENT u103)
(define-constant ERR-INSUFFICIENT-RESERVE u104)
(define-constant ERR-NO-BALANCE u105)
(define-constant ERR-ALREADY-INITIALIZED u106)

;; -------------------------
;; State
;; -------------------------
(define-data-var owner (optional principal) none)
(define-data-var base-price uint u1000000)   ;; base price per token in micro-STX (default = 1 STX)
(define-data-var slope uint u10000)          ;; incremental price per token index (micro-STX)
(define-data-var total-supply uint u0)       ;; number of tokens minted
(define-data-var stx-reserve uint u0)        ;; STX reserve in micro-STX backing the pool

(define-map balances
  { account: principal }
  { amount: uint })

;; -------------------------
;; Utilities
;; -------------------------
(define-private (get-balance (p principal))
  (default-to u0 (get amount (map-get? balances { account: p }))))

(define-private (set-balance (p principal) (amt uint))
  (map-set balances { account: p } { amount: amt }))

;; arithmetic helpers: compute n*(n-1)/2 safely
(define-private (triangular (n uint))
  (/ (* n (- n u1)) u2))

;; ---- cost to mint n tokens when current supply = S
;; cost = n*base + slope*( n*S + triangular(n) )
(define-read-only (calc-cost-to-mint (supply uint) (n uint))
  (if (<= n u0)
      u0
      (let ((b (var-get base-price))
            (m (var-get slope))
            (t (triangular n)))
        (+ (* n b) (* m (+ (* n supply) t)))
      )
  )
)

;; ---- proceeds from burning n tokens when current supply = S
;; proceeds = n*base + slope*( n*(S-1) - triangular(n) )
(define-read-only (calc-proceeds-on-burn (supply uint) (n uint))
  (if (<= n u0)
      u0
      (if (< supply n)
          u0  ;; return 0 if trying to burn more than supply
          (let ((b (var-get base-price))
                (m (var-get slope))
                (t (triangular n))
                (term (* n (- supply u1)))) ;; n*(S-1)
            (+ (* n b) (* m (- term t)))
          )
      )
  )
)

;; price of the next token (index = supply)
(define-read-only (price-next)
  (let ((s (var-get total-supply)))
    (ok (+ (var-get base-price) (* (var-get slope) s)))
  )
)

;; -------------------------
;; Initialization / Owner
;; -------------------------
(define-public (initialize (admin principal))
  (let ((current-owner (var-get owner)))
    (if (is-none current-owner)
        (begin
          (asserts! (not (is-eq admin (as-contract tx-sender))) (err ERR-INVALID-AMOUNT))
          (var-set owner (some admin))
          (ok admin)
        )
        (err ERR-ALREADY-INITIALIZED)
    )
  )
)

(define-public (set-params (new-base uint) (new-slope uint))
  (match (var-get owner)
    some-val (if (not (is-eq tx-sender some-val))
                 (err ERR-NOT-OWNER)
                 (begin
                   (asserts! (> new-base u0) (err ERR-ZERO))
                   (asserts! (> new-slope u0) (err ERR-ZERO))
                   (var-set base-price new-base)
                   (var-set slope new-slope)
                   (ok true)
                 )
             )
    (err ERR-NOT-OWNER)
  )
)

;; owner can withdraw excess reserve (reserve - min backing)
(define-public (owner-withdraw (amount uint))
  (match (var-get owner)
    o (if (not (is-eq tx-sender o))
          (err ERR-NOT-OWNER)
          (let ((supply (var-get total-supply))
                (reserve (var-get stx-reserve))
                (min-back (* supply (var-get base-price)))) ;; conservative: min backing = supply * base
            ;; require that withdraw doesn't reduce reserve below min-back (simple conservative policy)
            (if (> amount (- reserve min-back))
                (err ERR-INSUFFICIENT-RESERVE)
                (begin
                  (var-set stx-reserve (- reserve amount))
                  (as-contract (stx-transfer? amount tx-sender o))
                )
            )
          )
      )
    (err ERR-NOT-OWNER)
  )
)

;; -------------------------
;; Buy (mint n tokens)
;; Caller must send STX with call; required payment >= cost; excess refunded
;; -------------------------
(define-public (buy (n uint) (sent uint))
  (let ((sender tx-sender))
    (if (<= n u0)
        (err ERR-INVALID-AMOUNT)
        (let ((s (var-get total-supply))
              (cost (calc-cost-to-mint (var-get total-supply) n)))
          (if (< sent cost)
              (err ERR-INSUFFICIENT-PAYMENT)
              (begin
                ;; increase reserve by cost
                (var-set stx-reserve (+ (var-get stx-reserve) cost))
                ;; increase supply and user balance
                (var-set total-supply (+ s n))
                (set-balance sender (+ (get-balance sender) n))
                ;; refund excess if any
                (let ((excess (- sent cost)))
                  (if (> excess u0)
                      (match (as-contract (stx-transfer? excess tx-sender sender))
                        res (ok n)
                        err-val (err ERR-INSUFFICIENT-RESERVE)
                      )
                      (ok n)
                  )
                )
              )
          )
        )
    )
  )
)

;; -------------------------
;; Sell (burn n tokens)
;; Burns user's tokens and pays out proceeds
;; -------------------------
(define-public (sell (n uint))
  (let ((sender tx-sender))
    (if (<= n u0)
        (err ERR-INVALID-AMOUNT)
        (let ((bal (get-balance sender)))
          (if (< bal n)
              (err ERR-NO-BALANCE)
              (let ((s (var-get total-supply))
                    (proceeds (calc-proceeds-on-burn (var-get total-supply) n)))
                (if (> proceeds (var-get stx-reserve))
                    (err ERR-INSUFFICIENT-RESERVE)
                    (begin
                      ;; decrease reserve, reduce supply, reduce balance
                      (var-set stx-reserve (- (var-get stx-reserve) proceeds))
                      (var-set total-supply (- s n))
                      (set-balance sender (- bal n))
                      ;; transfer proceeds to seller
                      (match (as-contract (stx-transfer? proceeds tx-sender sender))
                        res (ok proceeds)
                        err-val (err err-val)
                      )
                    )
                )
              )
          )
        )
    )
  )
)

;; -------------------------
;; Read-only views
;; -------------------------
(define-read-only (get-total-supply) (ok (var-get total-supply)))
(define-read-only (get-stx-reserve) (ok (var-get stx-reserve)))
(define-read-only (get-balance-of (p principal)) (ok (get-balance p)))
(define-read-only (get-params) (ok { base: (var-get base-price), slope: (var-get slope) }))
