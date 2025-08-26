;; SustainableFarming Rewards Contract
;; Incentivize eco-friendly farming practices with carbon credits and premium payments

;; Define the carbon credit token
(define-fungible-token carbon-credit)

;; Owner captured at deploy time (the deployer)
(define-data-var contract-owner principal tx-sender)

;; Constants
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-credits (err u103))
(define-constant err-already-registered (err u104))

;; Data variables
(define-data-var total-carbon-credits uint u0)
(define-data-var reward-rate uint u10) ;; 10 carbon credits per sustainable practice

;; Maps
(define-map registered-farmers principal
  {
    name: (string-ascii 50),
    farm-size: uint,
    registration-block: uint,
    total-practices: uint,
    total-credits-earned: uint
  } ) (define-map sustainable-practices principal
  {
    organic-farming: bool,
    water-conservation: bool,
    renewable-energy: bool,
    soil-health: bool,
    biodiversity: bool
  }
)

;; Function 1: Register farmer and award initial carbon credits
(define-public (register-farmer (name (string-ascii 50)) (farm-size uint))
  (begin
    ;; Check if farmer is not already registered
    (asserts! (is-none (map-get? registered-farmers tx-sender)) err-already-registered)
    (asserts! (> farm-size u0) err-invalid-amount)
    (asserts! (not (is-eq name "")) err-invalid-amount)

    ;; Calculate initial credits based on farm size (1 credit per acre)
    (let ((initial-credits farm-size))
      ;; Register the farmer
      (map-set registered-farmers tx-sender {
        name: name,
        farm-size: farm-size, registration-block:stacks-block-height,
        total-practices: u0,
        total-credits-earned: initial-credits
      })

      ;; Initialize sustainable practices map
      (map-set sustainable-practices tx-sender {
        organic-farming: false,
        water-conservation: false,
        renewable-energy: false,
        soil-health: false,
        biodiversity: false
      })

      ;; Mint initial carbon credits
      (try! (ft-mint? carbon-credit initial-credits tx-sender))
      (var-set total-carbon-credits (+ (var-get total-carbon-credits) initial-credits))

      (ok {
        message: "Farmer registered successfully",
        initial-credits: initial-credits,
        farmer: tx-sender
      })
    )
  )
)

;; Function 2: Submit sustainable farming practice and earn rewards
(define-public (submit-practice (practice-type (string-ascii 20)) (premium-payment uint))
  (begin
    ;; Check if farmer is registered
    (asserts! (is-some (map-get? registered-farmers tx-sender)) err-not-registered)
    (asserts! (> premium-payment u0) err-invalid-amount)

    (let (
      (farmer-data (unwrap! (map-get? registered-farmers tx-sender) err-not-registered))
      (current-practices (unwrap! (map-get? sustainable-practices tx-sender) err-not-registered))
      (reward-credits (var-get reward-rate))
    )
      ;; Update practice based on type and award credits
      (let (
        (updated-practices
          (if (is-eq practice-type "organic")
              (merge current-practices { organic-farming: true })
              (if (is-eq practice-type "water")
                  (merge current-practices { water-conservation: true })
                  (if (is-eq practice-type "renewable")
                      (merge current-practices { renewable-energy: true })
                      (if (is-eq practice-type "soil")
                          (merge current-practices { soil-health: true })
                          (merge current-practices { biodiversity: true }))))))
      )
        ;; Update maps
        (map-set sustainable-practices tx-sender updated-practices)
        (map-set registered-farmers tx-sender
          (merge farmer-data {
            total-practices: (+ (get total-practices farmer-data) u1),
            total-credits-earned: (+ (get total-credits-earned farmer-data) reward-credits)
          }))

        ;; Transfer premium payment to contract (as STX)
        (try! (stx-transfer? premium-payment tx-sender (as-contract tx-sender)))

        ;; Mint reward carbon credits
        (try! (ft-mint? carbon-credit reward-credits tx-sender))
        (var-set total-carbon-credits (+ (var-get total-carbon-credits) reward-credits))

        (ok {
          message: "Sustainable practice submitted successfully",
          practice: practice-type,
          credits-earned: reward-credits,
          premium-received: premium-payment,
          total-credits: (+ (get total-credits-earned farmer-data) reward-credits)
        })
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-farmer-info (farmer principal))
  (map-get? registered-farmers farmer)
)

(define-read-only (get-farmer-practices (farmer principal))
  (map-get? sustainable-practices farmer)
)

(define-read-only (get-carbon-credit-balance (farmer principal))
  (ft-get-balance carbon-credit farmer)
)

(define-read-only (get-total-carbon-credits)
  (var-get total-carbon-credits)
)

(define-read-only (get-contract-stx-balance)
  (stx-get-balance (as-contract tx-sender))
)