;; byteguild-core
;; 
;; This smart contract serves as the central hub for the ByteGuild platform, 
;; managing guilds within the metaverse. It handles guild creation, membership, 
;; resource allocation, governance, and reputation tracking.
;;
;; Guilds function as decentralized organizations where members can pool resources,
;; coordinate activities, and build shared virtual spaces. The contract enforces
;; governance rules, permissions, and treasury management for each guild.

;; ==================
;; Constants / Errors
;; ==================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GUILD-EXISTS (err u101))
(define-constant ERR-GUILD-DOESNT-EXIST (err u102))
(define-constant ERR-USER-ALREADY-MEMBER (err u103))
(define-constant ERR-USER-NOT-MEMBER (err u104))
(define-constant ERR-INSUFFICIENT-STAKE (err u105))
(define-constant ERR-PROPOSAL-DOESNT-EXIST (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-VOTING-CLOSED (err u108))
(define-constant ERR-INSUFFICIENT-FUNDS (err u109))
(define-constant ERR-INVALID-PERMISSION (err u110))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u111))
(define-constant ERR-INVALID-PARAMETER (err u112))

;; Permission roles
(define-constant ROLE-OWNER u100)
(define-constant ROLE-ADMIN u50)
(define-constant ROLE-MEMBER u10)
(define-constant ROLE-GUEST u1)

;; Proposal status
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-PASSED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-EXECUTED u4)

;; Membership Tiers
(define-constant TIER-FOUNDER u3)
(define-constant TIER-VETERAN u2)
(define-constant TIER-MEMBER u1)

;; Other constants
(define-constant REQUIRED-STAKE-AMOUNT u1000000) ;; Amount required to create a guild (in smallest unit)
(define-constant MIN-VOTE-DURATION u144) ;; Minimum voting period in blocks (approx 1 day)
(define-constant DEFAULT-VOTING-THRESHOLD u51) ;; Default threshold percentage for proposals to pass

;; ===============
;; Data Structures
;; ===============

;; Guild data structure
(define-map guilds
  { guild-id: uint }
  {
    name: (string-ascii 64),
    description: (string-utf8 256),
    founder: principal,
    created-at: uint,
    stake-amount: uint,
    member-count: uint,
    treasury-balance: uint,
    reputation-score: uint,
    governance-token: (optional principal),
    active-proposal-count: uint
  }
)

;; Guild membership information
(define-map guild-members
  { guild-id: uint, member: principal }
  {
    joined-at: uint,
    role: uint,
    tier: uint,
    contribution: uint,
    reputation: uint
  }
)

;; Guild proposals for governance
(define-map proposals
  { guild-id: uint, proposal-id: uint }
  {
    title: (string-ascii 64),
    description: (string-utf8 256),
    proposer: principal,
    created-at: uint,
    expires-at: uint,
    status: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    action: (string-ascii 64),
    action-data: (optional (string-utf8 256))
  }
)

;; Track member votes on proposals
(define-map proposal-votes
  { guild-id: uint, proposal-id: uint, voter: principal }
  { 
    vote: bool,
    weight: uint
  }
)

;; Guild resources/assets
(define-map guild-assets
  { guild-id: uint, asset-id: uint }
  {
    name: (string-ascii 64),
    asset-type: (string-ascii 32),
    owner: principal,
    value: uint,
    metadata: (string-utf8 256),
    transferable: bool
  }
)

;; Guild resource permissions
(define-map resource-permissions
  { guild-id: uint, asset-id: uint, role: uint }
  {
    can-use: bool,
    can-manage: bool,
    can-transfer: bool
  }
)

;; ==================
;; Private Variables
;; ==================

;; Track the total number of guilds created
(define-data-var guild-count uint u0)

;; Contract administrator
(define-data-var contract-admin principal tx-sender)

;; ==================
;; Private Functions
;; ==================

;; Check if a user is the contract administrator
(define-private (is-contract-admin)
  (is-eq tx-sender (var-get contract-admin))
)

;; Check if a user is a member of a guild
(define-private (is-guild-member (guild-id uint) (user principal))
  (is-some (map-get? guild-members { guild-id: guild-id, member: user }))
)

;; Check if a user has a specific role in a guild
(define-private (has-guild-role (guild-id uint) (user principal) (required-role uint))
  (match (map-get? guild-members { guild-id: guild-id, member: user })
    member-data (>= (get role member-data) required-role)
    false
  )
)

;; Get guild by ID with validation
(define-private (get-guild (guild-id uint))
  (match (map-get? guilds { guild-id: guild-id })
    guild-data (some guild-data)
    (begin
      (print { error: "Guild does not exist", guild-id: guild-id })
      none
    )
  )
)

;; Update treasury balance
(define-private (update-treasury (guild-id uint) (amount int))
  (match (map-get? guilds { guild-id: guild-id })
    guild-data 
      (let ((current-balance (get treasury-balance guild-data))
            (new-balance-int (+ (to-int current-balance) amount))
            (new-balance (if (< new-balance-int 0) u0 (to-uint new-balance-int))))
        (map-set guilds 
          { guild-id: guild-id }
          (merge guild-data { treasury-balance: new-balance })
        )
        (ok new-balance)
      )
    (err ERR-GUILD-DOESNT-EXIST)
  )
)

;; Calculate voting power for a member based on their tier and reputation
(define-private (calculate-voting-power (guild-id uint) (member principal))
  (match (map-get? guild-members { guild-id: guild-id, member: member })
    member-data
      (let ((tier-weight (* (get tier member-data) u10))
            (rep-weight (/ (get reputation member-data) u100)))
        (+ tier-weight rep-weight u1)) ;; Base voting power of 1 + tier + reputation bonuses
    u0 ;; Return 0 if not a member
  )
)

;; Check if a proposal has passed
(define-private (has-proposal-passed (guild-id uint) (proposal-id uint))
  (match (map-get? proposals { guild-id: guild-id, proposal-id: proposal-id })
    proposal-data
      (let ((total-votes (+ (get yes-votes proposal-data) (get no-votes proposal-data)))
            (yes-percentage (if (is-eq total-votes u0) 
                              u0
                              (/ (* (get yes-votes proposal-data) u100) total-votes))))
        (and 
          (>= yes-percentage DEFAULT-VOTING-THRESHOLD) 
          (is-eq (get status proposal-data) STATUS-ACTIVE)
          (>= block-height (get expires-at proposal-data))
        ))
    false
  )
)

;; Increment guild counter and return new ID
(define-private (get-new-guild-id)
  (let ((current-count (var-get guild-count)))
    (var-set guild-count (+ current-count u1))
    (+ current-count u1)
  )
)

;; =====================
;; Read-Only Functions
;; =====================

;; Get information about a guild
(define-read-only (get-guild-info (guild-id uint))
  (match (map-get? guilds { guild-id: guild-id })
    guild-data (ok guild-data)
    (err ERR-GUILD-DOESNT-EXIST)
  )
)

;; Get guild membership details for a user
(define-read-only (get-member-info (guild-id uint) (member principal))
  (match (map-get? guild-members { guild-id: guild-id, member: member })
    member-data (ok member-data)
    (err ERR-USER-NOT-MEMBER)
  )
)

;; Get proposal details
(define-read-only (get-proposal (guild-id uint) (proposal-id uint))
  (match (map-get? proposals { guild-id: guild-id, proposal-id: proposal-id })
    proposal-data (ok proposal-data)
    (err ERR-PROPOSAL-DOESNT-EXIST)
  )
)

;; Check if a member has voted on a proposal
(define-read-only (has-voted (guild-id uint) (proposal-id uint) (voter principal))
  (is-some (map-get? proposal-votes { guild-id: guild-id, proposal-id: proposal-id, voter: voter }))
)

;; Get information about a guild asset
(define-read-only (get-asset-info (guild-id uint) (asset-id uint))
  (match (map-get? guild-assets { guild-id: guild-id, asset-id: asset-id })
    asset-data (ok asset-data)
    (err u404)
  )
)

;; Get total number of guilds
(define-read-only (get-guild-count)
  (var-get guild-count)
)

;; Check if user can access a guild resource
(define-read-only (can-access-resource (guild-id uint) (asset-id uint) (user principal))
  (match (map-get? guild-members { guild-id: guild-id, member: user })
    member-data 
      (match (map-get? resource-permissions 
                { guild-id: guild-id, asset-id: asset-id, role: (get role member-data) })
        permissions (ok (get can-use permissions))
        (ok false))
    (err ERR-USER-NOT-MEMBER)
  )
)

;; =====================
;; Public Functions
;; =====================

;; Create a new guild
(define-public (create-guild (name (string-ascii 64)) 
                           (description (string-utf8 256))
                           (stake-amount uint))
  (let ((new-guild-id (get-new-guild-id)))
    
    ;; Verify stake amount meets minimum requirement
    (asserts! (>= stake-amount REQUIRED-STAKE-AMOUNT) ERR-INSUFFICIENT-STAKE)
    
    ;; Create guild entry
    (map-set guilds
      { guild-id: new-guild-id }
      {
        name: name,
        description: description,
        founder: tx-sender,
        created-at: block-height,
        stake-amount: stake-amount,
        member-count: u1,
        treasury-balance: u0,
        reputation-score: u0,
        governance-token: none,
        active-proposal-count: u0
      }
    )
    
    ;; Add founder as first member with owner role
    (map-set guild-members
      { guild-id: new-guild-id, member: tx-sender }
      {
        joined-at: block-height,
        role: ROLE-OWNER,
        tier: TIER-FOUNDER,
        contribution: stake-amount,
        reputation: u100
      }
    )
    
    ;; Return the new guild ID
    (ok new-guild-id)
  )
)