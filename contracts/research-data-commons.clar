;; ================================================================================================
;; DECENTRALIZED RESEARCH DATA COMMONS
;; ================================================================================================
;;
;; A comprehensive platform for sharing scientific datasets with proper attribution,
;; usage tracking, data quality assessment, and researcher collaboration tools.
;;
;; This project consists of two main contracts:
;; 1. research-data-registry.clar - Core dataset management and quality assessment
;; 2. citation-tracker.clar - Citation management and usage tracking
;;
;; ================================================================================================

;; ================================================================================================
;; CONTRACT 1: RESEARCH DATA REGISTRY
;; File: contracts/research-data-registry.clar
;; ================================================================================================

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-DATASET-NOT-FOUND (err u101))
(define-constant ERR-INVALID-QUALITY-SCORE (err u102))
(define-constant ERR-DATASET-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-ACCESS-TYPE (err u104))
(define-constant ERR-ACCESS-DENIED (err u105))
(define-constant ERR-INVALID-PARAMETERS (err u106))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var dataset-counter uint u0)

;; Dataset access types
(define-constant ACCESS-PUBLIC u0)
(define-constant ACCESS-RESTRICTED u1)
(define-constant ACCESS-PRIVATE u2)

;; Quality assessment levels
(define-constant QUALITY-UNASSESSED u0)
(define-constant QUALITY-BRONZE u1)
(define-constant QUALITY-SILVER u2)
(define-constant QUALITY-GOLD u3)

;; Maps for storing dataset information
(define-map datasets
  { dataset-id: uint }
  {
    title: (string-ascii 256),
    description: (string-ascii 1024),
    creator: principal,
    ipfs-hash: (string-ascii 64),
    metadata-hash: (string-ascii 64),
    access-type: uint,
    license: (string-ascii 128),
    created-at: uint,
    updated-at: uint,
    size-bytes: uint,
    version: uint,
    quality-score: uint,
    download-count: uint,
    citation-count: uint,
    tags: (list 10 (string-ascii 32))
  }
)

;; Map for dataset collaborators
(define-map dataset-collaborators
  { dataset-id: uint, collaborator: principal }
  { role: (string-ascii 32), added-at: uint }
)

;; Map for dataset access permissions
(define-map dataset-access
  { dataset-id: uint, accessor: principal }
  { granted-at: uint, granted-by: principal, access-level: uint }
)

;; Map for quality assessments
(define-map quality-assessments
  { dataset-id: uint, assessor: principal }
  {
    score: uint,
    comments: (string-ascii 512),
    assessed-at: uint,
    criteria: {
      completeness: uint,
      accuracy: uint,
      consistency: uint,
      timeliness: uint,
      validity: uint
    }
  }
)

;; Map for dataset reviews
(define-map dataset-reviews
  { dataset-id: uint, reviewer: principal }
  {
    rating: uint,
    comment: (string-ascii 512),
    reviewed-at: uint
  }
)

;; Helper functions
(define-private (is-dataset-owner (dataset-id uint) (user principal))
  (match (map-get? datasets { dataset-id: dataset-id })
    dataset (is-eq (get creator dataset) user)
    false
  )
)

(define-private (is-dataset-collaborator (dataset-id uint) (user principal))
  (is-some (map-get? dataset-collaborators { dataset-id: dataset-id, collaborator: user }))
)

(define-private (has-dataset-access (dataset-id uint) (user principal))
  (match (map-get? datasets { dataset-id: dataset-id })
    dataset
    (or
      (is-eq (get access-type dataset) ACCESS-PUBLIC)
      (is-eq (get creator dataset) user)
      (is-dataset-collaborator dataset-id user)
      (is-some (map-get? dataset-access { dataset-id: dataset-id, accessor: user }))
    )
    false
  )
)

(define-private (calculate-overall-quality-score (completeness uint) (accuracy uint) (consistency uint) (timeliness uint) (validity uint))
  (/ (+ completeness accuracy consistency timeliness validity) u5)
)

;; Public functions

;; Register a new dataset
(define-public (register-dataset
  (title (string-ascii 256))
  (description (string-ascii 1024))
  (ipfs-hash (string-ascii 64))
  (metadata-hash (string-ascii 64))
  (access-type uint)
  (license (string-ascii 128))
  (size-bytes uint)
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (dataset-id (+ (var-get dataset-counter) u1))
      (current-height stacks-block-height)
    )
    (asserts! (<= access-type ACCESS-PRIVATE) ERR-INVALID-ACCESS-TYPE)
    (asserts! (> (len title) u0) ERR-INVALID-PARAMETERS)
    (asserts! (> (len ipfs-hash) u0) ERR-INVALID-PARAMETERS)

    (map-set datasets
      { dataset-id: dataset-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        ipfs-hash: ipfs-hash,
        metadata-hash: metadata-hash,
        access-type: access-type,
        license: license,
        created-at: current-height,
        updated-at: current-height,
        size-bytes: size-bytes,
        version: u1,
        quality-score: QUALITY-UNASSESSED,
        download-count: u0,
        citation-count: u0,
        tags: tags
      }
    )

    (var-set dataset-counter dataset-id)
    (ok dataset-id)
  )
)

;; Update dataset metadata
(define-public (update-dataset
  (dataset-id uint)
  (title (string-ascii 256))
  (description (string-ascii 1024))
  (metadata-hash (string-ascii 64))
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) ERR-DATASET-NOT-FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (or (is-dataset-owner dataset-id tx-sender) (is-dataset-collaborator dataset-id tx-sender)) ERR-NOT-AUTHORIZED)

    (map-set datasets
      { dataset-id: dataset-id }
      (merge dataset {
        title: title,
        description: description,
        metadata-hash: metadata-hash,
        updated-at: current-height,
        version: (+ (get version dataset) u1),
        tags: tags
      })
    )
    (ok true)
  )
)

;; Add collaborator to dataset
(define-public (add-collaborator (dataset-id uint) (collaborator principal) (role (string-ascii 32)))
  (begin
    (asserts! (is-dataset-owner dataset-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? datasets { dataset-id: dataset-id })) ERR-DATASET-NOT-FOUND)

    (map-set dataset-collaborators
      { dataset-id: dataset-id, collaborator: collaborator }
      { role: role, added-at: stacks-block-height }
    )
    (ok true)
  )
)

;; Grant access to restricted dataset
(define-public (grant-access (dataset-id uint) (accessor principal) (access-level uint))
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) ERR-DATASET-NOT-FOUND))
    )
    (asserts! (is-dataset-owner dataset-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get access-type dataset) ACCESS-RESTRICTED) ERR-INVALID-ACCESS-TYPE)

    (map-set dataset-access
      { dataset-id: dataset-id, accessor: accessor }
      {
        granted-at: stacks-block-height,
        granted-by: tx-sender,
        access-level: access-level
      }
    )
    (ok true)
  )
)

;; Submit quality assessment
(define-public (submit-quality-assessment
  (dataset-id uint)
  (completeness uint)
  (accuracy uint)
  (consistency uint)
  (timeliness uint)
  (validity uint)
  (comments (string-ascii 512))
)
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) ERR-DATASET-NOT-FOUND))
      (overall-score (calculate-overall-quality-score completeness accuracy consistency timeliness validity))
    )
    (asserts! (has-dataset-access dataset-id tx-sender) ERR-ACCESS-DENIED)
    (asserts! (and (<= completeness u10) (<= accuracy u10) (<= consistency u10) (<= timeliness u10) (<= validity u10)) ERR-INVALID-QUALITY-SCORE)
    (asserts! (and (>= completeness u1) (>= accuracy u1) (>= consistency u1) (>= timeliness u1) (>= validity u1)) ERR-INVALID-QUALITY-SCORE)

    (map-set quality-assessments
      { dataset-id: dataset-id, assessor: tx-sender }
      {
        score: overall-score,
        comments: comments,
        assessed-at: stacks-block-height,
        criteria: {
          completeness: completeness,
          accuracy: accuracy,
          consistency: consistency,
          timeliness: timeliness,
          validity: validity
        }
      }
    )

    ;; Update dataset quality score based on assessment
    (let
      (
        (quality-level
          (if (>= overall-score u9) QUALITY-GOLD
            (if (>= overall-score u7) QUALITY-SILVER
              (if (>= overall-score u5) QUALITY-BRONZE
                QUALITY-UNASSESSED))))
      )
      (map-set datasets
        { dataset-id: dataset-id }
        (merge dataset { quality-score: quality-level })
      )
    )

    (ok overall-score)
  )
)

;; Submit dataset review
(define-public (submit-review (dataset-id uint) (rating uint) (comment (string-ascii 512)))
  (begin
    (asserts! (is-some (map-get? datasets { dataset-id: dataset-id })) ERR-DATASET-NOT-FOUND)
    (asserts! (has-dataset-access dataset-id tx-sender) ERR-ACCESS-DENIED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-PARAMETERS)

    (map-set dataset-reviews
      { dataset-id: dataset-id, reviewer: tx-sender }
      {
        rating: rating,
        comment: comment,
        reviewed-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Increment download count
(define-public (record-download (dataset-id uint))
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) ERR-DATASET-NOT-FOUND))
    )
    (asserts! (has-dataset-access dataset-id tx-sender) ERR-ACCESS-DENIED)

    (map-set datasets
      { dataset-id: dataset-id }
      (merge dataset { download-count: (+ (get download-count dataset) u1) })
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-dataset (dataset-id uint))
  (map-get? datasets { dataset-id: dataset-id })
)

(define-read-only (get-dataset-collaborators (dataset-id uint))
  (map-get? dataset-collaborators { dataset-id: dataset-id, collaborator: tx-sender })
)

(define-read-only (get-quality-assessment (dataset-id uint) (assessor principal))
  (map-get? quality-assessments { dataset-id: dataset-id, assessor: assessor })
)

(define-read-only (get-dataset-review (dataset-id uint) (reviewer principal))
  (map-get? dataset-reviews { dataset-id: dataset-id, reviewer: reviewer })
)

(define-read-only (get-dataset-count)
  (var-get dataset-counter)
)

(define-read-only (can-access-dataset (dataset-id uint) (user principal))
  (has-dataset-access dataset-id user)
)

;; ================================================================================================
;; CONTRACT 2: CITATION TRACKER
;; File: contracts/citation-tracker.clar
;; ================================================================================================

;; Error constants
(define-constant ERR-CITATION-NOT-FOUND (err u200))
(define-constant ERR-CITATION-ALREADY-EXISTS (err u201))
(define-constant ERR-INVALID-CITATION-TYPE (err u202))
(define-constant ERR-DATASET-NOT-ACCESSIBLE (err u203))

;; Data variables
(define-data-var citation-counter uint u0)

;; Citation types
(define-constant CITATION-DIRECT u0)
(define-constant CITATION-DERIVED u1)
(define-constant CITATION-REFERENCED u2)

;; Maps for citation tracking
(define-map citations
  { citation-id: uint }
  {
    dataset-id: uint,
    citing-work-title: (string-ascii 256),
    citing-work-authors: (string-ascii 512),
    citing-work-doi: (optional (string-ascii 128)),
    citing-work-url: (optional (string-ascii 256)),
    citation-type: uint,
    cited-by: principal,
    cited-at: uint,
    context: (string-ascii 512),
    page-numbers: (optional (string-ascii 32))
  }
)

;; Map for tracking dataset citations
(define-map dataset-citations
  { dataset-id: uint }
  {
    total-citations: uint,
    direct-citations: uint,
    derived-citations: uint,
    reference-citations: uint,
    h-index: uint,
    last-updated: uint
  }
)

;; Map for researcher citation profiles
(define-map researcher-profiles
  { researcher: principal }
  {
    total-datasets: uint,
    total-citations-received: uint,
    total-citations-made: uint,
    h-index: uint,
    most-cited-dataset: (optional uint),
    profile-created: uint,
    last-active: uint
  }
)

;; Map for citation acknowledgments
(define-map citation-acknowledgments
  { citation-id: uint, acknowledger: principal }
  { acknowledged-at: uint, verified: bool }
)

;; Helper functions
(define-private (is-valid-citation-type (citation-type uint))
  (<= citation-type CITATION-REFERENCED)
)

(define-private (update-dataset-citation-stats (dataset-id uint) (citation-type uint))
  (let
    (
      (current-stats (default-to
        {
          total-citations: u0,
          direct-citations: u0,
          derived-citations: u0,
          reference-citations: u0,
          h-index: u0,
          last-updated: stacks-block-height
        }
        (map-get? dataset-citations { dataset-id: dataset-id })
      ))
      (new-total (+ (get total-citations current-stats) u1))
      (new-direct (if (is-eq citation-type CITATION-DIRECT) (+ (get direct-citations current-stats) u1) (get direct-citations current-stats)))
      (new-derived (if (is-eq citation-type CITATION-DERIVED) (+ (get derived-citations current-stats) u1) (get derived-citations current-stats)))
      (new-reference (if (is-eq citation-type CITATION-REFERENCED) (+ (get reference-citations current-stats) u1) (get reference-citations current-stats)))
    )
    (map-set dataset-citations
      { dataset-id: dataset-id }
      {
        total-citations: new-total,
        direct-citations: new-direct,
        derived-citations: new-derived,
        reference-citations: new-reference,
        h-index: (get h-index current-stats), ;; H-index calculation would need more complex logic
        last-updated: stacks-block-height
      }
    )
  )
)

(define-private (update-researcher-profile (researcher principal) (is-citing bool))
  (let
    (
      (current-profile (default-to
        {
          total-datasets: u0,
          total-citations-received: u0,
          total-citations-made: u0,
          h-index: u0,
          most-cited-dataset: none,
          profile-created: stacks-block-height,
          last-active: stacks-block-height
        }
        (map-get? researcher-profiles { researcher: researcher })
      ))
    )
    (map-set researcher-profiles
      { researcher: researcher }
      (merge current-profile {
        total-citations-made: (if is-citing (+ (get total-citations-made current-profile) u1) (get total-citations-made current-profile)),
        last-active: stacks-block-height
      })
    )
  )
)

;; Public functions

;; Record a citation
(define-public (record-citation
  (dataset-id uint)
  (citing-work-title (string-ascii 256))
  (citing-work-authors (string-ascii 512))
  (citing-work-doi (optional (string-ascii 128)))
  (citing-work-url (optional (string-ascii 256)))
  (citation-type uint)
  (context (string-ascii 512))
  (page-numbers (optional (string-ascii 32)))
)
  (let
    (
      (citation-id (+ (var-get citation-counter) u1))
      (current-height stacks-block-height)
    )
    ;; Validate inputs
    (asserts! (is-valid-citation-type citation-type) ERR-INVALID-CITATION-TYPE)
    (asserts! (> (len citing-work-title) u0) ERR-INVALID-PARAMETERS)
    (asserts! (> (len citing-work-authors) u0) ERR-INVALID-PARAMETERS)

    ;; Check if dataset exists and is accessible (this would need to call the other contract)
    ;; For now, we'll assume the dataset exists if dataset-id > 0
    (asserts! (> dataset-id u0) ERR-DATASET-NOT-ACCESSIBLE)

    ;; Record the citation
    (map-set citations
      { citation-id: citation-id }
      {
        dataset-id: dataset-id,
        citing-work-title: citing-work-title,
        citing-work-authors: citing-work-authors,
        citing-work-doi: citing-work-doi,
        citing-work-url: citing-work-url,
        citation-type: citation-type,
        cited-by: tx-sender,
        cited-at: current-height,
        context: context,
        page-numbers: page-numbers
      }
    )

    ;; Update statistics
    (update-dataset-citation-stats dataset-id citation-type)
    (update-researcher-profile tx-sender true)

    (var-set citation-counter citation-id)
    (ok citation-id)
  )
)

;; Acknowledge a citation (for dataset owners)
(define-public (acknowledge-citation (citation-id uint) (verified bool))
  (let
    (
      (citation (unwrap! (map-get? citations { citation-id: citation-id }) ERR-CITATION-NOT-FOUND))
    )
    ;; In a real implementation, we'd verify the acknowledger is the dataset owner
    ;; This would require cross-contract calls to the research-data-registry

    (map-set citation-acknowledgments
      { citation-id: citation-id, acknowledger: tx-sender }
      { acknowledged-at: stacks-block-height, verified: verified }
    )
    (ok true)
  )
)

;; Update citation context or details
(define-public (update-citation
  (citation-id uint)
  (context (string-ascii 512))
  (page-numbers (optional (string-ascii 32)))
)
  (let
    (
      (citation (unwrap! (map-get? citations { citation-id: citation-id }) ERR-CITATION-NOT-FOUND))
    )
    (asserts! (is-eq (get cited-by citation) tx-sender) ERR-NOT-AUTHORIZED)

    (map-set citations
      { citation-id: citation-id }
      (merge citation {
        context: context,
        page-numbers: page-numbers
      })
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-citation (citation-id uint))
  (map-get? citations { citation-id: citation-id })
)

(define-read-only (get-dataset-citation-stats (dataset-id uint))
  (map-get? dataset-citations { dataset-id: dataset-id })
)

(define-read-only (get-researcher-profile (researcher principal))
  (map-get? researcher-profiles { researcher: researcher })
)

(define-read-only (get-citation-acknowledgment (citation-id uint) (acknowledger principal))
  (map-get? citation-acknowledgments { citation-id: citation-id, acknowledger: acknowledger })
)

(define-read-only (get-total-citations)
  (var-get citation-counter)
)

(define-read-only (is-citation-acknowledged (citation-id uint))
  (is-some (map-get? citation-acknowledgments { citation-id: citation-id, acknowledger: tx-sender }))
)
