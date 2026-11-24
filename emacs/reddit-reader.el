;;; reddit-reader.el --- mu4e-style Reddit reader -*- lexical-binding: t; -*-

;; Author: Karthik Balasubramanian
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm, news, reddit, accessibility

;;; Commentary:
;; Keyboard-driven, low-vision Reddit reader rendered like mu4e-headers.
;; Subreddits list -> posts list -> post + comment tree.  Reads Reddit's
;; public JSON endpoints on demand, caches to SQLite.  Read-only: local
;; stars, read-marks, TTS read-aloud, open-in-browser.  No auth, no daemon.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'sqlite)
(require 'browse-url)

(declare-function outline-toggle-children "outline")

(defgroup reddit-reader nil
  "Emacs-native low-vision Reddit reader."
  :group 'communication)

;;; --- Customization -------------------------------------------------------

(defcustom reddit-reader-user-agent
  "reddit-reader/1.0 (https://balasubramanian.org)"
  "User-Agent header sent with every Reddit request.
Reddit returns HTTP 429 for blank or default user agents."
  :type 'string :group 'reddit-reader)

(defcustom reddit-reader-config-file
  (expand-file-name "reddit-reader/subreddits.txt"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config")))
  "Newline-delimited list of curated subreddit names."
  :type 'file :group 'reddit-reader)

(defcustom reddit-reader-data-dir
  (expand-file-name "reddit-reader/"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name "~/.local/share")))
  "Directory holding cache.db and state.db."
  :type 'directory :group 'reddit-reader)

(defcustom reddit-reader-post-limit 50
  "Number of posts to request per subreddit listing."
  :type 'integer :group 'reddit-reader)

(defcustom reddit-reader-cache-ttl 600
  "Seconds before a cached listing is considered stale.
`R' force-refetches regardless of this value."
  :type 'integer :group 'reddit-reader)

(defcustom reddit-reader-comment-depth-cap 3
  "Maximum display indent depth for comments.
Replies deeper than this render at this level (flattened)."
  :type 'integer :group 'reddit-reader)

(defcustom reddit-reader-tts-top-comments 5
  "How many top-scored comments `L' reads aloud after the post body."
  :type 'integer :group 'reddit-reader)

(defcustom reddit-reader-tts-command "say"
  "Program used for TTS read-aloud.  Receives text on stdin."
  :type 'string :group 'reddit-reader)

;;; --- Config --------------------------------------------------------------

(defconst reddit-reader--starter-config
  "# reddit-reader: one subreddit per line (no r/ prefix needed).\n\
# Lines starting with # and blank lines are ignored.\n\
# Example: emacs\n\
# Example: orgmode\n"
  "Seed contents written when the config file is missing.")

(defun reddit-reader--load-subreddits ()
  "Return curated subreddit names from `reddit-reader-config-file'.
Create the file with a commented starter (and return nil) if absent."
  (unless (file-exists-p reddit-reader-config-file)
    (make-directory (file-name-directory reddit-reader-config-file) t)
    (with-temp-file reddit-reader-config-file
      (insert reddit-reader--starter-config)))
  (let (names)
    (with-temp-buffer
      (insert-file-contents reddit-reader-config-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (push (string-remove-prefix "r/" line) names)))
        (forward-line 1)))
    (nreverse names)))

;;; --- Net -----------------------------------------------------------------

(defun reddit-reader--listing-url (subreddit sort)
  "Public JSON URL for SUBREDDIT listing by SORT."
  (format "https://www.reddit.com/r/%s/%s.json?limit=%d&raw_json=1"
          subreddit sort reddit-reader-post-limit))

(defun reddit-reader--comments-url (subreddit post-id)
  "Public JSON URL for the comment thread of POST-ID in SUBREDDIT."
  (format "https://www.reddit.com/r/%s/comments/%s.json?raw_json=1"
          subreddit post-id))

(defun reddit-reader--response-body (buffer)
  "Return the response body string from a url-retrieve BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (re-search-forward "\r?\n\r?\n" nil t)
        (buffer-substring-no-properties (point) (point-max))
      "")))

(defun reddit-reader--fetch (url callback)
  "GET URL asynchronously with the Reddit UA; call CALLBACK with body string.
On HTTP error or network failure, call CALLBACK with nil."
  (let ((url-request-extra-headers
         (list (cons "User-Agent" reddit-reader-user-agent))))
    (url-retrieve
     url
     (lambda (status)
       (let ((buf (current-buffer)))
         (unwind-protect
             (if (plist-get status :error)
                 (funcall callback nil)
               (funcall callback (reddit-reader--response-body buf)))
           (kill-buffer buf))))
     nil t t)))

(defun reddit-reader--fetch-listing (subreddit sort callback)
  "Fetch + parse SUBREDDIT listing by SORT; CALLBACK gets (posts-or-nil)."
  (reddit-reader--fetch
   (reddit-reader--listing-url subreddit sort)
   (lambda (body)
     (funcall callback
              (when body
                (condition-case nil
                    (reddit-reader--parse-listing body subreddit)
                  (error nil)))))))

(defun reddit-reader--fetch-comments (subreddit post-id callback)
  "Fetch comments for POST-ID; CALLBACK gets (raw-json-body-or-nil)."
  (reddit-reader--fetch
   (reddit-reader--comments-url subreddit post-id) callback))

;;; --- Cache (cache.db) ----------------------------------------------------

(defvar reddit-reader--cache-conn nil "Open handle to cache.db.")
(defvar reddit-reader--state-conn nil "Open handle to state.db.")

(defun reddit-reader--open (path stmts)
  "Open SQLite db at PATH, run each string in STMTS (idempotent), return handle."
  (make-directory (file-name-directory path) t)
  (let ((db (sqlite-open path)))
    (dolist (sql stmts) (sqlite-execute db sql))
    db))

(defun reddit-reader--cache-db ()
  "Return the open cache.db handle, opening + initializing if needed."
  (unless (and reddit-reader--cache-conn (sqlitep reddit-reader--cache-conn))
    (setq reddit-reader--cache-conn
          (reddit-reader--open
           (expand-file-name "cache.db" reddit-reader-data-dir)
           (list
            "CREATE TABLE IF NOT EXISTS posts (
              id TEXT, subreddit TEXT, sort TEXT, rank INTEGER,
              title TEXT, author TEXT, score INTEGER, num_comments INTEGER,
              created INTEGER, permalink TEXT, url TEXT, self_p INTEGER,
              selftext TEXT, nsfw INTEGER, fetched_at INTEGER,
              PRIMARY KEY (id, subreddit, sort))"
            "CREATE TABLE IF NOT EXISTS comment_cache (
              post_id TEXT PRIMARY KEY, fetched_at INTEGER, json TEXT)"))))
  reddit-reader--cache-conn)

(defun reddit-reader--state-db ()
  "Return the open state.db handle, opening + initializing if needed."
  (unless (and reddit-reader--state-conn (sqlitep reddit-reader--state-conn))
    (setq reddit-reader--state-conn
          (reddit-reader--open
           (expand-file-name "state.db" reddit-reader-data-dir)
           (list
            "CREATE TABLE IF NOT EXISTS reads (
              post_id TEXT PRIMARY KEY, read_at INTEGER)"
            "CREATE TABLE IF NOT EXISTS stars (
              post_id TEXT PRIMARY KEY, subreddit TEXT, title TEXT,
              permalink TEXT, starred_at INTEGER)"))))
  reddit-reader--state-conn)

(defun reddit-reader--close-dbs ()
  "Close both DB handles (used by tests / teardown)."
  (when (and reddit-reader--cache-conn (sqlitep reddit-reader--cache-conn))
    (sqlite-close reddit-reader--cache-conn))
  (when (and reddit-reader--state-conn (sqlitep reddit-reader--state-conn))
    (sqlite-close reddit-reader--state-conn))
  (setq reddit-reader--cache-conn nil reddit-reader--state-conn nil))

(defun reddit-reader--bool->int (b) (if b 1 0))
(defun reddit-reader--int->bool (n) (and n (/= n 0)))

(defun reddit-reader--cache-put-posts (subreddit sort posts fetched-at)
  "Replace cached POSTS for SUBREDDIT+SORT, stamped FETCHED-AT.
DELETE + inserts run in one transaction so a mid-write crash can't
leave a partial cache that `reddit-reader--cache-stale-p' treats as fresh."
  (let ((db (reddit-reader--cache-db))
        (rank 0)
        (committed nil))
    (sqlite-execute db "BEGIN")
    (unwind-protect
        (progn
          (sqlite-execute db "DELETE FROM posts WHERE subreddit=? AND sort=?"
                          (list subreddit sort))
          (dolist (p posts)
            (sqlite-execute
             db "INSERT OR REPLACE INTO posts
                 (id,subreddit,sort,rank,title,author,score,num_comments,created,
                  permalink,url,self_p,selftext,nsfw,fetched_at)
                 VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
             (list (alist-get 'id p) subreddit sort rank
                   (alist-get 'title p) (alist-get 'author p)
                   (alist-get 'score p) (alist-get 'num-comments p)
                   (alist-get 'created p) (alist-get 'permalink p)
                   (alist-get 'url p)
                   (reddit-reader--bool->int (alist-get 'self-p p))
                   (alist-get 'selftext p)
                   (reddit-reader--bool->int (alist-get 'nsfw p))
                   fetched-at))
            (setq rank (1+ rank)))
          (sqlite-execute db "COMMIT")
          (setq committed t))
      (unless committed
        (sqlite-execute db "ROLLBACK")))))

(defun reddit-reader--row->post (row)
  "Map a posts table ROW (column order of the SELECT below) to a post alist."
  (cl-destructuring-bind (id title author score ncom created
                          permalink url self_p selftext nsfw subreddit) row
    `((id . ,id) (subreddit . ,subreddit) (title . ,title) (author . ,author)
      (score . ,score) (num-comments . ,ncom) (created . ,created)
      (permalink . ,permalink) (url . ,url)
      (self-p . ,(reddit-reader--int->bool self_p))
      (selftext . ,selftext) (nsfw . ,(reddit-reader--int->bool nsfw)))))

(defun reddit-reader--cache-get-posts (subreddit sort)
  "Return cached posts for SUBREDDIT+SORT in rank order (list of alists)."
  (mapcar
   #'reddit-reader--row->post
   (sqlite-select
    (reddit-reader--cache-db)
    "SELECT id,title,author,score,num_comments,created,permalink,url,
            self_p,selftext,nsfw,subreddit
       FROM posts WHERE subreddit=? AND sort=? ORDER BY rank"
    (list subreddit sort))))

(defun reddit-reader--cache-stale-p (subreddit sort now)
  "Non-nil if cached SUBREDDIT+SORT is older than the TTL at time NOW.
Absent cache counts as stale."
  (let ((row (sqlite-select
              (reddit-reader--cache-db)
              "SELECT MIN(fetched_at) FROM posts WHERE subreddit=? AND sort=?"
              (list subreddit sort))))
    (let ((ts (caar row)))
      (or (null ts) (> (- now ts) reddit-reader-cache-ttl)))))

(defun reddit-reader--cache-put-comments (post-id json fetched-at)
  "Cache raw comments JSON for POST-ID."
  (sqlite-execute
   (reddit-reader--cache-db)
   "INSERT OR REPLACE INTO comment_cache (post_id,fetched_at,json) VALUES (?,?,?)"
   (list post-id fetched-at json)))

(defun reddit-reader--cache-get-comments (post-id)
  "Return cached comments JSON string for POST-ID, or nil."
  (caar (sqlite-select
         (reddit-reader--cache-db)
         "SELECT json FROM comment_cache WHERE post_id=?" (list post-id))))

;;; --- State (state.db) ----------------------------------------------------

(defun reddit-reader--read-p (post-id)
  "Non-nil if POST-ID is marked read."
  (and (sqlite-select (reddit-reader--state-db)
                      "SELECT 1 FROM reads WHERE post_id=?" (list post-id))
       t))

(defun reddit-reader--mark-read (post-id now)
  "Mark POST-ID read at time NOW (idempotent)."
  (sqlite-execute (reddit-reader--state-db)
                  "INSERT OR REPLACE INTO reads (post_id,read_at) VALUES (?,?)"
                  (list post-id now)))

(defun reddit-reader--toggle-read (post-id now)
  "Toggle read state of POST-ID."
  (if (reddit-reader--read-p post-id)
      (sqlite-execute (reddit-reader--state-db)
                      "DELETE FROM reads WHERE post_id=?" (list post-id))
    (reddit-reader--mark-read post-id now)))

(defun reddit-reader--unread-count (_subreddit ids)
  "Number of IDS (cached post ids for a subreddit) not yet marked read."
  (seq-count (lambda (id) (not (reddit-reader--read-p id))) ids))

(defun reddit-reader--starred-p (post-id)
  "Non-nil if POST-ID is locally starred."
  (and (sqlite-select (reddit-reader--state-db)
                      "SELECT 1 FROM stars WHERE post_id=?" (list post-id))
       t))

(defun reddit-reader--star (post now)
  "Toggle local star for POST (a post alist) at time NOW."
  (let ((id (alist-get 'id post)))
    (if (reddit-reader--starred-p id)
        (sqlite-execute (reddit-reader--state-db)
                        "DELETE FROM stars WHERE post_id=?" (list id))
      (sqlite-execute
       (reddit-reader--state-db)
       "INSERT OR REPLACE INTO stars
          (post_id,subreddit,title,permalink,starred_at) VALUES (?,?,?,?,?)"
       (list id (alist-get 'subreddit post) (alist-get 'title post)
             (alist-get 'permalink post) now)))))

(defun reddit-reader--starred-posts ()
  "Return starred posts (alists with id/subreddit/title/permalink), newest first."
  (mapcar
   (lambda (row)
     (cl-destructuring-bind (id subreddit title permalink) row
       `((id . ,id) (subreddit . ,subreddit) (title . ,title)
         (permalink . ,permalink))))
   (sqlite-select (reddit-reader--state-db)
                  "SELECT post_id,subreddit,title,permalink
                     FROM stars ORDER BY starred_at DESC")))

;;; --- Parse ---------------------------------------------------------------

(defun reddit-reader--json-read (s)
  "Parse JSON string S to an alist/list tree."
  (json-parse-string s
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun reddit-reader--child->post (child subreddit)
  "Turn one t3 listing CHILD into a post alist for SUBREDDIT."
  (let ((d (alist-get 'data child)))
    `((id           . ,(alist-get 'id d))
      (subreddit     . ,(or (alist-get 'subreddit d) subreddit))
      (title         . ,(or (alist-get 'title d) ""))
      (author        . ,(or (alist-get 'author d) "[unknown]"))
      (score         . ,(or (alist-get 'score d) 0))
      (num-comments  . ,(or (alist-get 'num_comments d) 0))
      (created       . ,(truncate (or (alist-get 'created_utc d) 0)))
      (permalink     . ,(or (alist-get 'permalink d) ""))
      (url           . ,(or (alist-get 'url d) ""))
      (self-p        . ,(and (alist-get 'is_self d) t))
      (selftext      . ,(or (alist-get 'selftext d) ""))
      (nsfw          . ,(and (alist-get 'over_18 d) t)))))

(defun reddit-reader--parse-listing (json subreddit)
  "Parse a listing JSON string into a list of post alists for SUBREDDIT."
  (let* ((tree (reddit-reader--json-read json))
         (children (alist-get 'children (alist-get 'data tree))))
    (mapcar (lambda (c) (reddit-reader--child->post c subreddit)) children)))

(defun reddit-reader--comment-node->alist (data depth)
  "Build a comment alist from t1 DATA at display DEPTH."
  `((id      . ,(alist-get 'id data))
    (author  . ,(or (alist-get 'author data) "[deleted]"))
    (score   . ,(or (alist-get 'score data) 0))
    (body    . ,(or (alist-get 'body data) ""))
    (created . ,(truncate (or (alist-get 'created_utc data) 0)))
    (depth   . ,depth)))

(defun reddit-reader--flatten-children (children depth acc)
  "Pre-order flatten t1 CHILDREN at raw DEPTH onto ACC (reversed).
Display depth is clamped to `reddit-reader-comment-depth-cap'.
\"more\" nodes and the empty-string replies sentinel are skipped."
  (dolist (child children)
    (when (equal (alist-get 'kind child) "t1")
      (let* ((data (alist-get 'data child))
             (disp (min depth reddit-reader-comment-depth-cap)))
        (push (reddit-reader--comment-node->alist data disp) acc)
        (let ((replies (alist-get 'replies data)))
          ;; replies is "" (string) when none, else a listing object.
          (when (and replies (not (stringp replies)))
            (setq acc (reddit-reader--flatten-children
                       (alist-get 'children (alist-get 'data replies))
                       (1+ depth) acc)))))))
  acc)

(defun reddit-reader--parse-comments (json)
  "Parse a comments JSON string into a flat, depth-capped comment list."
  (let* ((tree (reddit-reader--json-read json))   ; list of 2 listings
         (listing (nth 1 tree))
         (children (alist-get 'children (alist-get 'data listing))))
    (nreverse (reddit-reader--flatten-children children 0 nil))))

;;; --- UI: subreddits buffer -----------------------------------------------

(defface reddit-reader-unread-face
  '((t :weight bold))
  "Face for subreddits/posts with unread items."
  :group 'reddit-reader)

(defvar-local reddit-reader--subs nil
  "Subreddit list backing the current buffer.")

(defun reddit-reader--subreddit-rows (subs)
  "Build `tabulated-list-entries' rows for SUBS (names)."
  (mapcar
   (lambda (sub)
     (let* ((ids (mapcar (lambda (p) (alist-get 'id p))
                         (reddit-reader--cache-get-posts sub "hot")))
            (unread (reddit-reader--unread-count sub ids))
            (name-cell (if (> unread 0)
                           (propertize sub 'face 'reddit-reader-unread-face)
                         sub)))
       (list sub (vector name-cell
                         (if (> unread 0) (format "%d" unread) "")))))
   subs))

(defvar reddit-reader-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'reddit-reader-open-subreddit)
    (define-key m (kbd "g")   #'reddit-reader-refresh)
    (define-key m (kbd "R")   #'reddit-reader-refresh)
    (define-key m (kbd "n")   #'next-line)
    (define-key m (kbd "p")   #'previous-line)
    (define-key m (kbd "?")   #'reddit-reader-help)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for the subreddits list buffer.")

(define-derived-mode reddit-reader-mode tabulated-list-mode "Reddit"
  "Major mode for the curated subreddits list."
  (setq tabulated-list-format [("Subreddit" 32 t) ("Unread" 8 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun reddit-reader--render-subs (subs)
  "Populate the current `reddit-reader-mode' buffer with SUBS."
  (setq reddit-reader--subs subs)
  (setq tabulated-list-entries (reddit-reader--subreddit-rows subs))
  (tabulated-list-print t))

(defun reddit-reader-open-subreddit ()
  "Open the posts list for the subreddit at point."
  (interactive)
  (let ((sub (tabulated-list-get-id)))
    (when sub (reddit-reader--show-posts sub "hot"))))

(defun reddit-reader-refresh ()
  "Reload the subreddits list (recompute unread counts)."
  (interactive)
  (reddit-reader--render-subs (reddit-reader--load-subreddits)))

;;; --- UI: posts buffer ----------------------------------------------------

(defface reddit-reader-read-face
  '((t :inherit shadow))
  "Face for already-read posts."
  :group 'reddit-reader)

(defvar-local reddit-reader--sub nil
  "Subreddit shown in this posts buffer.")

(defvar-local reddit-reader--sort "hot"
  "Active sort for this posts buffer.")

(defun reddit-reader--age (created)
  "Human age string for CREATED (unix seconds)."
  (let ((secs (- (truncate (float-time)) created)))
    (cond ((< secs 3600) (format "%dm" (max 0 (/ secs 60))))
          ((< secs 86400) (format "%dh" (/ secs 3600)))
          (t (format "%dd" (/ secs 86400))))))

(defun reddit-reader--post-rows (posts)
  "Build `tabulated-list-entries' for POSTS (entry id = the post alist)."
  (mapcar
   (lambda (p)
     (let* ((read (reddit-reader--read-p (alist-get 'id p)))
            (star (if (reddit-reader--starred-p (alist-get 'id p)) "*" " "))
            (title (concat star " " (alist-get 'title p)))
            (title (if read
                       (propertize title 'face 'reddit-reader-read-face)
                     title)))
       (list p (vector title
                       (format "%d" (alist-get 'score p))
                       (format "%d" (alist-get 'num-comments p))
                       (reddit-reader--age (alist-get 'created p))))))
   posts))

(defvar reddit-reader-posts-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'reddit-reader-open-post)
    (define-key m (kbd "s")   #'reddit-reader-star-post)
    (define-key m (kbd "u")   #'reddit-reader-toggle-read-post)
    (define-key m (kbd "h")   #'reddit-reader-sort-hot)
    (define-key m (kbd "n")   #'reddit-reader-sort-new)
    (define-key m (kbd "t")   #'reddit-reader-sort-top)
    (define-key m (kbd "g")   #'reddit-reader-posts-revert)
    (define-key m (kbd "R")   #'reddit-reader-posts-refetch)
    (define-key m (kbd "o")   #'reddit-reader-browse-permalink)
    (define-key m (kbd "b")   #'reddit-reader-browse-url)
    (define-key m (kbd "/")   #'reddit-reader-filter)
    (define-key m (kbd "?")   #'reddit-reader-help)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for the posts list buffer.")

(define-derived-mode reddit-reader-posts-mode tabulated-list-mode "Reddit-Posts"
  "Major mode for a subreddit's posts."
  (setq tabulated-list-format [("Title" 70 nil) ("Score" 7 nil)
                               ("Cmts" 6 nil) ("Age" 5 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun reddit-reader--render-posts (posts)
  "Print POSTS into the current posts buffer."
  (setq tabulated-list-entries (reddit-reader--post-rows posts))
  (tabulated-list-print t))

(defun reddit-reader--show-posts (sub sort)
  "Open/refresh the posts buffer for SUB by SORT (cache-first, async refetch)."
  (let ((buf (get-buffer-create (format "*reddit:%s*" sub))))
    (with-current-buffer buf
      (unless (derived-mode-p 'reddit-reader-posts-mode)
        (reddit-reader-posts-mode))
      (setq reddit-reader--sub sub reddit-reader--sort sort)
      (setq mode-name (format "Reddit-Posts[%s]" sort))
      (reddit-reader--render-posts (reddit-reader--cache-get-posts sub sort)))
    (switch-to-buffer buf)
    (when (reddit-reader--cache-stale-p sub sort (truncate (float-time)))
      (reddit-reader--posts-refetch-into buf sub sort))))

(defun reddit-reader--posts-refetch-into (buf sub sort)
  "Re-fetch SUB by SORT from Reddit; repaint BUF (explicit) when it lands."
  (message "reddit: fetching r/%s/%s..." sub sort)
  (reddit-reader--fetch-listing
   sub sort
   (lambda (posts)
     (if (not posts)
         (message "reddit: fetch failed for r/%s (showing cache)" sub)
       (reddit-reader--cache-put-posts sub sort posts (truncate (float-time)))
       (when (buffer-live-p buf)
         (with-current-buffer buf (reddit-reader--render-posts posts)))
       (message "reddit: r/%s/%s updated" sub sort)))))

(defun reddit-reader-posts-refetch ()
  "Re-fetch the current sort from Reddit and repaint when it lands."
  (interactive)
  (reddit-reader--posts-refetch-into
   (current-buffer) reddit-reader--sub reddit-reader--sort))

(defun reddit-reader-posts-revert ()
  "Repaint from cache (no network)."
  (interactive)
  (reddit-reader--render-posts
   (reddit-reader--cache-get-posts reddit-reader--sub reddit-reader--sort)))

(defun reddit-reader--resort (sort)
  "Switch the current posts buffer to SORT and fetch it."
  (setq reddit-reader--sort sort)
  (setq mode-name (format "Reddit-Posts[%s]" sort))
  (reddit-reader--render-posts
   (reddit-reader--cache-get-posts reddit-reader--sub sort))
  (when (reddit-reader--cache-stale-p reddit-reader--sub sort (truncate (float-time)))
    (reddit-reader-posts-refetch)))

(defun reddit-reader-sort-hot () "Sort hot." (interactive) (reddit-reader--resort "hot"))
(defun reddit-reader-sort-new () "Sort new." (interactive) (reddit-reader--resort "new"))
(defun reddit-reader-sort-top () "Sort top." (interactive) (reddit-reader--resort "top"))

(defun reddit-reader-star-post ()
  "Toggle local star on the post at point."
  (interactive)
  (let ((p (tabulated-list-get-id)))
    (when p
      (reddit-reader--star p (truncate (float-time)))
      (reddit-reader-posts-revert))))

(defun reddit-reader-toggle-read-post ()
  "Toggle read state of the post at point."
  (interactive)
  (let ((p (tabulated-list-get-id)))
    (when p
      (reddit-reader--toggle-read (alist-get 'id p) (truncate (float-time)))
      (reddit-reader-posts-revert))))

(defun reddit-reader-open-post ()
  "Open the comment view for the post at point; mark it read."
  (interactive)
  (let ((p (tabulated-list-get-id)))
    (when p
      (reddit-reader--mark-read (alist-get 'id p) (truncate (float-time)))
      (reddit-reader--show-comments p))))

;; Task 12 stubs (filter and help -- fleshed out in Task 12)
(defun reddit-reader-filter ()
  "Filter the current posts buffer (stub -- implemented in Task 12)."
  (interactive))

(defun reddit-reader-help ()
  "Show the keybinding cheatsheet (stub -- implemented in Task 12)."
  (interactive))

;;; --- UI: comments buffer -------------------------------------------------

(defface reddit-reader-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for comment author/score headers."
  :group 'reddit-reader)

(defvar-local reddit-reader--post nil
  "Post alist backing the comments buffer.")

(defun reddit-reader--rail (depth)
  "Return DEPTH copies of the indent rail glyph."
  (apply #'concat (make-list depth "│ ")))

(defun reddit-reader--comment->string (c)
  "Render comment alist C to a propertized string block."
  (let* ((rail (reddit-reader--rail (alist-get 'depth c)))
         (hdr (propertize
               (format "%s%s  %d pts  %s\n"
                       rail
                       (alist-get 'author c)
                       (alist-get 'score c)
                       (reddit-reader--age (alist-get 'created c)))
               'face 'reddit-reader-meta-face))
         (body (mapconcat (lambda (ln) (concat rail ln))
                          (split-string (string-trim (alist-get 'body c)) "\n")
                          "\n")))
    (concat hdr body "\n\n")))

(defvar reddit-reader-comments-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "L")   #'reddit-reader-read-aloud)
    (define-key m (kbd "o")   #'reddit-reader-browse-permalink)
    (define-key m (kbd "b")   #'reddit-reader-browse-url)
    (define-key m (kbd "s")   #'reddit-reader-star-current)
    (define-key m (kbd "g")   #'reddit-reader-comments-refetch)
    (define-key m (kbd "R")   #'reddit-reader-comments-refetch)
    (define-key m (kbd "TAB") #'outline-toggle-children)
    (define-key m (kbd "?")   #'reddit-reader-help)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for the post + comments buffer.")

(define-derived-mode reddit-reader-comments-mode special-mode "Reddit-Comments"
  "Major mode for a post and its comment tree."
  (setq buffer-read-only t)
  (visual-line-mode 1))

(defun reddit-reader--render-comments (post comments)
  "Render POST header + selftext, then COMMENTS, into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat (alist-get 'title post) "\n") 'face 'bold))
    (insert (propertize
             (format "r/%s  by %s  %d pts  %d comments\n\n"
                     (alist-get 'subreddit post)
                     (alist-get 'author post)
                     (alist-get 'score post)
                     (alist-get 'num-comments post))
             'face 'reddit-reader-meta-face))
    (when (and (alist-get 'self-p post)
               (not (string-empty-p (alist-get 'selftext post))))
      (insert (string-trim (alist-get 'selftext post)) "\n\n"))
    (unless (alist-get 'self-p post)
      (insert (alist-get 'url post) "\n\n"))
    (insert (make-string 40 ?-) "\n\n")
    (if comments
        (dolist (c comments) (insert (reddit-reader--comment->string c)))
      (insert "(no comments loaded)\n"))
    (goto-char (point-min))))

(defun reddit-reader--show-comments (post)
  "Open the comments buffer for POST (cache-first, async refetch)."
  (let ((buf (get-buffer-create
              (format "*reddit:%s:%s*"
                      (alist-get 'subreddit post)
                      (alist-get 'id post)))))
    (with-current-buffer buf
      (reddit-reader-comments-mode)
      (setq reddit-reader--post post)
      (let ((cached (reddit-reader--cache-get-comments (alist-get 'id post))))
        (reddit-reader--render-comments
         post (and cached (reddit-reader--parse-comments cached)))))
    (switch-to-buffer buf)
    (reddit-reader-comments-refetch)))

(defun reddit-reader-comments-refetch ()
  "Fetch the comment thread and repaint when it lands."
  (interactive)
  (let* ((post reddit-reader--post)
         (sub (alist-get 'subreddit post))
         (id (alist-get 'id post))
         (buf (current-buffer)))
    (message "reddit: fetching comments...")
    (reddit-reader--fetch-comments
     sub id
     (lambda (json)
       (if (not json)
           (message "reddit: comment fetch failed (showing cache)")
         (reddit-reader--cache-put-comments id json (truncate (float-time)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (reddit-reader--render-comments
              post (reddit-reader--parse-comments json))))
         (message "reddit: comments updated"))))))

(defun reddit-reader-browse-permalink ()
  "Open the post permalink in the browser."
  (interactive)
  (let ((p (or (and (boundp 'reddit-reader--post) reddit-reader--post)
               (tabulated-list-get-id))))
    (when p
      (browse-url (concat "https://www.reddit.com"
                          (alist-get 'permalink p))))))

(defun reddit-reader-browse-url ()
  "Open the post content/media URL in the browser."
  (interactive)
  (let ((p (or (and (boundp 'reddit-reader--post) reddit-reader--post)
               (tabulated-list-get-id))))
    (when (and p (not (string-empty-p (alist-get 'url p))))
      (browse-url (alist-get 'url p)))))

(defun reddit-reader-star-current ()
  "Star the post backing the comments buffer."
  (interactive)
  (when reddit-reader--post
    (reddit-reader--star reddit-reader--post (truncate (float-time)))
    (message "reddit: star toggled")))

(defun reddit-reader-read-aloud ()
  "Read the post title, selftext, and top comments aloud via TTS."
  (interactive)
  (unless reddit-reader--post (user-error "No post loaded"))
  (let* ((post reddit-reader--post)
         (cached (reddit-reader--cache-get-comments (alist-get 'id post)))
         (comments (and cached (reddit-reader--parse-comments cached)))
         (top (seq-take
               (seq-sort-by (lambda (c) (alist-get 'score c)) #'> comments)
               reddit-reader-tts-top-comments))
         (text (mapconcat
                #'identity
                (append
                 (list (alist-get 'title post))
                 (when (and (alist-get 'self-p post)
                            (not (string-empty-p (or (alist-get 'selftext post) ""))))
                   (list (alist-get 'selftext post)))
                 (mapcar (lambda (c)
                           (format "%s said: %s"
                                   (alist-get 'author c)
                                   (alist-get 'body c)))
                         top))
                ". ")))
    (let ((proc (make-process
                 :name "reddit-tts"
                 :command (list reddit-reader-tts-command)
                 :connection-type 'pipe
                 :noquery t)))
      (process-send-string proc text)
      (process-send-eof proc))
    (message "reddit: reading aloud (%d comments)" (length top))))

;;;###autoload
(defun reddit-reader ()
  "Open the curated subreddits list."
  (interactive)
  (let ((subs (reddit-reader--load-subreddits)))
    (unless subs
      (user-error "No subreddits yet -- added a starter at %s; edit it and re-run"
                  reddit-reader-config-file))
    (let ((buf (get-buffer-create "*reddit*")))
      (with-current-buffer buf
        (reddit-reader-mode)
        (reddit-reader--render-subs subs))
      (switch-to-buffer buf))))

(provide 'reddit-reader)
;;; reddit-reader.el ends here
