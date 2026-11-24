;;; test-reddit-reader.el --- ERT for reddit-reader -*- lexical-binding: t; -*-
(require 'ert)
(require 'reddit-reader)

(defvar reddit-reader-test--fixtures-dir
  (expand-file-name
   "../fixtures/"
   (file-name-directory (or load-file-name buffer-file-name))))

(defun reddit-reader-test--read-fixture (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name reddit-reader-test--fixtures-dir))
    (buffer-string)))

(defun reddit-reader-test--read-fixture-abs (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(ert-deftest reddit-reader-test/package-loads ()
  "Smoke test -- package loads and entry command is defined."
  (should (fboundp 'reddit-reader)))

;;; --- Task 2: Parse listing -----------------------------------------------

(ert-deftest reddit-reader-test/parse-listing ()
  "Listing JSON -> list of post alists with required fields."
  (let* ((json (reddit-reader-test--read-fixture "listing-hot.json"))
         (posts (reddit-reader--parse-listing json "emacs")))
    (should (>= (length posts) 1))
    (let ((p (car posts)))
      (should (stringp (alist-get 'id p)))
      (should (equal (alist-get 'subreddit p) "emacs"))
      (should (stringp (alist-get 'title p)))
      (should (numberp (alist-get 'score p)))
      (should (numberp (alist-get 'num-comments p)))
      (should (numberp (alist-get 'created p)))
      (should (stringp (alist-get 'permalink p)))
      (should (memq (alist-get 'self-p p) '(t nil))))))

;;; --- Task 3: Parse comments ----------------------------------------------

(ert-deftest reddit-reader-test/parse-comments-flatten ()
  "Comments JSON -> flat depth-tagged list; depth never exceeds the cap."
  (let* ((reddit-reader-comment-depth-cap 3)
         (json (reddit-reader-test--read-fixture "comments-thread.json"))
         (comments (reddit-reader--parse-comments json)))
    (should (>= (length comments) 1))
    (dolist (c comments)
      (should (stringp (alist-get 'author c)))
      (should (stringp (alist-get 'body c)))
      (should (integerp (alist-get 'depth c)))
      (should (<= (alist-get 'depth c) 3))
      (should (>= (alist-get 'depth c) 0)))))

(ert-deftest reddit-reader-test/parse-comments-order-is-preorder ()
  "Flatten is pre-order: a reply appears after its parent."
  (let* ((reddit-reader-comment-depth-cap 3)
         (json (reddit-reader-test--read-fixture "comments-thread.json"))
         (comments (reddit-reader--parse-comments json))
         (depths (mapcar (lambda (c) (alist-get 'depth c)) comments)))
    ;; first comment is a top-level (depth 0)
    (should (equal (car depths) 0))))

(ert-deftest reddit-reader-test/parse-comments-depth-cap-clamps ()
  "A raw-depth-4 reply is clamped to the cap (3), never rendered at 4.
The fixture has a chain c1(0)->c2(1)->c3(2)->c4(3)->c5(raw 4)."
  (let* ((reddit-reader-comment-depth-cap 3)
         (json (reddit-reader-test--read-fixture "comments-thread.json"))
         (comments (reddit-reader--parse-comments json))
         (depths (mapcar (lambda (c) (alist-get 'depth c)) comments)))
    ;; the deepest rendered depth must equal the cap exactly, not exceed it
    (should (= (apply #'max depths) 3))
    ;; the raw-depth-4 node (by id c5) is clamped to 3
    (let ((deepest (seq-find (lambda (c) (equal (alist-get 'id c) "c5"))
                             comments)))
      (should deepest)
      (should (= (alist-get 'depth deepest) 3)))))

;;; --- Task 4: Config loader -----------------------------------------------

(ert-deftest reddit-reader-test/config-parse ()
  "Parser strips comments/blanks and an optional r/ prefix."
  (let ((tmp (make-temp-file "rr-subs" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "# my subs\n\nemacs\n r/orgmode \nReddit\n\n# end\n"))
          (let ((reddit-reader-config-file tmp))
            (should (equal (reddit-reader--load-subreddits)
                           '("emacs" "orgmode" "Reddit")))))
      (delete-file tmp))))

(ert-deftest reddit-reader-test/config-autocreate ()
  "Missing config file is created with a commented starter and returns nil."
  (let* ((dir (make-temp-file "rr-cfg" t))
         (tmp (expand-file-name "subreddits.txt" dir)))
    (unwind-protect
        (let ((reddit-reader-config-file tmp))
          (should (equal (reddit-reader--load-subreddits) nil))
          (should (file-exists-p tmp))
          (should (string-match-p "^#" (reddit-reader-test--read-fixture-abs tmp))))
      (delete-directory dir t))))

;;; --- Task 5: DB schema + connection helpers ------------------------------

(ert-deftest reddit-reader-test/db-init-creates-tables ()
  "Opening the DBs creates the expected tables."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir)))
    (unwind-protect
        (let ((cdb (reddit-reader--cache-db))
              (sdb (reddit-reader--state-db)))
          (should (member "posts"
                          (mapcar #'car
                                  (sqlite-select
                                   cdb "SELECT name FROM sqlite_master WHERE type='table'"))))
          (should (member "comment_cache"
                          (mapcar #'car
                                  (sqlite-select
                                   cdb "SELECT name FROM sqlite_master WHERE type='table'"))))
          (should (member "reads"
                          (mapcar #'car
                                  (sqlite-select
                                   sdb "SELECT name FROM sqlite_master WHERE type='table'"))))
          (should (member "stars"
                          (mapcar #'car
                                  (sqlite-select
                                   sdb "SELECT name FROM sqlite_master WHERE type='table'")))))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

;;; --- Task 6: Cache layer -------------------------------------------------

(ert-deftest reddit-reader-test/cache-posts-roundtrip ()
  "Posts written for (sub,sort) read back in rank order; refetch replaces."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir))
         (posts (list '((id . "a") (subreddit . "emacs") (title . "A")
                        (author . "x") (score . 9) (num-comments . 1)
                        (created . 100) (permalink . "/a") (url . "http://a")
                        (self-p . t) (selftext . "sa") (nsfw . nil))
                      '((id . "b") (subreddit . "emacs") (title . "B")
                        (author . "y") (score . 4) (num-comments . 0)
                        (created . 200) (permalink . "/b") (url . "http://b")
                        (self-p . nil) (selftext . "") (nsfw . nil)))))
    (unwind-protect
        (progn
          (reddit-reader--cache-put-posts "emacs" "hot" posts 12345)
          (let ((got (reddit-reader--cache-get-posts "emacs" "hot")))
            (should (equal (mapcar (lambda (p) (alist-get 'id p)) got) '("a" "b")))
            (should (equal (alist-get 'title (car got)) "A"))
            (should (eq (alist-get 'self-p (car got)) t)))
          ;; refetch with a single post replaces the prior two
          (reddit-reader--cache-put-posts
           "emacs" "hot"
           (list '((id . "c") (subreddit . "emacs") (title . "C") (author . "z")
                   (score . 1) (num-comments . 0) (created . 300) (permalink . "/c")
                   (url . "http://c") (self-p . nil) (selftext . "") (nsfw . nil)))
           99999)
          (should (equal (mapcar (lambda (p) (alist-get 'id p))
                                 (reddit-reader--cache-get-posts "emacs" "hot"))
                         '("c"))))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

(ert-deftest reddit-reader-test/cache-staleness ()
  "Stale-p compares fetched_at against the TTL."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir))
         (reddit-reader-cache-ttl 600))
    (unwind-protect
        (progn
          (reddit-reader--cache-put-posts "emacs" "hot"
            (list '((id . "a") (subreddit . "emacs") (title . "A") (author . "x")
                    (score . 1) (num-comments . 0) (created . 1) (permalink . "/a")
                    (url . "") (self-p . t) (selftext . "") (nsfw . nil)))
            1000)
          (should (reddit-reader--cache-stale-p "emacs" "hot" 2000)) ; 1000s old > 600
          (should-not (reddit-reader--cache-stale-p "emacs" "hot" 1300))) ; 300s old
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

(ert-deftest reddit-reader-test/comment-blob-roundtrip ()
  "Comment JSON blob writes and reads back verbatim."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (reddit-reader--cache-put-comments "abc" "[{\"k\":1}]" 42)
          (should (equal (reddit-reader--cache-get-comments "abc") "[{\"k\":1}]")))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

;;; --- Task 7: State layer -------------------------------------------------

(ert-deftest reddit-reader-test/state-read-and-star ()
  "Read-marks and stars persist; unread count reflects them."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir))
         (posts (list '((id . "a") (subreddit . "emacs") (title . "A")
                        (permalink . "/a"))
                      '((id . "b") (subreddit . "emacs") (title . "B")
                        (permalink . "/b")))))
    (unwind-protect
        (progn
          ;; nothing read yet -> unread = 2 of the 2 cached ids
          (should (= (reddit-reader--unread-count "emacs" '("a" "b")) 2))
          (reddit-reader--mark-read "a" 111)
          (should (reddit-reader--read-p "a"))
          (should-not (reddit-reader--read-p "b"))
          (should (= (reddit-reader--unread-count "emacs" '("a" "b")) 1))
          ;; toggle b read then unread
          (reddit-reader--toggle-read "b" 222)
          (should (reddit-reader--read-p "b"))
          (reddit-reader--toggle-read "b" 333)
          (should-not (reddit-reader--read-p "b"))
          ;; star a then list it
          (reddit-reader--star (car posts) 444)
          (should (reddit-reader--starred-p "a"))
          (should (equal (mapcar (lambda (s) (alist-get 'id s))
                                 (reddit-reader--starred-posts))
                         '("a")))
          (reddit-reader--star (car posts) 555) ; toggling off
          (should-not (reddit-reader--starred-p "a")))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

;;; --- Task 9: Subreddits buffer -------------------------------------------

(ert-deftest reddit-reader-test/subreddit-rows ()
  "Row builder yields one tabulated entry per subreddit with an unread cell."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (reddit-reader--cache-put-posts "emacs" "hot"
            (list '((id . "a") (subreddit . "emacs") (title . "A") (author . "x")
                    (score . 1) (num-comments . 0) (created . 1) (permalink . "/a")
                    (url . "") (self-p . t) (selftext . "") (nsfw . nil)))
            1000)
          (let ((rows (reddit-reader--subreddit-rows '("emacs" "orgmode"))))
            (should (= (length rows) 2))
            ;; entry id is the subreddit name; vector has name + unread cells
            (should (equal (car (nth 0 rows)) "emacs"))
            (should (vectorp (cadr (nth 0 rows))))))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

;;; --- Task 10: Posts buffer -----------------------------------------------

(ert-deftest reddit-reader-test/post-rows-dim-read ()
  "Post rows expose id + a vector; read posts carry the read face on the title."
  (let* ((dir (make-temp-file "rr-data" t))
         (reddit-reader-data-dir (file-name-as-directory dir))
         (posts (list '((id . "a") (subreddit . "emacs") (title . "Alpha")
                        (author . "x") (score . 9) (num-comments . 2)
                        (created . 1716950000) (permalink . "/a") (url . "")
                        (self-p . t) (selftext . "") (nsfw . nil)))))
    (unwind-protect
        (progn
          (reddit-reader--mark-read "a" 1)
          (let* ((rows (reddit-reader--post-rows posts))
                 (cell (aref (cadr (car rows)) 0)))
            (should (equal (car (car rows))
                           (car posts)))          ; entry id is the post alist
            (should (eq (get-text-property 0 'face cell) 'reddit-reader-read-face))))
      (reddit-reader--close-dbs)
      (delete-directory dir t))))

;;; --- Task 11: Comments buffer --------------------------------------------

(ert-deftest reddit-reader-test/comment-line-indent ()
  "A comment renders with a rail per display depth and its author/score."
  (let ((c '((id . "x") (author . "alice") (score . 7) (body . "hello")
             (depth . 2) (created . 1716950000))))
    (let ((s (reddit-reader--comment->string c)))
      (should (string-match-p "alice" s))
      (should (string-match-p "7" s))
      (should (string-match-p "hello" s))
      ;; two rail glyphs for depth 2
      (should (string-match-p "│ │ " s)))))

;;; --- Task 8: Net layer ---------------------------------------------------

(ert-deftest reddit-reader-test/listing-url ()
  "URL builder includes sort, limit, and raw_json."
  (let ((reddit-reader-post-limit 25))
    (should (equal (reddit-reader--listing-url "emacs" "new")
                   "https://www.reddit.com/r/emacs/new.json?limit=25&raw_json=1"))))

(ert-deftest reddit-reader-test/comments-url ()
  (should (equal (reddit-reader--comments-url "emacs" "abc123")
                 "https://www.reddit.com/r/emacs/comments/abc123.json?raw_json=1")))

(ert-deftest reddit-reader-test/response-body ()
  "Handler returns the body after the blank header line."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"ok\":1}")
    (should (equal (reddit-reader--response-body (current-buffer)) "{\"ok\":1}"))))
