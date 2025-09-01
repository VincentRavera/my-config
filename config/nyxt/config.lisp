;;; Examples: https://github.com/aadcg/nyxt-config
(in-package #:nyxt-user)

;;; Tested with Nyxt 3.12.0
;;; Navigation is with vim
(define-configuration (buffer)
    ;; All Buffers should use Default Vim bindings
  ((default-modes
       (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))

   ;; Enforce M-x
   (override-map
    (let ((map (make-keymap "override-map")))
      (define-key map
          "M-x" 'execute-command)))
   ))



;; Owning back Space by removing the key in Every modes
(defvar doom-free-space-key (list
                             "space" 'nothing
                             "C-space" 'nothing
                             "C-M-space" 'nothing
                             "C-shift-space" 'nothing
                             "M-space" 'nothing))

;; The new bindings
(defvar doom-normal-key (list
                             ;; "space" 'execute-command
                             "space space" 'switch-buffer
                             ;; b for Buffer
                             "space b b" 'switch-buffer
                             "space b n" 'switch-buffer-next
                             "space b p" 'switch-buffer-previous
                             "space b r" 'reload-current-buffer
                             "space b d" 'delete-current-buffer
                             "space b D" 'delete-buffer
                             "space b k" 'delete-current-buffer
                             "space b m" 'bookmark-current-url
                             "space b N" 'new
                             ;; f for favorites / bookmarks and history
                             "space f a" 'bookmark-current-url
                             "space f l" 'list-bookmarks
                             "space f b" 'list-bookmarks
                             "space f d" 'delete-bookmark
                             "space f f" 'set-url-from-bookmark
                             "space f F" 'set-url-from-bookmark
                             "space f r" 'history-all-query
                             "space f R" 'history-tree
                             ;; h for help
                             "space h a" 'describe-any
                             "space h C" 'describe-class
                             "space h c" 'describe-command
                             "space h b b" 'describe-command    ;; != Function
                             "space h b k" 'describe-bindings
                             "space h f" 'describe-function
                             "space h h" 'describe-any
                             "space h k" 'describe-key
                             "space h m" 'describe-mode
                             "space h M" 'manual
                             "space h p" 'describe-package
                             "space h r r" 'load-config-file
                             "space h s" 'describe-slot
                             "space h t" 'tutorial
                             "space h v" 'describe-variable
                             ;; s for search
                             "space s b" 'search-buffer
                             "space s B" 'search-buffers
                             "space s o" 'doom-query-search-engine
                             ;; MISC
                             "space o r" 'repl
                             ;; Workspaces
                             ;; TODO Manage wokspace as groups of buffers as firefox
                             ;; a la https://addons.mozilla.org/en-US/firefox/addon/simple-tab-groups/
                             ;; This require a too much work, maybe some day
                             ;; would be nice to  put those in the status line (see nyxt/source/status.lisp)
                             ;; Vim Scrolls
                             "C-u" 'scroll-page-up
                             "C-d" 'scroll-page-down))

;; Forgotten vi bindings
(define-configuration expedition-mode
  ((keyscheme-map
    (define-keyscheme-map "expedition-mode" (list :import %slot-value%)
                            nyxt/keyscheme:vi-normal
                            (list
                             "J" 'expedition-previous
                             "K" 'expedition-next)))))

;; Remove it from status line
(define-configuration reduce-tracking-mode
  ((visible-in-status-p nil)))

;; MissConfigured binding for document-mode
(define-configuration document-mode
  ((keyscheme-map
    (define-keyscheme-map "document-mode" (list :import %slot-value%)
                            nyxt/keyscheme:vi-normal
                            (append
                            (list
                             "s-space" 'nothing
                             "space" 'nothing
                             "C-u" 'scroll-page-up
                             "C-d" 'scroll-page-down
                             "J" 'expedition-previous
                             "K" 'expedition-next)
                            doom-normal-key)))))

;;; Prompt uses Vanilla Emacs bindings as vim does
(define-configuration (prompt-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search Engine Prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command-global doom-query-search-engine (&key (query-in-new-buffer-p t))
  "Search prompted text using the queried search engine.
QUERY-IN-NEW-BUFFER creates a new buffer with the search results."
  (let* ((selection (ffi-buffer-copy (current-buffer)))
         (engine (prompt1 :prompt "Search engine"
                          :sources 'nyxt::search-engine-source))
         (query (prompt1 :prompt "Query"
                        :input selection
                        :sources
                        'prompter:raw-source
                        ))
         (target-buffer (if query-in-new-buffer-p
                            (make-buffer-focus)
                            (current-buffer))))
    (when engine
      (buffer-load (make-instance 'nyxt:new-url-query :query query :engine engine)
                   :buffer target-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Defined DNS Blocking
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add your Favourite DNS Blocking source
;; see nyxt:describe-class?class=%1Bnyxt/mode/blocker:blocker-mode
(defparameter additionals-blocking-url (list
                                        (make-instance 'nyxt/mode/blocker:hostlist
                                                       :url (quri:uri "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/pro.txt")
                                                       :base-path #p"hagezi-pro.txt")))

(define-mode additionals-blocker-mode (nyxt/mode/blocker:blocker-mode)
  "Blocker mode with custom hosts from *my-blocked-hosts*."
  ((nyxt/mode/blocker:hostlists additionals-blocking-url)
   (visible-in-status-p nil)))



(define-configuration (buffer)
  ((default-modes
       (pushnew 'additionals-blocker-mode %slot-value%))
   (default-modes
       (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))))
