;;; Navigation is with vim
(define-configuration (buffer)
    ;; All Buffers should use Default Vim bindings
  ((default-modes
       (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))
   (default-modes
       (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))

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
                             "space f l" 'list-bookmarks        ;; Meh, would have prefered a prompt, with fuzzy search on url/tags
                             "space f b" 'list-bookmarks
                             "space f d" 'delete-bookmark
                             "space f f" 'set-url-from-bookmark
                             "space f F" 'set-url-from-bookmark ;; TODO open in New buffer
                             "space f r" 'history-all-query
                             "space f R" 'history-tree
                             ;; h for help
                             "space h a" 'describe-any
                             "space h C" 'describe-class
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
    "Mode to traverse URLs delimited by a user specified buffer rectangle."
  ((keyscheme-map
    (define-keyscheme-map "expedition-mode" (list :import %slot-value%)
                            nyxt/keyscheme:vi-normal
                            (list
                             "J" 'expedition-previous
                             "K" 'expedition-next)))))

;; MissConfigured binding for document-mode
(define-configuration document-mode
    "Mode to interact with structured documents.
This is typically for HTML pages, but other formats could be supported too.
It does not assume being online.

Important pieces of functionality are:
- Page scrolling and zooming.
- QR code generation.
- view-source: for URLs.
- Buffer content summarization.
- Heading navigation.
- Frame selection."
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
                            doom-normal-key
                            )))))

(define-configuration base-mode
  "Note the :import part of the define-keyscheme-map.
It re-uses the other keymap (in this case, the one that was slot value before
the configuration) and merely adds/modifies it."
  ((keyscheme-map
    (define-keyscheme-map "base-mode" (list :import %slot-value%)
                            nyxt/keyscheme:default
                            doom-free-space-key    ;; Free space key for us
                            nyxt/keyscheme:cua
                            doom-free-space-key    ;; Free space key for us
                            nyxt/keyscheme:vi-normal
                            doom-normal-key))))

;;; Prompt uses Vanilla Emacs bindings as vim does
(define-configuration (prompt-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search Engine Prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun all-search-queries ()
;;   "Return the `search-queries's from the `current-buffer'.
;; If there's no buffer, create a dummy one and get search engines from there."
;;   (let* ((current-buffer (current-buffer))
;;          (buffer (or current-buffer
;;                      (make-instance 'context-buffer))))
;;     (unwind-protect
;;          (search-queries buffer)
;;       (unless current-buffer
;;         (buffer-delete buffer)))))


;; Search Query (not persisetent)
(define-class user-search-queries (prompter:source)
  ((prompter:name "Recent queries")
   ;; (prompter:constructor (all-search-queries))
   ;; (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:filter-preprocessor nil)
   )
  (:documentation "Source listing `all-search-engines' in the current buffer."))


(define-command-global doom-query-search-engine (&key (query-in-new-buffer-p t))
  "Search prompted text using the queried search engine.
QUERY-IN-NEW-BUFFER creates a new buffer with the search results."
  (let* ((selection (ffi-buffer-copy (current-buffer)))
         (engine (prompt1 :prompt "Search engine"
                          :sources 'nyxt::search-engine-source))
         (query (prompt1 :prompt "Query"
                        :input selection
                        ;; :sources 'prompter:raw-source))
                        :sources
                        ;; (make-instance 'new-url-or-search-source :name "New Query")
                        ;; (make-instance 'user-search-queries :name "Recent queries")
                        'prompter:raw-source
                        ))
         (target-buffer (if query-in-new-buffer-p
                            (make-buffer-focus)
                            (current-buffer))))
    (when engine
      (buffer-load (make-instance 'nyxt:new-url-query :query query :engine engine)
                   :buffer target-buffer))))
