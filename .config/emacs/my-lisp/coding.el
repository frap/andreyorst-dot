;;; my-lisp/coding.el --- Emacs Coding -*- lexical-binding: t -*-

;;; Navigation & Editing
(use-package common-lisp-modes
  :straight  nil ;;(:host gitlab :repo "andreyorst/common-lisp-modes.el" :files (:defaults "*.el"))
  ;;  :delight common-lisp-modes-mode
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
;;;###autoload
  (define-minor-mode common-lisp-modes-mode
    "Mode for enabling all modes that are common for lisps.

For the reference, this is not a common-lisp modes mode, but a
common lisp-modes mode.

\\<common-lisp-modes-mode-map>"
    :lighter " clmm"
    :keymap (make-sparse-keymap))

;;;###autoload
  (define-minor-mode common-repl-modes-mode
    "Mode for enabling all modes that are common for REPLs.

 \\<common-repl-modes-mode-map>"
    :lighter " crmm"
    :keymap (make-sparse-keymap))

  (provide 'common-lisp-modes)

  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

;;; Coding helpers

(defun comment-pretty ()
  "Comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
(insert comment-start))
(insert " ")
(dotimes (_ space-on-each-side) (insert comment-char))
(when (> comment-length 0) (insert " "))
(insert comment)
(when (> comment-length 0) (insert " "))
(dotimes (_ (if (= (% comment-length 2) 0)
                (- space-on-each-side 1)
              space-on-each-side))
  (insert comment-char))))))

;; accept completion from copilot and fallback to company

(use-package copilot
  :straight
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  ;;:vc (copilot :url "https://github.com/zerolfx/copilot.el.git"
  ;; :repo zerolfx/copilot.el
  ;; :url "https://github.com/zerolfx/copilot.el.git"
  ;;:files ("dist" "*.el")
  ;;:lisp-dir "elpa/"
:bind ( :map copilot-completion-map
        (("M-<return>" . copilot-accept-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)
         ("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)))
 :hook (prog-mode . copilot-mode)
 :hook (yaml-mode . copilot-mode)
 :config (setq copilot-max-char -1)
 :ensure t)

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))


(use-package load-env-vars
  :hook ((clojure-mode . @-set-project-env)
         (lsp-mode     . @-set-project-env)
         (cider-mode   . @-set-project-env))
  :config
  (defvar @-dotenv-file-name ".env"
  "The name of the .env file."
  )
  (defun @-find-env-file ()
    "Find the closest .env file in the directory hierarchy."

    (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
           (file-name (concat env-file-directory @-dotenv-file-name)))
      (when (file-exists-p file-name)
        file-name)))
  (defun @-set-project-env ()
  "Export all environment variables in the closest .env file."

  (let ((env-file (@-find-env-file)))
    (when env-file
      (load-env-vars env-file))))
  )

(use-package multiple-cursors
  :ensure t
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines)))

(use-package multiple-cursors-core
  :straight nil
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode nxml-mode) . puni-mode)
         (puni-mode . electric-pair-mode))
  ;; paredit-like keys
  :bind ( :map puni-mode-map
          ("C-M-f" . puni-forward-sexp-or-up-list)
          ("C-M-b" . puni-backward-sexp-or-up-list)
          ("C-M-t" . puni-transpose)
          ;; slurping & barfing
          ("C-<left>" . puni-barf-forward)
          ("C-}" . puni-barf-forward)
          ("C-<right>" . puni-slurp-forward)
          ("C-)" . puni-slurp-forward)
          ("C-(" . puni-slurp-backward)
          ("C-M-<left>" . puni-slurp-backward)
          ("C-{" . puni-barf-backward)
          ("C-M-<right>" . puni-barf-backward)
          ;; depth chaining
          ("M-r" . puni-raise)
          ("M-s" . puni-splice)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-?" . puni-convolute)
          ("M-S" . puni-split)
          :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle))
  :preface
  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation)))))

(use-package puni
  :when window-system
  :bind ( :map puni-mode-map
          ;; doesn't work in terminal
          ("M-[" . puni-wrap-square)))

;; ────────────────────────────── Prettify Symbols ─────────────────────────────
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (remove-hook 'web-mode 'prettify-symbols-mode)

;; Make some word or string show as pretty Unicode symbols.  See `https://unicodelookup.com' for more.
(setq-default prettify-symbols-alist
              '(("<-" . ?←)
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ;; ("/=" . ?≠)
                ;; ("!=" . ?≠)
                ;; ("==" . ?≡)
                ;; ("<=" . ?≤)
                ;; (">=" . ?≥)
                ("=<<" . (?= (Br . Bl) ?≪))
                (">>=" . (?≫ (Br . Bl) ?=))
                ("<=<" . ?↢)
                (">=>" . ?↣)
                ("lambda" . 955)
                ("delta" . 120517)
                ("epsilon" . 120518)
                ("<" . 10216)
                (">" . 10217)
                ;; ("[" . 10214)
                ;; ("]" . 10215)
                ("<<" . 10218)
                (">>" . 10219)
                ))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package profiler
  :bind ("<f2>" . profiler-start-or-report)
  :commands (profiler-report)
  :preface
  (defun profiler-start-or-report ()
    (interactive)
    (if (not (profiler-cpu-running-p))
        (profiler-start 'cpu)
      (profiler-report)
      (profiler-cpu-stop))))

;;;;; rainbow
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((prog-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :delight t
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          sly-mprepl-mode
          lisp-interaction-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package region-bindings
   :straight
  (:host gitlab :repo "andreyorst/region-bindings.el")
;;  :vc (:url "https://gitlab.com/andreyorst/region-bindings.el.git")
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

;;; Languages

(use-package cc-mode
  :hook (c-mode-common . cc-mode-setup)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq-local comment-start "//"
                comment-end ""
                tab-width 4)))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package csv-mode
  :ensure t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package elisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

;; (use-package fennel-mode
;;     :straight
;;   (:url "https://git.sr.ht/~technomancy/fennel-mode" )
;;  ;; :vc (:url c"https://git.sr.ht/~technomancy/fennel-mode" :branch "main" :rev :newest)
;;   :hook ((fennel-mode . fennel-proto-repl-minor-mode)
;;          ((fennel-mode
;;            fennel-repl-mode
;;            fennel-proto-repl-mode)
;;           . common-lisp-modes-mode))
;;   :bind ( :map fennel-mode-map
;;           ("M-." . xref-find-definitions)
;;           ("M-," . xref-go-back)
;;           :map fennel-repl-mode-map
;;           ("C-c C-o" . fennel-repl-delete-all-output))
;;   :custom
;;   (fennel-eldoc-fontify-markdown t)
;;   (fennel-scratch-use-proto-repl t)
;;   :preface
;;   (defun fennel-repl-delete-all-output ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char (process-mark (get-buffer-process (current-buffer))))
;;       (forward-line 0)
;;       (let ((inhibit-read-only t))
;;         (delete-region (point) (point-min)))))
;;   :config
;;   (dolist (sym '(global local var set))
;;     (put sym 'fennel-indent-function 1)))

;; (use-package ob-fennel
;;   :straight nil
;;   :after org)

(use-package isayt
  :straight (:host gitlab :repo "andreyorst/isayt.el")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ( :map markdown-mode-map
          ("M-Q" . split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))

;; (use-package racket-mode
;;   :ensure t
;;   :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
;;   ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))


(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4))

;;(use-package ob-lua :after org)

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
  :bind ( :map clojure-mode-map
          ("C-:" . nil))
  :preface
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "project.clj" project-dir))
                  (executable-find "lein"))
             (setq-local compile-command "lein "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode 1)
    (clojure-set-compile-command))
  :config
  ;; (setq clojure-toplevel-inside-comment-form t
  ;;       ;; Because of CIDER's insistence to send forms to all linked REPLs, we
  ;;       ;; *have* to be able to switch cljc buffer to clj/cljs mode without
  ;;       ;; cider complaining.
  ;;       clojure-verify-major-mode nil)
  )

(use-package cider
  :ensure t
  :delight " CIDER"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval cider-find-and-clear-repl-output)
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          :map cider-mode-map
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
  :custom
  (nrepl-log-messages nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (nrepl-hide-special-buffers t)
  (cider-test-show-report-on-success t)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators nil)
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-show-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-enrich-classpath t)
  (cider-repl-history-file (expand-file-name "~/.config/cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-auto-inspect-after-eval nil)
  (cider-comment-continued-prefix "")
  (cider-comment-prefix "")
  :config
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  (defun cider-find-and-clear-repl-buffer ()
    "Find the current REPL buffer and clear it.
See `cider-find-and-clear-repl-output' for more info."
    (interactive)
    (cider-find-and-clear-repl-output 'clear-repl))
  (defun cider-open-portal ()
    (interactive)
    (cider-nrepl-request:eval
     "(do
        (ns dev)
        (def portal ((requiring-resolve 'portal.api/open) {:launcher :emacs}))
        (add-tap (requiring-resolve 'portal.api/submit)))"
     #'ignore))
  (defun cider-clear-portal ()
    (interactive)
    (cider-nrepl-request:eval "(portal.api/clear)" #'ignore))
  (defun cider-close-portal ()
    (interactive)
    (cider-nrepl-request:eval "(portal.api/close)" #'ignore))
  ;; Show emacs-lisp eval results in an overlay, CIDER style.
  ;; https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
  ;; We rely on CIDER to do the heavy lifting, can't seem to find a general library
  ;; for doing this style of overlays.
  (defun corgi/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
      :where point
      :duration 'command)
    ;; Preserve the return value.
    value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (corgi/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (corgi/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (corgi/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point))))))

;; (use-package clj-ns-name
;;   :vc (:url "https://github.com/corgi-emacs/clj-ns-name.git")
;;   :config
;;   (clj-ns-name-install))

;;(use-package walkclj
;;  :ensure t
;;  :vc (:url "https://github.com/corgi-emacs/walkclj.git"))

;; Most annoying JVM "feature" of all time
;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
(defun corgi/around-cider-jack-in-global-options (command project-type)
  (if (eq 'clojure-cli project-type)
      (concat cider-clojure-cli-global-options
              " -J-XX:-OmitStackTraceInFastThrow")
    (funcall command project-type)))

(advice-add #'cider-jack-in-global-options :around #'corgi/around-cider-jack-in-global-options)

(defun corgi/cider-pprint-eval-register (register)
  "Evaluate a Clojure snippet stored in a register.

Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer.

You can use {{...}} to insert emacs-lisp code that will get
evaluated, like `(println \"{{buffer-file-name}}\")'.
"
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (s)
                (eval
                 (read
                  (match-string 1 s))))
              (get-register register))))
    (cond
     ((string-match-p "^#_cljs" reg)
      (with-current-buffer (car (cider-repls 'cljs))
        (cider--pprint-eval-form reg)))
     ((string-match-p "^#_clj" reg)
      (with-current-buffer (car (cider-repls 'clj))
        (cider--pprint-eval-form reg)))
     (t
      (cider--pprint-eval-form reg)))))

(defun corgi/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*"))))))))

;; Create a *scratch-clj* buffer for evaluating ad-hoc Clojure expressions. If
;; you make sure there's always a babashka REPL connection then this is a cheap
;; way to always have a place to type in some quick Clojure expression evals.
(with-current-buffer (get-buffer-create "*scratch-clj*")
  (clojure-mode))

(use-package ob-clojure
  :straight nil
  :after (cider org)
  :custom
  (org-babel-clojure-backend 'cider))

(use-package clj-refactor
  :ensure t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :ensure t
  :hook (cider-mode . clj-decompiler-setup))

;; (use-package flymake
;;   :preface
;;   (defvar flymake-prefix-map (make-sparse-keymap))
;;   (fset 'flymake-prefix-map flymake-prefix-map)
;;   :bind ( :map ctl-x-map
;;           ("!" . flymake-prefix-map)
;;           :map flymake-prefix-map
;;           ("l" . flymake-show-buffer-diagnostics)
;;           ("n" . flymake-goto-next-error)
;;           ("p" . flymake-goto-prev-error))
;;   :custom
;;   (flymake-fringe-indicator-position 'right-fringe)
;;   (flymake-mode-line-lighter "FlyM")
;;   :config
;;   (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

;; (use-package package-lint-flymake
;;   :ensure t
;;   :defer t)

(use-package lisp-mode
  :straight nil
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package inf-lisp
  :straight nil
  :hook (inferior-lisp-mode . common-lisp-modes-mode)
  :bind ( :map common-lisp-modes-mode-map
          ("C-M-k" . lisp-eval-each-sexp))
  :commands (lisp-eval-last-sexp)
  :custom
  (inferior-lisp-program
   (cond ((executable-find "sbcl") "sbcl")
         ((executable-find "ecl") "ecl")))
  :config
  (defun lisp-eval-each-sexp ()
    "Evaluate each s-expression in the buffer consequentially."
    (interactive)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (save-excursion
                 (search-forward-regexp "[^[:space:]]." nil t))
          (forward-sexp)
          (when (and (not (nth 4 (syntax-ppss)))
                     (looking-back "." 1))
            (lisp-eval-last-sexp)))))))

(use-package json-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
          ("C-c C-f" . json-mode-beautify)))

(use-package restclient-jq
  :ensure t)

(use-package sly
  :ensure t
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :commands (sly-symbol-completion-mode)
  :config
  (sly-symbol-completion-mode -1))

(use-package scheme
  :straight nil
  :hook (scheme-mode . common-lisp-modes-mode))

(use-package sql-indent
  :ensure t)

(use-package terraform-mode
  :custom (terraform-format-on-save t)
  :ensure t
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package yasnippet
  :ensure t
  :defer t
    :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-reload-all
             yas-dropdown-prompt
             yas--all-templates
             yas--get-snippet-tables
             yas--template-key)
    :delight yas-minor-mode
      :hook ((text-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on)
         (conf-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
      :config
  (setq yas-prompt-functions (delq #'yas-dropdown-prompt
                                   yas-prompt-functions)
  ;; yas-snippet-dirs '(file-templates-dir)
        ))

(use-package yasnippet-classic-snippets
 :ensure t
 :demand t)

(use-package consult-yasnippet
 :ensure t
 :after consult
 :config (global-set-key (kbd "M-Y") 'consult-yasnippet))

(use-package yasnippet-capf
 :ensure t
 :after cape
 :init
 (setq yasnippet-capf-lookup-by 'key) ;; key or name
 :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))
;;; LSP

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-modeline-diagnostics-enable t)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-signature-doc-lines 1)
  :init
  (setq lsp-use-plists t))

(use-package lsp-ui
  :ensure t
  :demand t
  :commands lsp-ui-mode)

(use-package lsp-completion
  :straight nil
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-clojure
  :straight nil
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1)))
  :config
  (setq lsp-file-watch-threshold 10000
        lsp-signature-auto-activate nil
        ;; I use clj-kondo from master
        lsp-diagnostics-provider :none
        lsp-enable-indentation nil ;; uncomment to use cider indentation instead of lsp
        ))

(use-package lsp-clojure
  :straight nil
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

;;treesitter

;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; install treesitter lnaguages
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))


(use-package treesit
  :straight nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
     :straight (:host github :repo "mickeynp/combobulate")
     :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (json-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    ;;:load-path ("path-to-git-checkout-of-combobulate")
    ))

(provide 'coding)
