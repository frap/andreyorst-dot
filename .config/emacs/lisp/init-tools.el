;;; lisp/init-tools.el --- Emacs Tools -*- lexical-binding: t -*-

;;; Tools
;; (use-package project
;;   :straight (:type built-in)
;;   :bind-keymap ("s-p" . project-prefix-map)
;;   :bind ( :map project-prefix-map
;;           ("s" . project-save-some-buffers)
;;           ("t" . eshell)
;;           ("v" . magit)
;;           ("s-p" . project-switch-project))
;;   :bind (("C-c k" . #'project-kill-buffers)
;;          ("C-c m" . #'project-compile)
;;          ("C-x f" . #'find-file)
;;          ("C-c F" . #'project-switch-project)
;;          ("C-c R" . #'pt/recentf-in-project)
;;          ("C-c f" . #'project-find-file))
;;   :custom
;;   ;; This is one of my favorite things: you can customize
;;   ;; the options shown upon switching projects.
;;   (project-switch-commands
;;    '((project-find-file "Find file")
;;      (magit-project-status "Magit" ?g)
;;      (deadgrep "Grep" ?h)
;;      (pt/project-run-vterm "vterm" ?t)
;;      (project-dired "Dired" ?d)
;;      (pt/recentf-in-project "Recently opened" ?r)))
;;   (compilation-always-kill t)
;;   (project-vc-merge-submodules nil)
;;   (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
;;   (project-vc-extra-root-markers
;;    '("Cargo.toml" "compile_commands.json"
;;      "compile_flags.txt" "project.clj"
;;      "deps.edn" "shadow-cljs.edn" "bb.edn"))
;;   :preface
;;   (defcustom project-compilation-mode nil
;;     "Mode to run the `compile' command with."
;;     :type 'symbol
;;     :group 'project
;;     :safe #'symbolp
;;     :local t)
;;   (defun project-save-some-buffers (&optional arg)
;;     "Save some modified file-visiting buffers in the current project.

;; Optional argument ARG (interactively, prefix argument) non-nil
;; means save all with no questions."
;;     (interactive "P")
;;     (let* ((project-buffers (project-buffers (project-current)))
;;            (pred (lambda () (memq (current-buffer) project-buffers))))
;;       (funcall-interactively #'save-some-buffers arg pred)))
;;   (define-advice compilation-start
;;       (:filter-args (args) use-project-compilation-mode)
;;     (let ((cmd (car args))
;;           (mode (cadr args))
;;           (rest (cddr args)))
;;       (if (and (null mode) project-compilation-mode)
;;           (append (list cmd project-compilation-mode) rest)
;;         args)))
;;   (define-advice project-root (:filter-return (project) abbreviate-project-root)
;;     (abbreviate-file-name project))
;;   (defun project-make-predicate-buffer-in-project-p ()
;;     (let ((project-buffers (project-buffers (project-current))))
;;       (lambda () (memq (current-buffer) project-buffers))))
;;   (define-advice project-compile (:around (fn) save-project-buffers-only)
;;     "Only ask to save project-related buffers."
;;     (defvar compilation-save-buffers-predicate)
;;     (let ((compilation-save-buffers-predicate
;;            (project-make-predicate-buffer-in-project-p)))
;;       (funcall fn)))
;;   (define-advice recompile
;;       (:around (fn &optional edit-command) save-project-buffers-only)
;;     "Only ask to save project-related buffers if inside of a project."
;;     (defvar compilation-save-buffers-predicate)
;;     (let ((compilation-save-buffers-predicate
;;            (if (project-current)
;;                (project-make-predicate-buffer-in-project-p)
;;              compilation-save-buffers-predicate)))
;;       (funcall fn edit-command)))
;;   :config
;;   (defun gas/open-project nil
;;     "Get a view of the project."
;;     (interactive)
;;     (dired (project-root (project-current)))
;;     ;;(dirvish)
;;     ;;(vterm-toggle-show)
;;     (windmove-up)
;;     (windmove-up))
;;    ;;(setq project-switch-commands 'gas/open-project)
;;   (add-to-list 'project-switch-commands
;;                '(project-dired "Dired"))
;;   (add-to-list 'project-switch-commands
;;                '(project-switch-to-buffer "Switch buffer")))

(use-package projectile
 ;; :delight
  :config
  (setq projectile-project-search-path '(("~/work" . 2)  ("~/.config" . 1) ("~/dev/frap" . 3)))
  (setq ;; projectile-enable-caching nil
   projectile-sort-order 'recentf )
  (projectile-mode))

(defun pt/recentf-in-project ()
  "As `recentf', but filtering based on the current project root."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Not in a project"))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))

(use-package comint
  :straight nil
  :defer t
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil)
  (comint-input-ignoredups t))

(use-package ediff
  :straight nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package eat
  :hook (eshell-load . eat-eshell-mode))

;; (use-package esh-mode
;;   :straight nil
;;   :hook (eshell-mode . common-lisp-modes-mode)
;;   :preface
;;   (declare-function eshell-search-path "ext:esh-ext")
;;   (defun eshell-prompt ()
;;     (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
;;            (path (abbreviate-file-name default-directory))
;;            (branch (when (and (eshell-search-path "git")
;;                               (locate-dominating-file default-directory ".git"))
;;                      (concat (propertize (propertize " on " 'face '(:inherit shadow)))
;;                              (propertize (string-trim (shell-command-to-string "git branch --show-current"))
;;                                          'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
;;                                                    '(:inherit shadow)
;;                                                  '(:inherit font-lock-builtin-face))))))
;;            (container (cond
;;                        ((file-exists-p "/run/.containerenv")
;;                         (format " in %s"
;;                                 (with-temp-buffer
;;                                   (save-match-data
;;                                     (insert-file-contents "/run/.containerenv")
;;                                     (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
;;                                     (switch-to-buffer (current-buffer))
;;                                     (or (match-string-no-properties 1) "podman")))))
;;                        ((file-exists-p "/.dockerenv") " in docker")))
;;            (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
;;            (info (concat (or branch "")
;;                          (propertize (concat (or container "")
;;                                              (or ssh ""))
;;                                      'face '(:inherit shadow))))
;;            (prompt (if (= eshell-last-command-status 0)
;;                        "$"
;;                      (propertize "$" 'face '(:inherit error)))))
;;       (concat date " " path info "\n" prompt " ")))
;;   :custom
;;   (eshell-scroll-show-maximum-output nil)
;;   (eshell-prompt-function 'eshell-prompt)
;;   (eshell-banner-message ""))

;; (use-package esh-module
;;   :straight nil
;;   :after eshell
;;   :custom
;;   (eshell-modules-list
;;    (cl-remove 'eshell-term eshell-modules-list)))
;;when I do a git-pull I'd like to see what's new
(global-auto-revert-mode t)

(use-package vc-hooks
  :straight nil
  :defer t
  :custom
  (vc-follow-symlinks t))

(defun vcs-quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'.
    And don't forget to refresh version control in all buffers of
    current workspace."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (when (fboundp 'magit-mode-get-buffers)
      (mapc #'vcs--kill-buffer (magit-mode-get-buffers)))))

(defun vcs--kill-buffer (buffer)
  "Gracefully kill `magit' BUFFER.
    If any alive process is related to this BUFFER, wait for 5
    seconds before nuking BUFFER and the process. If it's dead -
    don't wait at all."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((process (get-buffer-process buffer)))
      (if (not (processp process))
          (kill-buffer buffer)
        (with-current-buffer buffer
          (if (process-live-p process)
              (run-with-timer 5 nil #'vcs--kill-buffer buffer)
            (kill-process process)
            (kill-buffer buffer)))))))

(use-package transient)

(global-set-key (kbd "C-x g") 'magit-status)
(use-package magit
  :ensure t
  :defer 1
  :bind
  ("C-c g" . magit-status)
  ("C-x g" . magit-status)
  :defines (magit-status-mode-map
            magit-revision-show-gravatars
            magit-display-buffer-function
            magit-diff-refine-hunk)
  :commands (magit-display-buffer-same-window-except-diff-v1
             magit-stage-file
             magit-unstage-file)
  :mode (("COMMIT_EDITMSG" . git-commit-mode))
  :init
  (setq-default magit-git-executable (executable-find "git"))
  :hook (git-commit-mode . flyspell-mode)
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
   :config
  (setq-default vc-follow-symlinks t)
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map
    [remap magit-mode-bury-buffer]
    #'vcs-quit)
 (setq magit-revision-show-gravatars
        '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        ;; show word-granularity on selected hunk
        magit-diff-refine-hunk t)
  (setq git-commit-summary-max-length 120)
  (setq magit-commit-show-diff nil)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-auto-more t)
  (setq magit-log-margin-show-committer-date t)
  (setq magit-revert-buffers 'silent)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-wip-after-apply-mode t)
  (setq magit-wip-after-save-mode t)
  (setq magit-wip-before-change-mode t)
  (setq transient-values
        '((magit-log:magit-log-mode "--graph" "--color" "--decorate"))))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

;;;;; gutter
(use-package git-gutter
  :ensure t
  :delight
  :when IS-GUI?
  :defer t
  :bind (("C-x P" . git-gutter:popup-hunk)
         ("M-P" . git-gutter:previous-hunk)
         ("M-N" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk))
  :hook ((prog-mode org-mode) . git-gutter-mode )
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "†") ; ✘
  (setq git-gutter:added-sign "†")
  ;; (setq git-gutter:deleted-sign "†")
  ;; (set-face-foreground 'git-gutter:added "Green")
  ;; (set-face-foreground 'git-gutter:modified "Gold")
  ;; (set-face-foreground 'git-gutter:deleted "Red")
  )
;;;;; gutter-fringe
(use-package git-gutter-fringe
  :ensure t
  :delight
  :after git-gutter
  :when IS-GUI?
  :defer t
  :init
  (require 'git-gutter-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

;; show todos
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; Git-Link
;; git-link grabs links to lines, regions, commits, or home pages.
(use-package git-link
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

;;;;; git-time
(use-package git-timemachine)

(use-package server
  :straight nil
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( :map prog-mode-map
          ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojurec-mode clojure-script-mode))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG"   . "#ee5555")
           ("FIX"   . "#0fa050")
           ("PROJ"  . "#447f44")
           ("IDEA"  . "#0fa050")
           ("INFO"  . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF"  . "#e09030")))))

(use-package compile
  :straight nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

;; (use-package clojure-compilation-mode
;;   :straight nil
;;   :no-require
;;   :preface
;;   (defun clojure-compilation--split-classpath (classpath)
;;     "Split the CLASSPATH string."
;;     (split-string classpath ":" t "[[:space:]\n]+"))
;;   (defmemo clojure-compilation--get-project-dependencies-memo
;;       (command _deps-file _mod-time)
;;     "Call COMMAND to obtain the classpath string.
;; DEPS-FILE and MOD-TIME are used for memoization."
;;     (thread-last
;;       command
;;       shell-command-to-string
;;       clojure-compilation--split-classpath
;;       (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
;;   (defun clojure-compilation--get-lein-project-dependencies (root)
;;     "Obtain classpath from lein for ROOT."
;;     (let* ((project-file (expand-file-name "project.clj" root))
;;            (mod-time (file-attribute-modification-time (file-attributes project-file))))
;;       (clojure-compilation--get-project-dependencies-memo
;;        "lein classpath" project-file mod-time)))
;;   (defun clojure-compilation--get-deps-project-dependencies (root)
;;     "Obtain classpath from deps for ROOT."
;;     (let* ((project-file (expand-file-name "deps.edn" root))
;;            (mod-time (file-attribute-modification-time (file-attributes project-file))))
;;       (clojure-compilation--get-project-dependencies-memo
;;        "clojure -Spath" project-file mod-time)))
;;   (defun clojure-compilation-get-project-dependencies (project)
;;     "Get dependencies of the given PROJECT.
;; Returns a list of all jar archives."
;;     (when (bound-and-true-p tramp-gvfs-enabled)
;;       (let ((root (project-root project)))
;;         (cond ((file-exists-p (expand-file-name "deps.edn" root))
;;                (clojure-compilation--get-deps-project-dependencies root))
;;               ((file-exists-p (expand-file-name "project.clj" root))
;;                (clojure-compilation--get-lein-project-dependencies root))))))
;;   (defvar-local clojure-compilation-project-deps nil
;;     "List of project's dependencies")
;;   (defvar-local clojure-compilation-project-deps-mod-time nil
;;     "Accumulated modification time of all project's libraries")
;;   (define-project-compilation-mode clojure-compilation
;;     (require 'tramp-gvfs)
;;     (setq-local clojure-compilation-project-deps
;;                 (clojure-compilation-get-project-dependencies
;;                  clojure-compilation-current-project))
;;     (setq-local clojure-compilation-project-deps-mod-time
;;                 (seq-reduce #'+ (mapcar (lambda (f)
;;                                           (time-to-seconds
;;                                            (file-attribute-modification-time
;;                                             (file-attributes f))))
;;                                         clojure-compilation-project-deps)
;;                             0)))
;;   (defun clojure-compilation--find-file-in-project (file)
;;     "Check if FILE is part of the currently compiled project."
;;     (if (file-name-absolute-p file)
;;         file
;;       (seq-find
;;        (lambda (s) (string-suffix-p file s))
;;        clojure-compilation-current-project-files)))
;;   (defun clojure-compilation--file-exists-jar-p (jar file)
;;     "Check if FILE is present in the JAR archive."
;;     (with-temp-buffer
;;       (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
;;         (goto-char (point-min))
;;         (save-match-data
;;           (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
;;   (defmemo clojure-compilation--find-dep-memo
;;       (file _project _deps-mod-time)
;;     "Find FILE in current project dependency list.
;; PROJECT and DEPS-MOD-TIME are used for memoizing the call."
;;     (when (not (string-empty-p file))
;;       (seq-find (lambda (d)
;;                   (clojure-compilation--file-exists-jar-p d file))
;;                 clojure-compilation-project-deps)))
;;   (defun clojure-compilation--find-dep (file)
;;     "Find FILE in current project dependency list."
;;     (clojure-compilation--find-dep-memo
;;      file
;;      clojure-compilation-current-project
;;      clojure-compilation-project-deps-mod-time))
;;   (defun clojure-compilation-filename ()
;;     "Function that gets filename from the error message.
;; If the filename comes from a dependency, try to guess the
;; dependency artifact based on the project's dependencies."
;;     (when-let ((filename (substring-no-properties (match-string 1))))
;;       (or (clojure-compilation--find-file-in-project filename)
;;           (when-let ((dep (clojure-compilation--find-dep filename)))
;;             (concat (expand-file-name dep) "/" filename)))))
;;   :config
;;   (compile-add-error-syntax
;;    'clojure-compilation 'some-warning
;;    "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
;;    :file #'clojure-compilation-filename
;;    :line 2 :level 'warn :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clj-kondo-warning
;;    "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
;;    :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clj-kondo-error
;;    "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
;;    :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'kaocha-tap
;;    "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
;;    :file #'clojure-compilation-filename
;;    :line 2 :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clojure-fail
;;    "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
;;    :file #'clojure-compilation-filename
;;    :line 2 :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clojure-reflection-warning
;;    "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
;;    :file #'clojure-compilation-filename
;;    :line 2 :col 3
;;    :level 'warn :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clojure-performance-warning
;;    "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
;;    :file #'clojure-compilation-filename
;;    :line 2 :col 3
;;    :level 'warn :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'clojure-syntax-error
;;    "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
;;    :file #'clojure-compilation-filename
;;    :line 2 :col 3)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'kaocha-unit-error
;;    "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
;;    :file #'clojure-compilation-filename
;;    :line 2 :hyperlink 1 :highlight 1)
;;   (compile-add-error-syntax
;;    'clojure-compilation 'eastwood-warning
;;    "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
;;    :file #'clojure-compilation-filename
;;    :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))

;; (use-package fennel-compilation-mode
;;   :straight nil
;;   :no-require
;;   :preface
;;   (define-project-compilation-mode fennel-compilation)
;;   :config
;;   (compile-add-error-syntax
;;    'fennel-compilation
;;    'fennel-compile-error
;;    "^Compile error in \\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\)?\\$"
;;    :file 1 :line 2 :col 3)
;;   (compile-add-error-syntax
;;    'fennel-compilation
;;    'fennel-compile-error-2
;;    "^\\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\|\\?\\)? Compile error: "
;;    :file 1 :line 2 :col 3)
;;   (compile-add-error-syntax
;;    'fennel-compilation
;;    'fennel-test-error
;;    "^not ok[[:space:]]+[0-9]+[^
;; ]+
;; #[[:space:]]+\\([^:]+\\):\\([0-9]+\\):"
;;    :file 1 :line 2 :level 'error)
;;   (compile-add-error-syntax
;;    'fennel-compilation
;;    'lua-stacktrace
;;    "\\(?:^[[:space:]]+\\([^
;; :]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
;;    :file 1 :line 2))

(provide 'init-tools)
