;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Red Elvis
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 29.1+ configuration.

;;; Code:

;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))
            ;; doom normal emacs-startup-hook
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)
            ;; extra minibuffer
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

(defun gas/display-startup-time ()
  (message "Emacs chargé dans %s avec %d ramasse-miettes."
           (format "%.2f secondes"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gas/display-startup-time)

;;CONSTANTS
(defconst IS-GUI?     (display-graphic-p))
(defconst IS-MAC?     (eq system-type 'darwin))
(defconst IS-LINIX?   (eq system-type 'gnu/linux))

;; emacsclient --no-wait--alternate-editor=emacs [FILE]
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Generic packages
(require 'package)
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-quickstart nil)
;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

;; Configure Package Manager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initialising twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; use-package
;; Install use-package if not installed
(eval-and-compile
  (unless (and (fboundp 'package-installed-p)
               (package-installed-p 'use-package))
    (package-refresh-contents) ; update archives
    (package-install 'use-package)) ; grab the newest use-package
  (if init-file-debug
      (setq use-package-compute-statistics t)
    (setq use-package-compute-statistics nil))
  (require 'use-package))

;; Configure use-package
(use-package use-package
  :custom
  (use-package-verbose t)
;;  (use-package-always-ensure t)  ; :ensure t by default
  (use-package-always-defer nil) ; :defer t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

;; ─────────────────── Additional Packages and Configurations ──────────────────
;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

(use-package delight
  :doc "A feature that removes certain minor-modes from mode-line.
"
  :delight)
(delight '((abbrev-mode " Abv" abbrev)
           (auto-fill-function " AF")
           (visual-line-mode)
           (smart-tab-mode " \\t" smart-tab)
           (eldoc-mode nil "eldoc")
           (rainbow-mode)
           (overwrite-mode " Ov" t)
           (emacs-lisp-mode "Ɛlisp" :major)))

;;; PATH
;; Add Lisp directory to `load-path'.
;; Add our custom lisp modules to the Emacs load path so they can be discovered.
(push (expand-file-name "my-lisp/" (file-name-directory user-init-file)) load-path)

;;; TODO - delete this?
(defun update-aws-envs (fn)
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(AWS.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
      (shell-command-to-string "source $HOME/.config/shell/interactive ; printf $PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

(use-package local-config
  :no-require
  :preface
  (defgroup local-config ()
    "Customisation group for local settings."
    :prefix "local-config-"
    :group 'emacs)
  (defcustom local-config-dark-theme 'modus-vivendi
    "Dark theme to use."
    :tag "Dark theme"
    :type 'symbol
    :group 'local-config)
  (defcustom local-config-light-theme 'modus-operandi
    "Light theme to use."
    :tag "Light theme"
    :type 'symbol
    :group 'local-config)
  (defcustom no-hscroll-modes '(term-mode)
    "Major modes to disable horizontal scrolling."
    :tag "Modes to disable horizontal scrolling"
    :type '(repeat symbol)
    :group 'local-config)
  (provide 'local-config))

(use-package functions
  :no-require
  :preface
  (require 'subr-x)
  (defun split-pararagraph-into-lines ()
    "Split the current paragraph into lines with one sentence each."
    (interactive)
    (save-excursion
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))
      (let ((auto-fill-p auto-fill-function)
            (end (progn (end-of-line) (backward-sentence) (point))))
        (back-to-indentation)
        (unless (= (point) end)
          (auto-fill-mode -1)
          (while (< (point) end)
            (forward-sentence)
            (delete-horizontal-space)
            (newline-and-indent))
          (deactivate-mark)
          (when auto-fill-p
            (auto-fill-mode t))
          (when (looking-at "^$")
            (delete-char -1))))))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun termux-color-theme-dark-p ()
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "~/.termux/theme-variant"))
      (looking-at-p "dark")))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (cond ((in-termux-p)
           (termux-color-theme-dark-p))
          ((featurep 'dbus)
           (dbus-color-theme-dark-p))
          (t nil)))
  (defun memoize (fn)
    "Create a storage for FN's args.
Checks if FN was called with set args before.  If so, return the
value from the storage and don't call FN.  Otherwise calls FN,
and saves its result in the storage.  FN must be referentially
transparent."
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        ;; `memo' is used as a singleton to check for absense of value
        (let ((value (gethash args memo memo)))
          (if (eq value memo)
              (puthash args (apply fn args) memo)
            value)))))
  (defmacro defmemo (name &rest funtail)
    (declare (doc-string 3) (indent 2) (debug defun))
    `(defalias ',name (memoize (lambda ,@funtail))))
  (defvar-local ssh-tunnel-port nil)
  (put 'ssh-tunnel-port 'safe-local-variable #'numberp)
  (defun ssh-tunnel (host port &optional local-port)
    "Start an SSH tunnel from localhost to HOST:PORT.
If LOCAL-PORT is nil, PORT is used as local port."
    (interactive (list (read-string "host: " nil 'ssh-host-history)
                       (read-number "port: " ssh-tunnel-port 'ssh-port-history)
                       (when current-prefix-arg
                         (read-number "local port: " ssh-tunnel-port 'ssh-port-history))))
    (let ((name (if (and local-port (not (= local-port port)))
                    (format "*ssh-tunnel:%s:%s:%s" local-port host port)
                  (format "*ssh-tunnel:%s:%s" host port))))
      (async-shell-command
       (format "ssh -4 -N -L %s:localhost:%s %s" (or local-port port) port host)
       (concat " " name))))
  (provide 'functions))

(use-package defaults
  :no-require
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format "Ɛmacs"
   auto-window-vscroll nil
   mouse-highlight t
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system)
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore ;turn off the bell noise
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (equal system-type 'darwin)
    ;; Configure mac modifiers to be what I expect
    (with-no-warnings
      (setq  ns-command-modifier 'super
             ns-option-modifier 'meta
             ns-right-option-modifier 'nil
             ns-right-command-modifier 'nil)))
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))


;;; Core packages

(use-package kmacro
  :defer t
  :preface
  (defun block-undo (fn &rest args)
    (let ((marker (prepare-change-group)))
      (unwind-protect (apply fn args)
        (undo-amalgamate-change-group marker))))
  :config
  (dolist (f '(kmacro-call-macro
               kmacro-exec-ring-item
               apply-macro-to-region-lines))
    (advice-add f :around #'block-undo)))

(use-package subr
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))


;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p)
;;   :after (marginalia dired)
;;   :init
;;   (add-hook 'marginalia-mode #'all-the-icons-completion-marginalia-setup)
;;   (add-hook 'dired-mode #'all-the-icons-dired-mode)
;;   :config
;;   (use-package all-the-icons-dired
;;     :ensure t)
;;   (use-package all-the-icons-completion
;;     :ensure t)
;;   :config
;;   (all-the-icons-completion-mode 1))


(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))


(use-package rect
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :preface
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

(use-package page
  :bind ( :map narrow-map
          ("]" . narrow-forward-page)
          ("[" . narrow-backward-page))
  :preface
  (defun narrow-forward-page (&optional count)
    (interactive "p")
    (or count (setq count 1))
    (widen)
    (forward-page count)
    (narrow-to-page))
  (defun narrow-backward-page (&optional count)
    (interactive "p")
    (or count (setq count 1))
    (widen)
    (forward-page (- (1+ count))) ; 1+ needed to actually cross page boundary
    (narrow-to-page)))

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

;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :delight hs-minor-mode
;;   :commands (hs-hide-block)
;;   :bind ( :map hs-minor-mode-map
;;           ("C-c @ C-p" . hs-hide-all-private))
;;   :preface
;;   (defvar hs-mode-private-regex-alist
;;     `(((emacs-lisp-mode lisp-mode)
;;        . ,(rx bol "(def" (+ (not space)) (+ space) (+ (not space)) "--"))
;;       ((clojure-mode clojurescrip-mode clojurec-mode)
;;        . ,(rx "(" (or "defn-"
;;                       (seq "def" (* (not space)) (+ space)
;;                            "^" (or ":private"
;;                                    (seq "{" (* (not "}")) ":private" (+ space) "true")))
;;                       "comment")))
;;       (zig-mode
;;        . ,(rx bol (* space) "fn" (+ (not "{")) "{"))
;;       (fennel-mode
;;        . ,(rx bol "(" (or (seq (or "fn" "local" "var") (+ space) "-" alpha)
;;                           "comment"))))
;;     "Alist of major modes to regular expressions for finding private definitions")
;;   (defun hs-hide-all-private ()
;;     "Hide all private definitions in the current buffer.
;; Search is based on regular expressions in the
;; `hs-private-regex-mode-alist' variable."
;;     (interactive)
;;     (when hs-minor-mode
;;       (if-let ((re (alist-get major-mode hs-mode-private-regex-alist nil nil
;;                               (lambda (key1 key2)
;;                                 (if (listp key1)
;;                                     (and (memq key2 key1) t)
;;                                   (eq key1 key2))))))
;;           (save-excursion
;;             (goto-char (point-max))
;;             (while (re-search-backward re nil t)
;;               (hs-hide-block)))
;;         (error "Mode %s doesn't define a regex to find private definitions" major-mode))))
;;   :config
;;   (easy-menu-add-item hs-minor-mode-map '(menu-bar hide/show)
;;                       ["Hide all private definitions" hs-hide-all-private
;;                        :help "Hide all private definitions based on `hs-mode-private-regex-alist'."]
;;                       "--")
;;   (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
;;     "Move point to the location of the mouse pointer."
;;     (mouse-set-point last-input-event)))

;; (use-package repeat-mode
;;   :hook (after-init . repeat-mode))
;;; Messaging


;; (use-package message-view-patch
;;   :ensure t
;;   :hook (gnus-part-display . message-view-patch-highlight))


(require 'editor)
(require 'nav)
(require 'tools)
(require 'ui)


;;________________________________________________________________
;;;    Settings
;;________________________________________________________________
;; By default emacs will not delete selection text when typing on it, let's fix it
;; (delete-selection-mode t)
;; ;; find-file-at-point, smarter C-x C-f when point on path or URL
;; (ffap-bindings)
;; ;; Ask y or n instead of yes or no
;; (defalias 'yes-or-no-p 'y-or-n-p)
;; ;; show zero-width characters
;; (set-face-background 'glyphless-char "red")
;; (electric-indent-mode nil)  ; Auto indentation.
;; (global-subword-mode 1)     ; Iterate through CamelCase words.
;; (global-auto-revert-mode 1) ; Automatically revert buffer when it changes on disk.
;; (mouse-avoidance-mode 'exile)
;; ;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
;; (global-dash-fontify-mode)
;; (when window-system (global-prettify-symbols-mode t))

;; ;;;; Modeline
;; (size-indication-mode)
;; (setq display-time-24hr-format t
;;       ;; display-time-format "%l:%M%p" ;  %b %y"
;;       display-time-default-load-average nil)
;; (display-time-mode)

;; ;;;; General But Better Defaults
;; (setq-default
;;  ad-redefinition-action 'accept     ; Silence warnings for redefinition.
;;  confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
;;  cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
;;  speedbar t                         ; Quick file access with bar.
;;  backup-by-copying t                ; don't clobber symlinks.
;;  ;; backup-directory-alist `(("."~/.config/emacs/var/backup/per-session))
;;  default-directory "~/"
;;  custom-safe-themes t
;;  load-prefer-newer t ; don't use the compiled code if its the older package.
;;  make-backup-files t               ; backup of a file the first time it is saved.
;;  delete-by-moving-to-trash t       ; move deleted files to trash.
;;  delete-old-versions t             ; delete excess backup files silently.
;;  kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2).
;;  kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2).
;;  version-control t                 ; version numbers for backup files.
;;  auto-save-default t               ; auto-save every buffer that visits a file.
;;  auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
;;  auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
;;  compilation-always-kill t         ; kill compilation process before starting another.
;;  compilation-ask-about-save nil    ; save all buffers on `compile'.
;;  compilation-scroll-output t
;;  tab-width 4
;;  indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
;;  indent-line-function 'insert-tab
;;  require-final-newline t
;;  x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
;;  save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
;;  apropos-do-all t                  ; Shows all options when running apropos.
;;  mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click.
;;  message-log-max 1000
;;  fill-column 80
;;  make-pointer-invisible t          ; hide cursor when writing.
;;  column-number-mode t              ; show (line,column) in mode-line.
;;  cua-selection-mode t              ; delete regions.
;;  enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
;;  dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
;;  backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
;;  )

;; (setq
;;  debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
;;  jka-compr-verbose init-file-debug
;;  read-process-output-max (* 64 1024); 64kb
;;  ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
;;  idle-update-delay 1.0              ; default is 0.5.
;;  scroll-step 1                      ; scroll with less jump.
;;  scroll-preserve-screen-position t
;;  scroll-margin 3
;;  scroll-conservatively 101
;;  scroll-up-aggressively 0.01
;;  scroll-down-aggressively 0.01
;;  lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
;;  auto-window-vscroll nil            ; Lighten vertical scroll.
;;  fast-but-imprecise-scrolling nil
;;  mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;  mouse-wheel-progressive-speed nil
;;  hscroll-step 1                     ; Horizontal Scroll.
;;  hscroll-margin 1
;;  help-window-select t               ; select help window when opened
;;  redisplay-skip-fontification-on-input t
;;  tab-always-indent 'complete        ; smart tab behavior - indent or complete.
;;  visible-bell t                     ; Flash the screen on error, don't beep.
;;  view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
;;  use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
;;  echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
;;  delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
;;  kill-whole-line t 			        ; kills the entire line plus the newline
;;  save-place-forget-unreadable-files nil
;;  blink-matching-paren t             ; Blinking parenthesis.
;;  next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
;;  require-final-newline t            ; require final new line.
;;  mouse-sel-retain-highlight t       ; keep mouse high-lighted.
;;  highlight-nonselected-windows nil
;;  transient-mark-mode t              ; highlight the stuff you are marking.
;;  ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
;;  pgtk-wait-for-event-timeout 0.001
;;  display-line-numbers-type 'relative
;;  speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
;;  frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b") ; name of the file I am editing as the name of the window.
;;  )

;;________________________________________________________________
;;;;    Custom settings
;;________________________________________________________________
;; Separate Customization from init file
(setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Garbage collection on focus-out, Emacs should feel snappier
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;;; Load Path
;; Since all the configuration files are stored in a folder, they need to be added to `load-path' now.
;; (defun update-to-load-path (folder)
;;   "Update FOLDER and its subdirectories to `load-path'."
;;   (let ((base folder))
;;     (unless (member base load-path)
;;       (add-to-list 'load-path base))
;;     (dolist (f (directory-files base))
;;       (let ((name (concat base "/" f)))
;;         (when (and (file-directory-p name)
;;                    (not (equal f ".."))
;;                    (not (equal f ".")))
;;           (unless (member base load-path)
;;             (add-to-list 'load-path name)))))))

;; (update-to-load-path (expand-file-name "elpa" user-emacs-directory))

;;;; Load custom-files
;; (defun load-directory (dir)
;;   "Load all *.el files in a directory."
;;   (let ((load-it (lambda (f)
;;                    (load-file (concat (file-name-as-directory dir) f)))))
;;     (mapc load-it (directory-files dir nil "\\.el$"))))

;; (load-directory "~/.config/emacs/my-lisp") ; load my configuration of packages

;;;; remove old backup files
;; Automatically purge backup files not accessed in a week:
;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

;;; Finish up
(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; enable some major-mode
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
