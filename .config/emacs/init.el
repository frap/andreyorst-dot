;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 29.1+ configuration.

(defun gas/display-startup-time ()
  (message "Emacs chargé dans %s avec %d ramasse-miettes."
           (format "%.2f secondes"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gas/display-startup-time)
;;; Code:

;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

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

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

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

;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq
 user-full-name "Andrés Gasson"
 user-mail-address "gas@tuatara.red"
 github-account-name "frap")

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
           (emacs-lisp-mode "εlisp" :major)))

;; github.com/doomemacs/doomemacs/blob/develop/core/core.el#L296
(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq
   gcmh-idle-delay 'auto ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :delight " Ⓖ")

;;;; ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-n" . nil)
         ("M-o" . nil)
         ("M-p" . nil))
  :delight)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (name . "\*mu4e\*"))
               ("coding" (or
                               (mode . python-mode)
                               (mode . clojure-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)
;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)
(setq ibuffer-default-sorting-mode 'recency)

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
   frame-title-format "εmacs"
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
(use-package window
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Calendar*"
                 (display-buffer-at-bottom))))

(use-package messages
  :no-require
  :preface
  (provide 'messages)
  :bind ( :map messages-buffer-mode-map
          ("C-c C-o" . messages-clear-buffer))
  :config
  (defun messages-clear-buffer ()
    "Clear the *Messages* buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

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

(use-package mouse
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil)))

;; doom-modeline dropped all-the-icons support in favor of nerd-icons
(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package mode-line
  :no-require
  :preface
  (defvar mode-line-interactive-position
    `(line-number-mode
      (:propertize " %l:%C"
                   help-echo "mouse-1: Goto line"
                   mouse-face mode-line-highlight
                   local-map ,(let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1] 'goto-line)
                                map)))
    "Mode line position with goto line binding.")
  (put 'mode-line-interactive-position 'risky-local-variable t)
  (fset 'abbreviate-file-name-memo (memoize #'abbreviate-file-name))
  (defvar mode-line-buffer-file-name
    '(:eval (propertize (if-let ((name (buffer-file-name)))
                            (abbreviate-file-name-memo name)
                          (buffer-name))
                        'help-echo (buffer-name)
                        'face (when (and (buffer-file-name) (buffer-modified-p))
                                'font-lock-builtin-face)))
    "Show file name if buffer is visiting a file, otherwise show
buffer name.  If file is modified, a `font-lock-builtin-face' is
applied to the name.")
  (put 'mode-line-buffer-file-name 'risky-local-variable t)
  (defvar mode-line-input-method
    '(:eval (when current-input-method-title
              (propertize (concat " " current-input-method-title)
                          'help-echo (concat "Input method: " current-input-method))))
    "Display input method name in the modeline.")
  (put 'mode-line-input-method 'risky-local-variable t)
  (defvar mode-line-buffer-encoding
    '(:eval (propertize
             (let ((sys (coding-system-plist buffer-file-coding-system)))
               (concat " " (if (memq (plist-get sys :category)
                                     '(coding-category-undecided coding-category-utf-8))
                               "UTF-8"
                             (upcase (symbol-name (plist-get sys :name))))))
             'help-echo 'mode-line-mule-info-help-echo
             'local-map mode-line-coding-system-map)))
  (put 'mode-line-buffer-encoding 'risky-local-variable t)
  (defvar mode-line-line-encoding
    '(:eval (when-let ((eol (pcase (coding-system-eol-type buffer-file-coding-system)
                              (0 "LF")
                              (1 "CRLF")
                              (2 "CR")
                              (_ nil))))
              (propertize
               (concat " " eol)
               'help-echo (format "Line ending style: %s"
                                  (pcase eol
                                    ("LF" "Unix style LF")
                                    ("CRLF" "DOS style CRLF")
                                    ("CR" "Mac style CR")
                                    (_ "Undecided")))
               'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                            map)))))
  (put 'mode-line-line-encoding 'risky-local-variable t)
  (setq-default mode-line-format
                '(" " mode-line-buffer-file-name mode-line-input-method
                  mode-line-buffer-encoding mode-line-line-encoding
                  mode-line-interactive-position (vc-mode vc-mode) " "
                  mode-line-modes " " mode-line-misc-info))
  (provide 'mode-line))

(use-package font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (defun aorst/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1000.0)))
  (defun setup-fonts ()
    (cond ((font-installed-p "JetBrains Mono")
           (set-face-attribute 'default nil :font "JetBrains Mono")) ;; :size (eval (round (* 14 resolution-factor)))
          ((font-installed-p "Iosevka Curly")
           (set-face-attribute 'default nil :font "Iosevka Curly")))
    (when (font-installed-p "Overpass")
      (set-face-attribute 'variable-pitch nil :font "Overpass")))
   ;; When Emacs is ran in GUI mode, configure common Emoji fonts, making it more
   ;; likely that Emoji will work out of the box
   (when (display-graphic-p)
     (set-fontset-font t 'symbol "Apple Color Emoji")
     (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
     (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
     (set-fontset-font t 'symbol "Symbola" nil 'append))
   (provide 'font))

;;;; ligature
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;;; ligature-for-jetbrain
(when (aorst/font-installed-p "JetBrainsMono")
  (dolist (char/ligature-re
           `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
                               "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                           (+ "<"))))
             (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  ,(rx (+ "&")))
             (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                           (+ "|"))))
             (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  ,(rx (or "+>" (+ "+"))))
             (?\[ ,(rx (or "[<" "[|")))
             (?\{ ,(rx "{|"))
             (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
             (?\; ,(rx (+ ";")))
             (?_  ,(rx (or "_|_" "__")))
             (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  ,(rx "$>"))
             (?^  ,(rx "^="))
             (?\] ,(rx "]#"))))
    (apply (lambda (char ligature-re)
             (set-char-table-range composition-function-table char
                                   `([,ligature-re 0 font-shape-gstring])))
           char/ligature-re)))


(use-package novice
  :preface
  (defvar disabled-commands (locate-user-emacs-file "disabled.el")
    "File to store disabled commands, that were enabled permanently.")
  :config
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  (load disabled-commands 'noerror))

(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package subr
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))

(use-package mwheel
  :bind (("S-<down-mouse-1>" . nil)
         ("S-<mouse-3>" . nil)
         ("<mouse-4>" . mwheel-scroll)
         ("<mouse-5>" . mwheel-scroll))
  :custom
  (mouse-wheel-flip-direction (not (featurep 'pgtk)))
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :preface
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (let ((buffer (current-buffer))
          (tabwidth tab-width))
      (or (> (buffer-size buffer) 1000000) ; avoid searching in huge buffers
          (with-temp-buffer
            (insert-buffer-substring buffer)
            (setq-local tab-width tabwidth)
            (untabify (point-min) (point-max))
            (goto-char (point-min))
            (let* ((window-width
                    ;; this computes a more accurate width rather than `window-width', and respects
                    ;; `text-scale-mode' font width.
                    (/ (window-body-width nil t) (window-font-width)))
                   (hscroll-offset
                    ;; `window-hscroll' returns columns that are not affected by
                    ;; `text-scale-mode'.  Because of that, we have to recompute the correct
                    ;; `window-hscroll' by multiplying it with a non-scaled value and
                    ;; dividing it with a scaled width value, rounding it to the upper
                    ;; boundary.  Since there's no way to get unscaled value, we have to get
                    ;; a width of a face that is not scaled by `text-scale-mode', such as
                    ;; `window-divider' face.
                    (ceiling (/ (* (window-hscroll) (window-font-width nil 'window-divider))
                                (float (window-font-width)))))
                   (line-number-width
                    ;; compensate line numbers width
                    (if (bound-and-true-p display-line-numbers-mode)
                        (- display-line-numbers-width)
                      0))
                   (threshold (+ window-width hscroll-offset line-number-width
                                 -2))) ; compensate imprecise calculations
              (catch 'excessive
                (while (not (eobp))
                  (let ((start (point)))
                    (save-restriction
                      (narrow-to-region start (min (+ start 1 threshold)
                                                   (point-max)))
                      (forward-line 1))
                    (unless (or (bolp)
                                (and (eobp) (<= (- (point) start)
                                                threshold)))
                      (throw 'excessive t))))))))))
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode no-hscroll-modes))
         (truncated-lines-p)))
  :init
  (if (fboundp #'context-menu-mode)
      (context-menu-mode 1)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package select
  :no-require
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape)
         (after-init . column-number-mode)
         (after-init . line-number-mode))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  (shell-command-default-error-buffer "*Commands Shell Erreurs*")
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  :preface
  (unless (fboundp 'minibuffer-keyboard-quit)
    (autoload #'minibuffer-keyboard-quit "delsel" nil t))
  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro
                  executing-kbd-macro)
        (funcall-interactively quit))))
  (define-advice exchange-point-and-mark
      (:around (fn &optional arg) tmm)
    "Conditionally exchange point and mark.

Only exchange point and mark when `transient-mark-mode' is either
disabled, or enabled and the mark is active."
    (when (or (and transient-mark-mode
                   mark-active)
              (not transient-mark-mode))
      (funcall fn arg))))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package common-lisp-modes
  :vc (:url "https://gitlab.com/andreyorst/common-lisp-modes.el.git"))

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

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

(use-package bindings
  :bind ( :map ctl-x-map
          ("C-d" . dired-jump))
  :preface
  ;; set global keybindings
  ;;; window management
  (global-set-key (kbd "M-p") (kbd "C-- C-x o"))
  (global-set-key (kbd "M-n") (kbd "C-x o"))
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  ;;; copy & eval
  (global-set-key (kbd "s-v") 'clipboard-yank)
  (global-set-key (kbd "s-k") 'kill-current-buffer)
  (global-set-key (kbd "s-e") 'eval-region)
  (global-set-key (kbd "s-b") 'eval-buffer)
  (global-set-key (kbd "s-c") 'ns-copy-including-secondary)
  (global-set-key (kbd "M-v") 'clipboard-yank)
  :init
  (setq mode-line-end-spaces nil))

(use-package frame
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore))
  :config
  (define-advice toggle-frame-fullscreen
      (:before (&optional frame) hide-menu-bar)
    "Hide menu bar when FRAME goes full screen."
    (set-frame-parameter
     nil 'menu-bar-lines
     (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)) 1 0)))
  (define-advice switch-to-buffer-other-frame
      (:around (fn buffer-or-name &optional norecord) clone-frame-parameters)
    "Clone fame parameters when switching to another frame."
    (let* ((default-frame-alist
            (seq-remove (lambda (elem) (eq (car elem) 'name))
                        (frame-parameters (selected-frame)))))
      (funcall-interactively fn buffer-or-name norecord))))

(use-package startup
  :no-require
  :custom
  (inhibit-splash-screen t)
  :config
  (setq initial-major-mode #'emacs-lisp-mode)
  (setq initial-scratch-message
        ";; ABANDONNEZ TOUT ESPOIR VOUS QUI ENTREZ ICI\n\n" )
  (defun +scratch-immortal ()
    "Bury, don't kill \"*scratch*\" buffer.
          For `kill-buffer-query-functions'."
    (if (eq (current-buffer) (get-buffer "*scratch*"))
        (progn (bury-buffer)
               nil)
      t))
  (defun +scratch-buffer-setup ()
    "Add comment to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for:" mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (next-line 2))
      (rename-buffer (concat "*scratch<" mode ">*") t)))
  (add-hook 'kill-buffer-query-functions #'+scratch-immortal))

(use-package menu-bar
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package tooltip
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset (line-pixel-height))
  (tooltip-frame-parameters
   `((name . "tooltip")
     (internal-border-width . 2)
     (border-width . 1)
     (no-special-glyphs . t))))

;; paste in text terminalform gui
(when (and (not (display-graphic-p))
           (executable-find "xclip"))
  (use-package xclip
    :config
    (when (executable-find xclip-program)
      (with-no-warnings
        (xclip-mode t)))))

(use-package modus-themes
  :ensure t
  :requires (local-config)
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-completions
   '((matches . (intense bold))
     (selection . (intense))))
  (modus-operandi-palette-overrides
   '((bg-main "#fbfbfb")
     (string "#702f00")
     (bg-line-number-active "#f0f0f0")))
  (modus-vivendi-palette-overrides
   `((bg-main ,(if (in-termux-p) "#000000" "#181818"))
     (bg-line-number-active "#1e1e1e")
     (string "#f5aa80")))
  :custom-face
  (region ((t :extend nil))))

(use-package modus-themes
  :after modus-themes
  :no-require
  :custom
  (modus-themes-common-palette-overrides
   `(;; syntax
     (builtin magenta-faint)
     (keyword cyan-faint)
     (comment fg-dim)
     (constant blue-faint)
     (docstring fg-dim)
     (docmarkup fg-dim)
     (fnname magenta-faint)
     (preprocessor cyan-faint)
     (string red-faint)
     (type magenta-cooler)
     (variable blue-faint)
     (rx-construct magenta-faint)
     (rx-backslash blue-faint)
     ;; misc
     (bg-paren-match bg-ochre)
     (bg-region bg-inactive)
     (fg-region unspecified)
     ;; line-numbers
     (fg-line-number-active fg-main)
     (bg-line-number-inactive bg-main)
     (fg-line-number-inactive fg-dim)
     ;; modeline
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     ;; links
     (underline-link unspecified)
     (underline-link-visited unspecified)
     (underline-link-symbolic unspecified)
     ,@modus-themes-preset-overrides-faint))
  :config
  (load-theme
   (if (dark-mode-enabled-p)
       local-config-dark-theme
     local-config-light-theme)
   t))

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

;;;; uniquify-files
(use-package uniquify-files
  :ensure t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

(use-package formfeed
  :no-require
  :hook ((help-mode
          org-mode
          outline-mode
          prog-mode)
         . formfeed-make-display-line)
  :preface
  (defun formfeed-make-display-line ()
    "Display the formfeed ^L char as a comment or as a continuous line."
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list (or fill-column 70)
                              (make-glyph-code
                               (string-to-char (or comment-start "-"))
                               'shadow)))))
  (provide 'formfeed))

(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package paren
  :hook (prog-mode . show-paren-mode))

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

(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t))

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package esh-mode
  :hook (eshell-mode . common-lisp-modes-mode)
  :preface
  (declare-function eshell-search-path "ext:esh-ext")
  (defun eshell-prompt ()
    (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
           (path (abbreviate-file-name default-directory))
           (branch (when (and (eshell-search-path "git")
                              (locate-dominating-file default-directory ".git"))
                     (concat (propertize (propertize " on " 'face '(:inherit shadow)))
                             (propertize (string-trim (shell-command-to-string "git branch --show-current"))
                                         'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
                                                   '(:inherit shadow)
                                                 '(:inherit font-lock-builtin-face))))))
           (container (cond
                       ((file-exists-p "/run/.containerenv")
                        (format " in %s"
                                (with-temp-buffer
                                  (save-match-data
                                    (insert-file-contents "/run/.containerenv")
                                    (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
                                    (switch-to-buffer (current-buffer))
                                    (or (match-string-no-properties 1) "podman")))))
                       ((file-exists-p "/.dockerenv") " in docker")))
           (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
           (info (concat (or branch "")
                         (propertize (concat (or container "")
                                             (or ssh ""))
                                     'face '(:inherit shadow))))
           (prompt (if (= eshell-last-command-status 0)
                       "$"
                     (propertize "$" 'face '(:inherit error)))))
      (concat date " " path info "\n" prompt " ")))
  :custom
  (eshell-scroll-show-maximum-output nil)
  (eshell-prompt-function 'eshell-prompt)
  (eshell-banner-message ""))

(use-package esh-module
  :after eshell
  :custom
  (eshell-modules-list
   (cl-remove 'eshell-term eshell-modules-list)))

;;;; Dired
(require 'dired)
(setq dired-listing-switches "-agho --group-directories-first"
      dired-omit-files "^\\.[^.].*"
      dired-omit-verbose nil
      dired-dwim-target t ; Copy and move files netween dired buffers
      dired-recursive-copies 'always ; "always" means no asking
      dired-recursive-deletes 'top   ; "top" means ask once for top level directory
      dired-ls-F-marks-symlinks t ; -F marks links with @
      dired-hide-details-hide-symlink-targets nil
      auto-save-list-file-prefix nil ; not create directory .emacs.d/auto-save-list
      ;; Auto refresh dired, but be quiet about it
      global-auto-revert-non-file-buffers t
      wdired-allow-to-change-permissions t
      auto-revert-verbose nil
      auto-revert-interval 1
      delete-by-moving-to-trash t)

(autoload 'dired-omit-mode "dired-x")

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (hl-line-mode 1)))
(define-key dired-mode-map "z" #'dired-omit-mode)
(define-key dired-mode-map "l" #'dired-up-directory)
(bind-keys :map dired-mode-map
           ("/" . dired-goto-file)
           ("," . dired-create-directory)
           ("." . dired-create-empty-file)
           ;; ("I" . dired-insert-subdir)
           ("K" . dired-kill-subdir)
           ;; ("O" . dired-find-file-other-window)
           ("[" . dired-prev-dirline)
           ("]" . dired-next-dirline)
           ;; ("^" . mode-line-other-buffer)
           ("x" . dired-do-delete)
           ("X" . dired-do-flagged-delete)
           ("y" . dired-do-copy))

;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/sxiv") ;or /usr/bin/gimp
;; (setq image-dired-marking-shows-next nil)
(setq image-dired-thumb-size 256)
;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c x") 'image-dired)
  (define-key dired-mode-map (kbd "M-<return>") 'image-dired-dired-display-external))

;; (use-package dired
;;   :bind ( :map dired-mode-map
;;           ("<backspace>" . dired-up-directory)
;;           ("M-<up>" . dired-up-directory)
;;           ("~" . dired-home-directory))
;;   :hook (dired-mode . dired-hide-details-mode)
;;   :custom
;;   (dired-listing-switches "-lAXhv --group-directories-first")
;;   :config
;;   (defun dired-home-directory ()
;;     (interactive)
;;     (dired (expand-file-name "~/"))))

(use-package comint
  :defer t
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil)
  (comint-input-ignoredups t))

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

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :delight hs-minor-mode
  :commands (hs-hide-block)
  :bind ( :map hs-minor-mode-map
          ("C-c @ C-p" . hs-hide-all-private))
  :preface
  (defvar hs-mode-private-regex-alist
    `(((emacs-lisp-mode lisp-mode)
       . ,(rx bol "(def" (+ (not space)) (+ space) (+ (not space)) "--"))
      ((clojure-mode clojurescrip-mode clojurec-mode)
       . ,(rx "(" (or "defn-"
                      (seq "def" (* (not space)) (+ space)
                           "^" (or ":private"
                                   (seq "{" (* (not "}")) ":private" (+ space) "true")))
                      "comment")))
      (zig-mode
       . ,(rx bol (* space) "fn" (+ (not "{")) "{"))
      (fennel-mode
       . ,(rx bol "(" (or (seq (or "fn" "local" "var") (+ space) "-" alpha)
                          "comment"))))
    "Alist of major modes to regular expressions for finding private definitions")
  (defun hs-hide-all-private ()
    "Hide all private definitions in the current buffer.
Search is based on regular expressions in the
`hs-private-regex-mode-alist' variable."
    (interactive)
    (when hs-minor-mode
      (if-let ((re (alist-get major-mode hs-mode-private-regex-alist nil nil
                              (lambda (key1 key2)
                                (if (listp key1)
                                    (and (memq key2 key1) t)
                                  (eq key1 key2))))))
          (save-excursion
            (goto-char (point-max))
            (while (re-search-backward re nil t)
              (hs-hide-block)))
        (error "Mode %s doesn't define a regex to find private definitions" major-mode))))
  :config
  (easy-menu-add-item hs-minor-mode-map '(menu-bar hide/show)
                      ["Hide all private definitions" hs-hide-all-private
                       :help "Hide all private definitions based on `hs-mode-private-regex-alist'."]
                      "--")
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

;;;; helpful
(use-package helpful
  :doc "Helpful improves the built-in Emacs help system by providing more contextual information."
  :commands (helpful-callable helpful-variable helpful-command helpful-symbol helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

(use-package  which-key
  :hook (after-init . which-key-mode)
  :init
    (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
    :config
    (setq which-key-idle-delay 0.2)
    (setq which-key-idle-secondary-delay 0.1)
    (which-key-setup-side-window-bottom)
    (setq which-key-replacement-alist
          '((("left") . ("🡸"))
            (("right") . ("🡺"))
            (("up") . ("🡹"))
            (("down") . ("🡻"))
            (("delete") . ("DEL"))
            (("\\`DEL\\'") . ("BKSP"))
            (("RET") . ("⏎"))
            ))
    (which-key-setup-minibuffer)
    ;;  (:with-hook which-key-init-buffer-hook
    ;;  (:hook (lambda (setq line-spacing 4))))
    )

(use-package doc-view
  :defer t
  :custom
  (doc-view-resolution 192))

(use-package flymake
  :preface
  (defvar flymake-prefix-map (make-sparse-keymap))
  (fset 'flymake-prefix-map flymake-prefix-map)
  :bind ( :map ctl-x-map
          ("!" . flymake-prefix-map)
          :map flymake-prefix-map
          ("l" . flymake-show-buffer-diagnostics)
          ("n" . flymake-goto-next-error)
          ("p" . flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-mode-line-lighter "FlyM")
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(use-package package-lint-flymake
  :ensure t
  :defer t)

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

(use-package face-remap
  :hook (text-scale-mode . text-scale-adjust-latex-previews)
  :preface
  (defun text-scale-adjust-latex-previews ()
    "Adjust the size of latex previews when changing text scale."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (pcase major-mode
              ('latex-mode (eq (overlay-get ov 'category)
                               'preview-overlay))
              ('org-mode (eq (overlay-get ov 'org-overlay-type)
                             'org-latex-overlay)))
        (overlay-put
         ov 'display
         (cons 'image
               (plist-put
                (cdr (overlay-get ov 'display))
                :scale (+ 1.0 (* 0.25 text-scale-mode-amount)))))))))

(use-package browse-url
  :when (fboundp 'xwidget-webkit-browse-url)
  :custom (browse-url-browser-function #'xwidget-webkit-browse-url))

(use-package repeat-mode
  :hook (after-init . repeat-mode))


;;; Completion

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)
  :bind ( ("M-y"         . consult-yank-pop)
          ("M-g g"       . consult-goto-line)
          :map ctl-x-map
          ("b" . consult-buffer)
          ("c" . consult-prefix-map)
          ("C-k C-k" . consult-kmacro)
          :map consult-prefix-map
          ("r" . consult-recent-file)
          ("o" . consult-outline)
          ("i" . consult-imenu)
          ("g" . consult-grep)
          :map dired-mode-map
         ("O" . consult-file-externally)
         :map help-map
         ("a" . consult-apropos)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (consult-preview-key nil)
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :ensure t
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :commands (corfu-quit)
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev)))


;;; Org

(use-package org
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link)
          ("M-Q" . split-pararagraph-into-lines)
          :map org-src-mode-map
           ("C-x w" . org-edit-src-exit)
           ("C-x C-s" . org-edit-src-exit))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
  :custom
  (org-tags-column -120)
  (org-startup-folded 'content)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-preview-latex-image-directory ".ltximg/")
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
   (setq
   ;; Default location of org files
   org-directory "~/logseq/"

   ;; Define stages for todo tasks
   org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))

   ;; When item enters DONE, add a CLOSED: property with current date-time stamp
   org-log-done 'time
      ;; Make TODO states easier to distinguish by using different colours
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   hl-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("DOING" . "DarkOrchid")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue"))

   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("DOING" . "DarkOrchid")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue"))
   )
   (unless (version<= org-version "9.1.9")
  (add-to-list 'org-modules 'org-tempo)))

(use-package ob-shell :after org)

(use-package org-capture
  :defer t
  :after blog
  :bind ("C-c o c" . org-capture)
  :custom
  (org-directory blog-directory)
  (org-capture-templates
   `(,blog-capture-template
     ,@(mapcar
        (lambda (spec)
          (seq-let (btn descr heading) spec
            `( ,btn ,descr entry
               (file+headline ,(expand-file-name "kb/index.org" blog-directory) ,heading)
               "* [[blog-html:%^{Link}][%^{Description}]]\n:properties:\n:blog-collapsable: t\n:end:"
               :immediate-finish t
               :before-finalize (org-hugo-export-to-md))))
        '(("a" "Article" "Articles")
          ("t" "Talk" "Talks")
          ("w" "Web page" "Various Web pages")
          ("b" "Books, courses" "Books, Courses"))))))

(use-package org-tree-slide
  :ensure t
  :defer t
  :custom
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-never-touch-face t))

(use-package org-modern
  :ensure t
  :defer t
  :hook (org-tree-slide-mode . org-modern-mode)
  :custom-face
  (org-modern-block-name ((t (:height 1.0))))
  (org-modern-label ((t (:height 1.0))))
  :custom
  (org-modern-hide-stars t)
  (org-modern-block-fringe nil))

(use-package ox-latex
  :after ox)


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

(use-package elisp-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

;; (use-package pprint-to-buffer
;;   :vc (:url "https://github.com/plexus/plexmacs.git"
;;        :rev :newest
;;        :lisp-dir "pprint-to-buffer/"))

(use-package racket-mode
  :ensure t
  :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

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

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

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

(use-package csv-mode
  :ensure t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package scala-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t)

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4))

(use-package ob-lua :after org)

(use-package fennel-mode
  :vc (:url "https://git.sr.ht/~technomancy/fennel-mode" :branch "main" :rev :newest)
  :hook ((fennel-mode . fennel-proto-repl-minor-mode)
         ((fennel-mode
           fennel-repl-mode
           fennel-proto-repl-mode)
          . common-lisp-modes-mode))
  :bind ( :map fennel-mode-map
          ("M-." . xref-find-definitions)
          ("M-," . xref-go-back)
          :map fennel-repl-mode-map
          ("C-c C-o" . fennel-repl-delete-all-output))
  :custom
  (fennel-eldoc-fontify-markdown t)
  (fennel-scratch-use-proto-repl t)
  :preface
  (defun fennel-repl-delete-all-output ()
    (interactive)
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-region (point) (point-min)))))
  :config
  (dolist (sym '(global local var set))
    (put sym 'fennel-indent-function 1)))

(use-package ob-fennel :after org)

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
                 (point)))))
)

(use-package clj-ns-name
  :vc (:url "https://github.com/corgi-emacs/clj-ns-name.git")
  :config
  (clj-ns-name-install))

(use-package walkclj
  :ensure t
  :vc (:url "https://github.com/corgi-emacs/walkclj.git"))

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

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package inf-lisp
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
  :hook (scheme-mode . common-lisp-modes-mode))

(use-package geiser
  :ensure t
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package sql-indent
  :ensure t)

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
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

;;; Navigation & Editing

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
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
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

(use-package region-bindings
  :vc (:url "https://gitlab.com/andreyorst/region-bindings.el.git")
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

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

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package dumb-jump
  :ensure t
  :defer t
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package phi-search
  :ensure t
  :defer t)

;;;; avy
(use-package avy
  :bind(("C-'" . 'avy-goto-char)
        ("C-:" . 'avy-goto-char-2)
        ("M-g g" . 'avy-goto-line)
        ("M-g e" . 'avy-goto-word-0)
        ("M-g w" . 'avy-goto-word-1)
        ;; ("M-" . 'avy-copy-line)
        ;; ("M-" . 'avy-copy-region)
        ("M-g l" . 'avy-move-line)
        ("M-g M-r" . 'avy-move-region)
        ("C-c C-j" . 'avy-resume))
  :config
  (setq avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode exwm-mode))
  :custom
  (avy-timeout-seconds 0.5)
  (avy-style 'pre))
;; :custom-face
;; (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))))

;; (use-package avy
;;   :ensure t
;;   :bind
;;   (("C-:" . avy-goto-char-timer)
;;    ("C-M-:" . avy-goto-line)))

(use-package isayt
  :vc (:url "https://gitlab.com/andreyorst/isayt.el.git")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

;; ;;;; undo-tree
;; ;; Allow tree-semantics for undo operations.
;; (use-package undo-tree
;;   :ensure t
;;   :delight
;;   :bind ("C-x u" . undo-tree-visualize)
;;   :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
;;   :init (global-undo-tree-mode)
;;   :custom
;;   ;; Show a diff window displaying changes between undo nodes.
;;   (undo-tree-visualizer-diff t)
;;   ;; Prevent undo tree files from polluting your git repo
;;   (undo-tree-history-directory-alist '(("." . "~/.config/emacs/var/undo-tree-hist")))
;;   ;; Each node in the undo tree should have a timestamp.
;;   (undo-tree-visualizer-timestamps t))

(use-package vundo
  :ensure t
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 20))

(use-package terraform-mode
  :ensure t)

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


;;; Tools

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package project
  :ensure t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers
   '("Cargo.toml" "compile_commands.json"
     "compile_flags.txt" "project.clj"
     "deps.edn" "shadow-cljs.edn" "bb.edn"))
  :preface
  (defcustom project-compilation-mode nil
    "Mode to run the `compile' command with."
    :type 'symbol
    :group 'project
    :safe #'symbolp
    :local t)
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (define-advice compilation-start
      (:filter-args (args) use-project-compilation-mode)
    (let ((cmd (car args))
          (mode (cadr args))
          (rest (cddr args)))
      (if (and (null mode) project-compilation-mode)
          (append (list cmd project-compilation-mode) rest)
        args)))
  (define-advice project-root (:filter-return (project) abbreviate-project-root)
    (abbreviate-file-name project))
  (defun project-make-predicate-buffer-in-project-p ()
    (let ((project-buffers (project-buffers (project-current))))
      (lambda () (memq (current-buffer) project-buffers))))
  (define-advice project-compile (:around (fn) save-project-buffers-only)
    "Only ask to save project-related buffers."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (project-make-predicate-buffer-in-project-p)))
      (funcall fn)))
  (define-advice recompile
      (:around (fn &optional edit-command) save-project-buffers-only)
    "Only ask to save project-related buffers if inside of a project."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (if (project-current)
               (project-make-predicate-buffer-in-project-p)
             compilation-save-buffers-predicate)))
      (funcall fn edit-command)))
  :config
  (add-to-list 'project-switch-commands
               '(project-dired "Dired"))
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer")))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

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

(use-package magit
  :ensure t
    :defer t
  :bind ("C-c g" . magit-status)
  :defines (magit-status-mode-map
            magit-revision-show-gravatars
            magit-display-buffer-function
            magit-diff-refine-hunk)
  :commands (magit-display-buffer-same-window-except-diff-v1
             magit-stage-file
             magit-unstage-file)
  :init
  (setq-default magit-git-executable (executable-find "git"))
  :hook (git-commit-mode . flyspell-mode)
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
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
  :when window-system
  :defer t
  :bind (("C-x P" . git-gutter:popup-hunk)
         ("M-P" . git-gutter:previous-hunk)
         ("M-N" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk))
  :hook ((prog-mode org-mode) . git-gutter-mode )
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "†") ; ✘
 ;; (setq git-gutter:added-sign "†")
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
  :when window-system
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

;;;;; git-time
(use-package git-timemachine
  :ensure t)

(use-package server
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

(use-package recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package compile
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

(use-package clojure-compilation-mode
  :no-require
  :preface
  (defun clojure-compilation--split-classpath (classpath)
    "Split the CLASSPATH string."
    (split-string classpath ":" t "[[:space:]\n]+"))
  (defmemo clojure-compilation--get-project-dependencies-memo
      (command _deps-file _mod-time)
    "Call COMMAND to obtain the classpath string.
DEPS-FILE and MOD-TIME are used for memoization."
    (thread-last
      command
      shell-command-to-string
      clojure-compilation--split-classpath
      (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
  (defun clojure-compilation--get-lein-project-dependencies (root)
    "Obtain classpath from lein for ROOT."
    (let* ((project-file (expand-file-name "project.clj" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "lein classpath" project-file mod-time)))
  (defun clojure-compilation--get-deps-project-dependencies (root)
    "Obtain classpath from deps for ROOT."
    (let* ((project-file (expand-file-name "deps.edn" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "clojure -Spath" project-file mod-time)))
  (defun clojure-compilation-get-project-dependencies (project)
    "Get dependencies of the given PROJECT.
Returns a list of all jar archives."
    (when (bound-and-true-p tramp-gvfs-enabled)
      (let ((root (project-root project)))
        (cond ((file-exists-p (expand-file-name "deps.edn" root))
               (clojure-compilation--get-deps-project-dependencies root))
              ((file-exists-p (expand-file-name "project.clj" root))
               (clojure-compilation--get-lein-project-dependencies root))))))
  (defvar-local clojure-compilation-project-deps nil
    "List of project's dependencies")
  (defvar-local clojure-compilation-project-deps-mod-time nil
    "Accumulated modification time of all project's libraries")
  (define-project-compilation-mode clojure-compilation
    (require 'tramp-gvfs)
    (setq-local clojure-compilation-project-deps
                (clojure-compilation-get-project-dependencies
                 clojure-compilation-current-project))
    (setq-local clojure-compilation-project-deps-mod-time
                (seq-reduce #'+ (mapcar (lambda (f)
                                          (time-to-seconds
                                           (file-attribute-modification-time
                                            (file-attributes f))))
                                        clojure-compilation-project-deps)
                            0)))
  (defun clojure-compilation--find-file-in-project (file)
    "Check if FILE is part of the currently compiled project."
    (if (file-name-absolute-p file)
        file
      (seq-find
       (lambda (s) (string-suffix-p file s))
       clojure-compilation-current-project-files)))
  (defun clojure-compilation--file-exists-jar-p (jar file)
    "Check if FILE is present in the JAR archive."
    (with-temp-buffer
      (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
  (defmemo clojure-compilation--find-dep-memo
      (file _project _deps-mod-time)
    "Find FILE in current project dependency list.
PROJECT and DEPS-MOD-TIME are used for memoizing the call."
    (when (not (string-empty-p file))
      (seq-find (lambda (d)
                  (clojure-compilation--file-exists-jar-p d file))
                clojure-compilation-project-deps)))
  (defun clojure-compilation--find-dep (file)
    "Find FILE in current project dependency list."
    (clojure-compilation--find-dep-memo
     file
     clojure-compilation-current-project
     clojure-compilation-project-deps-mod-time))
  (defun clojure-compilation-filename ()
    "Function that gets filename from the error message.
If the filename comes from a dependency, try to guess the
dependency artifact based on the project's dependencies."
    (when-let ((filename (substring-no-properties (match-string 1))))
      (or (clojure-compilation--find-file-in-project filename)
          (when-let ((dep (clojure-compilation--find-dep filename)))
            (concat (expand-file-name dep) "/" filename)))))
  :config
  (compile-add-error-syntax
   'clojure-compilation 'some-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
   :file #'clojure-compilation-filename
   :line 2 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-fail
   "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'eastwood-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
   :file #'clojure-compilation-filename
   :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))

(use-package fennel-compilation-mode
  :no-require
  :preface
  (define-project-compilation-mode fennel-compilation)
  :config
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-compile-error
   "^Compile error in \\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\)?\\$"
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-compile-error-2
   "^\\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\|\\?\\)? Compile error: "
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-test-error
   "^not ok[[:space:]]+[0-9]+[^
]+
#[[:space:]]+\\([^:]+\\):\\([0-9]+\\):"
   :file 1 :line 2 :level 'error)
  (compile-add-error-syntax
   'fennel-compilation
   'lua-stacktrace
   "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
   :file 1 :line 2))

(use-package password-store
  :no-require
  :when (executable-find "pass")
  :commands (password-store-copy
             password-store-get
             password-store-insert
             password-store-generate)
  :functions (password-store--completing-read@use-orderless)
  :load-path "/usr/share/doc/pass/emacs/"
  :config
  (define-advice password-store--completing-read
      (:around (fn &optional require-match) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn require-match))))

;;; Messaging


(use-package message-view-patch
  :ensure t
  :hook (gnus-part-display . message-view-patch-highlight))

(use-package elfeed
  :defer t
  :ensure t)


;;;; Encoding
;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;________________________________________________________________
;;;    Settings
;;________________________________________________________________
;; By default emacs will not delete selection text when typing on it, let's fix it
(delete-selection-mode t)
;; find-file-at-point, smarter C-x C-f when point on path or URL
(ffap-bindings)
;; Ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;; show zero-width characters
(set-face-background 'glyphless-char "red")
(electric-indent-mode nil)  ; Auto indentation.
(global-subword-mode 1)     ; Iterate through CamelCase words.
(global-auto-revert-mode 1) ; Automatically revert buffer when it changes on disk.
(mouse-avoidance-mode 'exile)
;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
(global-dash-fontify-mode)
(when window-system (global-prettify-symbols-mode t))

;;;; Modeline
(size-indication-mode)
(setq display-time-24hr-format t
      ;; display-time-format "%l:%M%p" ;  %b %y"
      display-time-default-load-average nil)
(display-time-mode)

;;;; General But Better Defaults
(setq-default
 ad-redefinition-action 'accept     ; Silence warnings for redefinition.
 confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
 cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
 speedbar t                         ; Quick file access with bar.
 backup-by-copying t                ; don't clobber symlinks.
 ;; backup-directory-alist `(("."~/.config/emacs/var/backup/per-session))
 default-directory "~/"
 custom-safe-themes t
 load-prefer-newer t ; don't use the compiled code if its the older package.
 make-backup-files t               ; backup of a file the first time it is saved.
 delete-by-moving-to-trash t       ; move deleted files to trash.
 delete-old-versions t             ; delete excess backup files silently.
 kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2).
 kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2).
 version-control t                 ; version numbers for backup files.
 auto-save-default t               ; auto-save every buffer that visits a file.
 auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
 auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
 compilation-always-kill t         ; kill compilation process before starting another.
 compilation-ask-about-save nil    ; save all buffers on `compile'.
 compilation-scroll-output t
 tab-width 4
 indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
 indent-line-function 'insert-tab
 require-final-newline t
 x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
 save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
 apropos-do-all t                  ; Shows all options when running apropos.
 mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click.
 message-log-max 1000
 fill-column 80
 make-pointer-invisible t          ; hide cursor when writing.
 column-number-mode t              ; show (line,column) in mode-line.
 cua-selection-mode t              ; delete regions.
 enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
 dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
 backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
 )

(setq
 debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
 jka-compr-verbose init-file-debug
 read-process-output-max (* 64 1024); 64kb
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0              ; default is 0.5.
 scroll-step 1                      ; scroll with less jump.
 scroll-preserve-screen-position t
 scroll-margin 3
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
 auto-window-vscroll nil            ; Lighten vertical scroll.
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 hscroll-step 1                     ; Horizontal Scroll.
 hscroll-margin 1
 help-window-select t               ; select help window when opened
 redisplay-skip-fontification-on-input t
 tab-always-indent 'complete        ; smart tab behavior - indent or complete.
 visible-bell t                     ; Flash the screen on error, don't beep.
 view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
 use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
 echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
 delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
 kill-whole-line t 			        ; kills the entire line plus the newline
 save-place-forget-unreadable-files nil
 blink-matching-paren t             ; Blinking parenthesis.
 next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
 require-final-newline t            ; require final new line.
 mouse-sel-retain-highlight t       ; keep mouse high-lighted.
 highlight-nonselected-windows nil
 transient-mark-mode t              ; highlight the stuff you are marking.
 ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
 pgtk-wait-for-event-timeout 0.001
 display-line-numbers-type 'relative
 speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
 frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b") ; name of the file I am editing as the name of the window.
 )

;;________________________________________________________________
;;;;    Fonts
;;________________________________________________________________
(global-font-lock-mode 1)             ; Use font-lock everywhere.
(setq font-lock-maximum-decoration t) ; We have CPU to spare; highlight all syntax categories.

;; Set the font face
(cond ((aorst/font-installed-p "JetBrainsMono")
       (set-face-attribute 'default nil :font (font-spec :family "JetBrainsMono" :size 10.0 :weight 'regular))
       (set-face-attribute 'fixed-pitch nil :font (font-spec :family "JetBrainsMono" :size 10.0 :weight 'regular)))
      ((aorst/font-installed-p "Source Code Pro")
       (set-face-attribute 'default nil :font "Source Code Pro 10")))

;; For variable pitched fonts Iosevka Aile is used if available.
(when (aorst/font-installed-p "Iosevka Aile")
  (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :size 10.5 :weight 'regular))
  (set-face-attribute 'font-lock-comment-face nil :family "Iosevka Aile Oblique" :height 106) ; :foreground "#5B6268"
  (set-face-attribute 'font-lock-function-name-face nil :family "Iosevka Aile" :height 102 :slant 'italic :weight 'regular) ; 'medium
  ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  )

;; Set up emoji rendering
;; Default Windows emoji font
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Segoe UI Emoji"))

;; Linux emoji font
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Noto Color Emoji"))

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
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elpa" user-emacs-directory))

;;;; Load custom-files
(defun load-directory (dir)
  "Load all *.el files in a directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory "~/.config/emacs/my-lisp") ; load my configuration of packages

;;;; remove old backup files
;; Automatically purge backup files not accessed in a week:
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

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
