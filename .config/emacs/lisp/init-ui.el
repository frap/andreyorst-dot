;;; my-lisp/init-ui.el --- Emacs UI -*- lexical-binding: t -*-
(use-package defaults
  :straight nil
  :no-require
  :preface
  (setq-default
   ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
   idle-update-delay 1.0              ; default is 0.5.
   indent-tabs-mode nil
   load-prefer-newer t
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format  '(buffer-file-name "Ɛmacs: %b (%f)" "Ɛmacs: %b") ; name of the file I am editing as the name of the window.
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
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))

;; (setq
;;  debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
;;  jka-compr-verbose init-file-debug
;;  read-process-output-max (* 64 1024); 64kb


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
;;
;;  )

(use-package functions
  :straight nil
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

(use-package local-config
  :straight nil
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

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-minibuffer-flag t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   ;; :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (ace-window-display-mode 1))

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

(use-package font
  :straight nil
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (global-font-lock-mode 1)             ; Use font-lock everywhere.
  (setq font-lock-maximum-decoration t) ; We have CPU to spare; highlight all syntax categories.
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (if (find-font (font-spec :name font-name))
        t
      nil))
  ;; Set reusable font name variables
  (defvar my/fixed-width-font "JetBrains Mono"
    "The font to use for monospaced (fixed width) text.")

  (defvar my/variable-width-font "Iosevka Aile"
    "The font to use for variable-pitch (document) text.")
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1000.0)))
  ;; ;; show zero-width characters
  (set-face-background 'glyphless-char "red")
  (defun setup-fonts ()
    (when (font-installed-p my/fixed-width-font)
           (set-face-attribute 'default nil :font (font-spec :family my/fixed-width-font :height 180 :weight 'light))
           (set-face-attribute 'fixed-pitch nil :font (font-spec :family my/fixed-width-font :height 190 :weight 'light)))

    ;; For variable pitched fonts Iosevka Aile is used if available.
    (when (font-installed-p my/variable-width-font)
      (set-face-attribute 'variable-pitch nil :font  my/variable-width-font :height 1.3 :weight 'regular)
      ;;  (set-face-attribute 'font-lock-comment-face nil :family "Iosevka Aile Oblique" :height 106) ; :foreground "#5B6268"
      ;; (set-face-attribute 'font-lock-function-name-face nil :family "Iosevka Aile" :height 102 :slant 'italic :weight 'regular) ; 'medium
      ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)
      ;;(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
      ))
  ;; (when (font-installed-p "Overpass")
  ;;   (set-face-attribute 'variable-pitch nil :font "Overpass")))
  ;; When Emacs is ran in GUI mode, configure common Emoji fonts, making it more
  ;; likely that Emoji will work out of the box
  ;; Set up emoji rendering
  (when (display-graphic-p)
    (set-fontset-font t 'symbol "Apple Color Emoji")
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
    (set-fontset-font t 'symbol "Symbola" nil 'append))

    ;; presentation-mode
    ;; Load org-faces to make sure we can set appropriate faces
    (require 'org-faces)

    ;; Hide emphasis markers on formatted text
    (setq org-hide-emphasis-markers t)

                                        ; Resize Org headings
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

    ;; Make the document title a bit bigger
    (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (provide 'font))

(use-package frame
  :straight nil
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore))
  :config
  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
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
      (funcall-interactively fn buffer-or-name norecord)))
  ;; Use the following snippet after you’ve set the alpha value
(defun toggle-transparency ()
  "Crave for transparency!"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(defun switch-theme (theme)
  "Disable any currently active themes and load THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)))

(use-package menu-bar
  :straight nil
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package mouse
  :straight nil
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil)))

(use-package mwheel
  :straight nil
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

(use-package mode-line
  :straight nil
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
  ;; ;;;; Modeline
;; (size-indication-mode)
;; (setq display-time-24hr-format t
;;       ;; display-time-format "%l:%M%p" ;  %b %y"
;;       display-time-default-load-average nil)
;; (display-time-mode)
  (provide 'mode-line))

(use-package modus-themes
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
  :straight nil
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

;; doom-modeline dropped all-the-icons support in favor of nerd-icons
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package pixel-scroll
  :straight nil
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package tooltip
  :straight nil
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset (line-pixel-height))
  (tooltip-frame-parameters
   `((name . "tooltip")
     (internal-border-width . 2)
     (border-width . 1)
     (no-special-glyphs . t))))

(use-package window
  :straight nil
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Calendar*"
                 (display-buffer-at-bottom))))

;; paste in text terminalform gui
(when (and (not (display-graphic-p))
           (executable-find "xclip"))
  (use-package xclip
    :config
    (when (executable-find xclip-program)
      (with-no-warnings
        (xclip-mode t)))))

;;;; ligature
(use-package ligature
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
(when (font-installed-p "JetBrainsMono")
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


;;________________________________________________________________
;;;    Settings
;;________________________________________________________________
;; By default emacs will not delete selection text when typing on it, let's fix it
;; (delete-selection-mode t)
;; ;; find-file-at-point, smarter C-x C-f when point on path or URL
;; (ffap-bindings)
;; ;; Ask y or n instead of yes or no
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (mouse-avoidance-mode 'exile)
;; ;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
;; (global-dash-fontify-mode)
;; (when window-system (global-prettify-symbols-mode t))



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



(provide 'init-ui)
;;; init-ui.el ends here
