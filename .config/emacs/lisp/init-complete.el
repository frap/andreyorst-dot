;;; lisp/init-complete.el --- Emacs Navigation -*- lexical-binding: t -*-

;;; Completion

(use-package corfu
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :commands (corfu-quit)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :hook (corfu-mode . corfu-history-mode)
  :hook (before-save-hook . corfu-quit)
  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)  ;; Enable auto completion
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary 'separator) ;; Automatically quit at word boundary
  (corfu-quit-no-match 'separator) ;; Automatically quit if there is no match
  (corfu-scroll-margin 5)          ;; Use scroll margin
  ;; (corfu-preview-current nil)    ;; Do not preview current candidate
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-on-exact-match 'quit)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :straight nil
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :hook (after-init . corfu-terminal-mode))

(use-package prescient)
(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; Optionally:
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face))))
;; Remember to add an entry for `t', the library uses that as default.
;; The Custom interface is also supported for tuning the variable above.)

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev))
  :hook (corfu-mode . add-cape-completions)
  :config
  (defun add-cape-completions ()
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions
    ;;              #'cape-keyword)
    ;; (add-to-list 'completion-at-point-functions
    ;;              #'cape-symbol)
    ))

(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; (use-package consult
;;  :demand t
;;  ;; Replace bindings. Lazily loaded due by `use-package'.
;;  :bind

;;   ;; Custom M-# bindings for fast register access
;;   ("M-#" . consult-register-load)
;;   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
;;   ("C-M-#" . consult-register)

;;   ;; ("M-s r" . consult-ripgrep)
;;   ("M-s r" . consult-ripgrep-project-root)


;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI. You may want to also
;;   ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;  ;; The :init configuration is always executed (Not lazy)
;;  :init

;;  ;;(setq consult-project-function 'projectile-project-root)



;;  ;; Use Consult to select xref locations with preview
;;  (setq
;;   xref-show-xrefs-function #'consult-xref
;;   xref-show-definitions-function #'consult-xref)

;;  ;; Configure other variables and modes in the :config section,
;;  ;; after lazily loading the package.
;;  :config


;;  ;; Optionally make narrowing help available in the minibuffer.
;;  ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;  ;; Optionally configure a function which returns the project root directory.
;;  ;; There are multiple reasonable alternatives to chose from.
;;  ;;;; 1. project.el (project-roots)
;;  ;; (setq consult-project-root-function
;;  ;;       (lambda ()
;;  ;;         (when-let (project (project-current))
;;  ;;           (car (project-roots project)))))
;;  ;;;; 2. projectile.el (projectile-project-root)
;;  ;;(autoload 'projectile-project-root "projectile")
;;  ;;(setq consult-project-root-function #'projectile-project-root)
;;  ;;;; 3. vc.el (vc-root-dir)
;;   (setq consult-project-root-function #'vc-root-dir)
;;  ;;;; 4. locate-dominating-file
;;  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
;; )
;; (use-package consult
;;   :ensure t
;;   :commands (consult-completion-in-region)
;;   :preface
;;   (defvar consult-prefix-map (make-sparse-keymap))
;;   (fset 'consult-prefix-map consult-prefix-map)
;;   :bind
;;   ;; C-c bindings (mode-specific-map)
;;   ("C-c h" . consult-history)
;;   ("C-c m" . consult-mode-command)
;;   ("C-c b" . consult-bookmark)
;;   ("C-c k" . consult-kmacro)
;;   ;; C-x bindings (ctl-x-map)
;;   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
;;   ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
;;   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
;;   ;;   ;; Other custom bindings
;;   ("M-y" . consult-yank-pop) ;; orig. yank-pop
;;   ;; M-g bindings (goto-map)
;;   ("M-g e" . consult-compile-error)
;;   ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
;;   ("M-g g" . consult-goto-line)          ;; orig. goto-line
;;   ("M-g M-g" . consult-goto-line)        ;; orig. goto-line
;;   ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
;;   ("M-g m" . consult-mark)
;;   ("M-g k" . consult-global-mark)
;;   ("M-g i" . consult-imenu)
;;   ("M-g I" . consult-imenu-multi)
;;   ;; M-s bindings (search-map)
;;   ("M-s f" . consult-find)
;;   ("M-s F" . consult-locate)
;;   ("M-s g" . consult-grep)
;;   ("M-s G" . consult-git-grep)
;;   ("M-s r" . consult-ripgrep-project-root)
;;   ("M-s l" . consult-line)
;;   ("M-s L" . consult-line-multi)
;;   ("M-s m" . consult-multi-occur)
;;   ("M-s k" . consult-keep-lines)
;;   ("M-s u" . consult-focus-lines)
;;   ;; Isearch integration
;;   ("M-s e" . consult-isearch-history)
;;   :map isearch-mode-map
;;   ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
;;   ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
;;   ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
;;   ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
;;   :map ctl-x-map
;;   ("b" . consult-buffer)
;;   ("c" . consult-prefix-map)
;;   ("C-k C-k" . consult-kmacro)
;;   :map consult-prefix-map
;;   ("r" . consult-recent-file)
;;   ("o" . consult-outline)
;;   ("i" . consult-imenu)
;;   ("g" . consult-grep)
;;   :map dired-mode-map
;;   ("O" . consult-file-externally)
;;   :map help-map
;;   ("a" . consult-apropos)
;;   :map minibuffer-local-map
;;   ("M-r" . consult-history)
;;   :custom
;;   (consult-preview-key nil)
;;   :init
;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq
;;    register-preview-delay 0.1
;;    register-preview-function #'consult-register-format)

;; ;; Optionally tweak the register preview window.
;; ;; This adds thin lines, sorting and hides the mode line of the window.
;; (advice-add #'register-preview :override #'consult-register-window)

;;  ;; Optionally tweak the register preview window.
;;  ;; This adds thin lines, sorting and hides the mode line of the window.
;;  (advice-add #'register-preview :override #'consult-register-window)
;;   (setq completion-in-region-function #'consult-completion-in-region)
;;   (defun consult-ripgrep-project-root (&optional initial)
;;     (interactive "P")
;;     (let ((dir (funcall consult-project-function)))
;;       (consult--grep
;;        "Ripgrep" #'consult--ripgrep-make-builder dir initial)))
;;   (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")
;;   :config

;;   (setq consult-project-root-function #'vc-root-dir)
;;   )
;; Consult
;; Consult provides several enhanced functions for completing-read. It
;; fits nicely with Vertico.
;;
;; I generally remapped everything
;; obvious. consult-yank-from-kill-ring as a remapping of yank proved
;; a bit too disorienting.
(use-package consult
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap recentf-open] . consult-recent-file)
  ([remap yank] . nil)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-s r" . consult-ripgrep)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ([remap repeat-complex-command] . consult-complex-command)
  ("M-s e" . consult-isearch-history)
  ([remap isearch-edit-string] . consult-isearch-history)
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  ([remap Info-search] . consult-info)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  ;;  (consult-preview-key nil)
  ;; :bind ( :map ctl-x-map
  ;;         ("c" . consult-prefix-map)
  ;;         :map consult-prefix-map
  ;;         ("r" . consult-recent-file)
  ;;         ("o" . consult-outline)
  ;;         ("i" . consult-imenu)
  ;;         ("g" . consult-grep))
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-project-root-function #'vc-root-dir))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark)

(use-package embark-consult
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (setq tab-always-indent 'complete)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;;(setq minibuffer-prompt-properties
  ;;      '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package minibuffer
  :straight nil
  ;;  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))

(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

;; (use-package vertico
;;   :ensure t
;;   :bind ( :map vertico-map
;;           ("M-RET" . vertico-exit-input))
;;   :hook (after-init . vertico-mode))

;; (use-package vertico-directory
;;   :straight nil
;;   :after vertico
;;   :bind ( :map vertico-map
;;           ("RET" . vertico-directory-enter)
;;           ("DEL" . vertico-directory-delete-char)
;;           ("M-DEL" . vertico-directory-delete-word))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
(use-package vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (require 'vertico-indexed)
  (vertico-indexed-mode)  (vertico-mode)
  (require 'vertico-repeat)
  (require 'vertico-directory)
  (require 'vertico-multiform)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config
  (vertico-multiform-mode))

(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))


;;; Navigation
;; (use-package dumb-jump
;;   :ensure t
;;   :defer t
;;   :commands (dumb-jump-xref-activate)
;;   :custom
;;   (dumb-jump-prefer-searcher 'rg)
;;   (dumb-jump-selector 'completing-read)
;;   :init
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; (use-package page
;;   :straight nil
;;   :bind ( :map narrow-map
;;           ("]" . narrow-forward-page)
;;           ("[" . narrow-backward-page))
;;   :preface
;;   (defun narrow-forward-page (&optional count)
;;     (interactive "p")
;;     (or count (setq count 1))
;;     (widen)
;;     (forward-page count)
;;     (narrow-to-page))
;;   (defun narrow-backward-page (&optional count)
;;     (interactive "p")
;;     (or count (setq count 1))
;;     (widen)
;;     (forward-page (- (1+ count))) ; 1+ needed to actually cross page boundary
;;     (narrow-to-page)))

;; (use-package rect
;;   :straight nil
;;   :bind (("C-x r C-y" . rectangle-yank-add-lines))
;;   :preface
;;   (defun rectangle-yank-add-lines ()
;;     (interactive "*")
;;     (when (use-region-p)
;;       (delete-region (region-beginning) (region-end)))
;;     (save-restriction
;;       (narrow-to-region (point) (point))
;;       (yank-rectangle))))

;;; Messaging

;; (use-package message-view-patch
;;   :ensure t
;;   :hook (gnus-part-display . message-view-patch-highlight))

(provide 'init-complete)
