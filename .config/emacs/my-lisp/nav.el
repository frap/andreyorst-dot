;;; my-lisp/nav.el --- Emacs Navigation -*- lexical-binding: t -*-

;;; Completion

(defun add-cape-completions ()
  (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions
  ;;              #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions
  ;;              #'cape-symbol)
  )

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev))
  :hook (corfu-mode . add-cape-completions))

(use-package
 consult
 :demand t
 ;; Replace bindings. Lazily loaded due by `use-package'.
 :bind
 ( ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ("<help> a" . consult-apropos) ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line) ;; orig. goto-line
  ("M-g M-g" . consult-goto-line) ;; orig. goto-line
  ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings (search-map)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ;; ("M-s r" . consult-ripgrep)
  ("M-s r" . consult-ripgrep-project-root)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map
  isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch

 ;; Enable automatic preview at point in the *Completions* buffer. This is
 ;; relevant when you use the default completion UI. You may want to also
 ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
 :hook (completion-list-mode . consult-preview-at-point-mode)

 ;; The :init configuration is always executed (Not lazy)
 :init
 (defun consult-ripgrep-project-root (&optional initial)
   (interactive "P")
   (let ((dir (funcall consult-project-function)))
     (consult--grep
      "Ripgrep" #'consult--ripgrep-make-builder dir initial)))

 ;; (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")
 ;;(setq consult-project-function 'projectile-project-root)

 ;; Optionally configure the register formatting. This improves the register
 ;; preview for `consult-register', `consult-register-load',
 ;; `consult-register-store' and the Emacs built-ins.
 (setq
  register-preview-delay 0.1
  register-preview-function #'consult-register-format)

 ;; Optionally tweak the register preview window.
 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; Optionally tweak the register preview window.
 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; Use Consult to select xref locations with preview
 (setq
  xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)

 ;; Configure other variables and modes in the :config section,
 ;; after lazily loading the package.
 :config
 ;; Optionally configure preview. The default value
 ;; is 'any, such that any key triggers the preview.
 (setq consult-preview-key nil)
 ;; For some commands and buffer sources it is useful to configure the
 ;; :preview-key on a per-command basis using the `consult-customize' macro.
 (consult-customize
  consult-theme
  :preview-key
  '(:debounce 0.2 any)
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-bookmark
  consult-recent-file
  consult-xref
  consult--source-bookmark
  consult--source-file-register
  consult--source-recent-file
  consult--source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.4 any))

 ;; Optionally configure the narrowing key.
 ;; Both < and C-+ work reasonably well.
 (setq consult-narrow-key "<") ;; (kbd "C-+")

 ;; Optionally make narrowing help available in the minibuffer.
 ;; You may want to use `embark-prefix-help-command' or which-key instead.
 ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

 ;; Optionally configure a function which returns the project root directory.
 ;; There are multiple reasonable alternatives to chose from.
 ;;;; 1. project.el (project-roots)
 ;; (setq consult-project-root-function
 ;;       (lambda ()
 ;;         (when-let (project (project-current))
 ;;           (car (project-roots project)))))
 ;;;; 2. projectile.el (projectile-project-root)
 ;;(autoload 'projectile-project-root "projectile")
 ;;(setq consult-project-root-function #'projectile-project-root)
 ;;;; 3. vc.el (vc-root-dir)
  (setq consult-project-root-function #'vc-root-dir)
 ;;;; 4. locate-dominating-file
 ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
 )
;; (use-package consult
;;   :ensure t
;;   :commands (consult-completion-in-region)
;;   :preface
;;   (defvar consult-prefix-map (make-sparse-keymap))
;;   (fset 'consult-prefix-map consult-prefix-map)
;;   :bind ( ("M-y"         . consult-yank-pop)
;;           ("M-g g"       . consult-goto-line)
;;           :map ctl-x-map
;;           ("b" . consult-buffer)
;;           ("c" . consult-prefix-map)
;;           ("C-k C-k" . consult-kmacro)
;;           :map consult-prefix-map
;;           ("r" . consult-recent-file)
;;           ("o" . consult-outline)
;;           ("i" . consult-imenu)
;;           ("g" . consult-grep)
;;           :map dired-mode-map
;;           ("O" . consult-file-externally)
;;           :map help-map
;;           ("a" . consult-apropos)
;;           :map minibuffer-local-map
;;           ("M-r" . consult-history))
;;   :custom
;;   (consult-preview-key nil)
;;   :init
;;   (setq completion-in-region-function #'consult-completion-in-region))

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
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t) ;; Enable auto completion
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
  (corfu-scroll-margin 5) ;; Use scroll margin
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

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
   (require 'corfu-history)
   (corfu-history-mode 1)
   (savehist-mode 1)
   (add-to-list 'savehist-additional-variables 'corfu-history)
  :hook (before-save-hook . corfu-quit))

(use-package corfu-popupinfo
  :straight nil
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

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

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
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; Navigation
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

(use-package dumb-jump
  :ensure t
  :defer t
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package isearch
  :straight nil
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package phi-search
  :ensure t
  :defer t)

;;;Help
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
  :init (setq which-key-sort-order #'which-key-key-order-alpha
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

(provide 'nav)
