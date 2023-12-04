;;; my-lisp/nav.el --- Emacs Navigation -*- lexical-binding: t -*-

;;; Completion
(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev)))

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
