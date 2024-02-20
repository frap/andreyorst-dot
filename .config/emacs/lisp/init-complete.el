;;; lisp/init-complete.el --- Emacs Navigation -*- lexical-binding: t -*-

;;; Completion
(use-package corfu
  :bind (:map corfu-map
          ("C-n" . corfu-next)
          ("C-p" . corfu-previous)
          ("S-TAB" . corfu-previous)
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
  (corfu-auto nil)        ; Only use `corfu' when calling `completion-at-point' or
                          ; `indent-for-tab-command'
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)       ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;;(corfu-separator ?\s)          ;; Orderless field separator

  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
     (bound-and-true-p vertico--input))
     (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
     (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
:config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package corfu-doc
  ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
            ;; This is a manual toggle for the documentation popup.
            ([remap corfu-show-documentation] . corfu-doc-toggle) ; Remap the default doc command
            ;; Scroll in the documentation window
            ("M-n" . corfu-doc-scroll-up)
            ("M-p" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  (corfu-echo-documentation nil))


;; (use-package corfu-popupinfo
;;   :straight nil
;;   :bind ( :map corfu-popupinfo-map
;;           ("M-p" . corfu-popupinfo-scroll-down)
;;           ("M-n" . corfu-popupinfo-scroll-up))
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom-face
;;   (corfu-popupinfo ((t :height 1.0))))

;; (use-package corfu-terminal
;;   :straight (corfu-terminal
;;              :type git
;;              :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
;;   :unless (display-graphic-p)
;;   :hook (after-init . corfu-terminal-mode))

 ;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

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

;; Cape provides Completion At Point Extensions which can be used in combination with Corfu, Company or the default completion UI.
(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)
         )
  :custom
  (cape-dabbrev-min-length 3)
  :init
  ;; Elisp
  (defun kb/cape-capf-ignore-keywords-elisp (cand)
    "Ignore keywords with forms that begin with \":\" (e.g.
:history)."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file)
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Eshell
  (defun kb/cape-capf-setup-eshell ()
    (let ((result))
      (dolist (element '(pcomplete-completions-at-point cape-file) result)
        (add-to-list 'completion-at-point-functions element))
      ))

  ;; Git-commit
  (defun kb/cape-capf-setup-git-commit ()
  (let ((result))
    (dolist (element '(cape-symbol cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))
  :hook (git-commit-mode . kb/cape-capf-setup-git-commit)
  :config
 ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; (use-package popon
;;   :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))


;; Consult
;; Consult provides several enhanced functions for completing-read. It
;; fits nicely with Vertico.
;;
;; I generally remapped everything
;; obvious. consult-yank-from-kill-ring as a remapping of yank proved
;; a bit too disorienting.

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)
  :bind
  ;; C-c bindings (mode-specific-map)
 (("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ;;   ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line)          ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)        ;; orig. goto-line
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
  ("M-s r" . consult-ripgrep-project-root)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
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
  (setq completion-in-region-function #'consult-completion-in-region)
  (defun consult-ripgrep-project-root (&optional initial)
    (interactive "P")
    (let ((dir (funcall consult-project-function)))
      (consult--grep
       "Ripgrep" #'consult--ripgrep-make-builder dir initial)))
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")
  :config
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
 ;; Use Consult to select xref locations with preview
 (setq
  xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref))


;;   ;; Custom M-# bindings for fast register access
;;   ("M-#" . consult-register-load)
;;   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
;;   ("C-M-#" . consult-register)

;;   ;; ("M-s r" . consult-ripgrep)
;;   ("M-s r" . consult-ripgrep-project-root)

;;  ;; Optionally make narrowing help available in the minibuffer.
;;  ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;Consult provides search and navigation commands based on the Emacs completion function completing-read.
;; (use-package consult
;;   :demand
;;   :commands (consult-completion-in-region)
;;   :preface
;;   (defvar consult-prefix-map (make-sparse-keymap))
;;   (fset 'consult-prefix-map consult-prefix-map)
;;   :init
;;   (setq completion-in-region-function #'consult-completion-in-region)
;;   (setq consult-project-root-function #'vc-root-dir)
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)
;;   (setq consult-preview-key "C-l")
;;   (setq consult-narrow-key "<"
;;         consult-widen-key ">")
;;   :bind
;;   ([remap switch-to-buffer] . consult-buffer)
;;   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
;;   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
;;   ([remap project-switch-to-buffer] . consult-project-buffer)
;;   ([remap bookmark-jump] . consult-bookmark)
;;   ([remap recentf-open] . consult-recent-file)
;;   ([remap yank] . nil)
;;   ([remap yank-pop] . consult-yank-pop)
;;   ([remap goto-line] . consult-goto-line)
;;   ("M-g m" . consult-mark)
;;   ("M-g M" . consult-global-mark)
;;   ("M-g o" . consult-outline)
;;   ("M-g i" . consult-imenu)
;;   ("M-g I" . consult-imenu-multi)
;;   ("M-s l" . consult-line)
;;   ("M-s L" . consult-line-multi)
;;   ("M-s k" . consult-keep-lines)
;;   ("M-s u" . consult-focus-lines)
;;   ("M-s r" . consult-ripgrep)
;;   ("M-s f" . consult-find)
;;   ("M-s F" . consult-locate)
;;   ("M-g e" . consult-compile-error)
;;   ("M-g f" . consult-flymake)
;;   ([remap repeat-complex-command] . consult-complex-command)
;;   ("M-s e" . consult-isearch-history)
;;   ([remap isearch-edit-string] . consult-isearch-history)
;;   ([remap next-matching-history-element] . consult-history)
;;   ([remap previous-matching-history-element] . consult-history)
;;   ([remap Info-search] . consult-info)

;;   ;;  (consult-preview-key nil)
;;   ;; :bind ( :map ctl-x-map
;;   ;;         ("c" . consult-prefix-map)
;;   ;;         :map consult-prefix-map
;;   ;;         ("r" . consult-recent-file)
;;   ;;         ("o" . consult-outline)
;;   ;;         ("i" . consult-imenu)
;;   ;;         ("g" . consult-grep))
;; :config
;;   (global-set-key [remap imenu] 'consult-imenu)
;;   (global-set-key [remap switch-to-buffer] 'consult-buffer)
;;   (global-set-key [remap goto-line] 'consult-goto-line)
;;   (consult-customize consult-theme
;;                  :preview-key
;;                  '("M-."
;;                    :debounce 0.5 "<up>" "<down>"
;;                    :debounce 1 any))
;; )

;;Insert paths into the minibuffer prompt in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map ;; vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :after vertico
  :bind (:map vertico-map
              ("C-x C-l" . embark-act))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
  ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

  (use-package embark-consult
    :after (embark consult)
    ;; :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    ;; :hook
    ;; (embark-collect-mode . embark-consult-preview-minor-mode)
    ;;:hook (embark-collect-mode . consult-preview-at-point-mode)
    )

  ;; A few more useful configurations...
  ;; (use-package emacs
  ;;   :init
  ;;   ;; TAB cycle if there are only few candidates
  ;;   (setq completion-cycle-threshold 3)

  ;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;;   ;; (setq read-extended-command-predicate
  ;;   ;;       #'command-completion-default-include-p)

  ;;   ;; Enable indentation+completion using the TAB key.
  ;;   ;; `completion-at-point' is often bound to M-TAB.
  ;;   ;; should be configured in the `indent' package, but `indent.el'
  ;;   ;; doesn't provide the `indent' feature.
  ;;   (setq tab-always-indent 'complete)
  ;;   ;; Add prompt indicator to `completing-read-multiple'.
  ;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;;   (defun crm-indicator (args)
  ;;     (cons
  ;;      (format "[CRM%s] %s"
  ;;              (replace-regexp-in-string
  ;;               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
  ;;              (car args))
  ;;      (cdr args)))
  ;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;;   ;; Do not allow the cursor in the minibuffer prompt
  ;;   ;;(setq minibuffer-prompt-properties
  ;;   ;;      '(read-only t cursor-intangible t face minibuffer-prompt))
  ;;   ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;;   ;; Vertico commands are hidden in normal buffers.
  ;;   ;; (setq read-extended-command-predicate
  ;;   ;;       #'command-completion-default-include-p)

  ;;   ;; Enable recursive minibuffers
  ;;   (setq enable-recursive-minibuffers t))

  (use-package marginalia
    :after vertico
    :config
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
    :ensure t
    :demand
    ;; :straight (vertico :files (:defaults "extensions/*")
    ;;                    :includes (vertico-indexed
    ;;                               vertico-flat
    ;;                               vertico-grid
    ;;                               vertico-mouse
    ;;                               ;; vertico-quick
    ;;                               vertico-buffer
    ;;                               vertico-repeat
    ;;                               vertico-reverse
    ;;                               vertico-directory
    ;;                               vertico-multiform
    ;;                               vertico-unobtrusive
    ;;                               ))
     :hook
     ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
      (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
     )
    :bind
    (:map vertico-map
          ("C-'"           . vertico-quick-exit)
          ("C-c '"         . vertico-quick-insert)
          ("<return>"      . exit-minibuffer)
          ("C-m"           . vertico-insert)
          ("C-c SPC"       . vertico-quick-exit)
          ("C-<backspace>" . vertico)
          ("DEL"           . vertico-directory-delete-char)
          ;; ("RET" . vertico-directory-enter)
          ;; ("DEL" . vertico-directory-delete-char)
          ;; ("M-DEL" . vertico-directory-delete-word)
          )
    :init
     (setq vertico-resize t)

    ;; multiform extension
    ;; (setq vertico-grid-separator "       ")
    ;; (setq vertico-grid-lookahead 50)
    ;; (setq vertico-buffer-display-action '(display-buffer-reuse-window))
    ;; (setq vertico-multiform-categories
    ;;       '((file indexed)
    ;;         (consult-grep buffer)
    ;;         (consult-location)
    ;;         (imenu buffer)
    ;;         (library reverse indexed)
    ;;         (org-roam-node reverse indexed)
    ;;         (t reverse)
    ;;         ))
    ;; (setq vertico-multiform-commands
    ;;       '(("flyspell-correct-*" grid reverse)
    ;;         (org-refile grid reverse indexed)
    ;;         (consult-yank-pop indexed)
    ;;         (consult-flycheck)
    ;;         (consult-lsp-diagnostics)
    ;;         (git-related-find-file (vertico-sort-function . nil))
    ;;         ))
    ;; (defun kb/vertico-multiform-flat-toggle ()
    ;;   "Toggle between flat and reverse."
    ;;   (interactive)
    ;;   (vertico-multiform--display-toggle 'vertico-flat-mode)
    ;;   (if vertico-flat-mode
    ;;       (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
    ;;     (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

    ;; Workaround for problem with `tramp' hostname completions. This overrides
    ;; the completion style specifically for remote files! See
    ;; https://github.com/minad/vertico#tramp-hostname-completion
    ;; (defun lc/basic-remote-try-completion (string table pred point)
    ;;   (and (vertico--remote-p string)
    ;;        (completion-basic-try-completion string table pred point)))
    ;; (defun lc/basic-remote-all-completions (string table pred point)
    ;;   (and (vertico--remote-p string)
    ;;        (completion-basic-all-completions string table pred point)))
    ;; (add-to-list 'completion-styles-alist
    ;;              '(basic-remote           ; Name of `completion-style'
    ;;                lc/basic-remote-try-completion lc/basic-remote-all-completions nil))

    (setq completion-in-region-function
           (lambda (&rest args)
             (apply (if vertico-mode
                        #'consult-completion-in-region
                      #'completion--in-region)
                    args)))
    :config
    ;; (vertico-multiform-mode)
    (vertico-mode t)
    (vertico-mouse-mode)
    (set-face-attribute 'vertico-mouse nil :inherit nil)
    (savehist-mode)
    ;; Prefix the current candidate with “» ”. From
    ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
    (advice-add #'vertico--format-candidate :around
                (lambda (orig cand prefix suffix index _start)
                  (setq cand (funcall orig cand prefix suffix index _start))
                  (concat
                   (if (= vertico--index index)
                       (propertize "» " 'face 'vertico-current)
                     "  ")
                   cand))))

    (use-package vertico-prescient
      :hook (vertico-mode . vertico-prescient-mode))

    (use-package wgrep
      :commands wgrep-change-to-wgrep-mode
      :custom
      (wgrep-auto-save-buffer t))

    (provide 'init-complete)
