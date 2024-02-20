;;; lisp/init-lsp-treesit.el --- Emacs LSP/Treesit -*- lexical-binding: t -*-
;;; Languages
;;treesitter

(use-package treesit-auto
  :if (>= emacs-major-version 29)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-extra-load-path
               (substitute-env-in-file-name "$HOME/.config/emacs/tree-sitter"))
  ;;(treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (add-to-list 'treesit-extra-load-path
;;              (expand-file-name "~/.config/emacs/tree-sitter"))

;; (use-package treesit
;;   :straight nil
;;   :preface
;;   (defun install-ts-grammars (&optional force)
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '(
;;                ;; not sure where these ones are meant to come from, not at that url
;;                ;;(cmake  "https://github.com/tree-sitter/tree-sitter-cmake")
;;                ;;(go-mod  "https://github.com/tree-sitter/tree-sitter-go-mod")
;;                ;; these aren't yet part of emacs
;;                ;;(php  "https://github.com/tree-sitter/tree-sitter-php")
;;                ;;(html  "https://github.com/tree-sitter/tree-sitter-html")
;;                ;;(elisp  "https://github.com/tree-sitter/tree-sitter-elisp")
;;                ;;(swift  "https://github.com/tree-sitter/tree-sitter-swift")
;;                ;;(cli  "https://github.com/tree-sitter/tree-sitter-cli")
;;                (bash  "https://github.com/tree-sitter/tree-sitter-bash")
;;                (toml  "https://github.com/tree-sitter/tree-sitter-toml")
;;                (yaml  "https://github.com/tree-sitter/tree-sitter-yaml")
;;                (rust  "https://github.com/tree-sitter/tree-sitter-rust")
;;                (ruby  "https://github.com/tree-sitter/tree-sitter-ruby")
;;                (json  "https://github.com/tree-sitter/tree-sitter-json")
;;                (go  "https://github.com/tree-sitter/tree-sitter-go")
;;                (c "https://github.com/tree-sitter/tree-sitter-c")
;;                (cpp  "https://github.com/tree-sitter/tree-sitter-cpp")
;;                (css "https://github.com/tree-sitter/tree-sitter-css")
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;                (python "https://github.com/tree-sitter/tree-sitter-python")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       (if (or force (not (treesit-language-available-p (car grammar))))
;;           (treesit-install-language-grammar (car grammar)))))
;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (typescript-mode . tsx-ts-mode)
;;                      (js-mode . tsx-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (json-mode . json-ts-mode)
;;                      (go-mode . go-ts-mode)
;;                      (yaml-mode . yaml-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   (add-to-list 'treesit-extra-load-path
;;                (expand-file-name "~/.config/emacs/tree-sitter"))
;;   :config
;;   (install-ts-grammars)
;;   (setq +tree-sitter-hl-enabled-modes '(python-mode tsx-ts-mode)))

;;Code-folding using tree-sitter. Using the forked version with treesit support here
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "garyo/ts-fold" :branch "andrew-sw/treesit-el-support")
  :preface
  (defun my/ts-fold-mode-hook ()
    (keymap-local-set "S-TAB" 'ts-fold-toggle))
  :hook
  (((yaml-ts-mode python-ts-mode) . ts-fold-mode)
   ((yaml-ts-mode python-ts-mode) . ts-fold-indicators-mode)
   (ts-fold-mode . my/ts-fold-mode-hook)))


;;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook  (go-mode . lsp)
         (typescript-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (web-mode . lsp)
         (shell-script-mode . lsp)
         (c-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-mode)
         ;; This is needed so corfu + orderless work correctly on lsp-mode
         ;; (lsp-completion-mode . (lambda ()
         ;;                          (setf (alist-get 'styles
         ;;                                           (alist-get 'lsp-capf completion-category-defaults))
         ;;                                '(orderless))))
         (dired-mode . lsp-dired-mode)
   :custom
    ;;(lsp-ui-sideline-enable t) ;; "a little info over there")
    (lsp-prefer-capf t)
    (lsp-completion-provider :none) ;; "we use corfu now baby"
    ;;(lsp-disabled-clients nil '(jsts-ls ts-ls))
    (lsp-ui-doc-enable t) ;; "a little info over here")
    (lsp-ui-doc-show-with-cursor t)
    (lsp-idle-delay 0.1) ;; "might need to make this bigger if computers don't like it")
    (lsp-headerline-breadcrumb-enable t) ;; "it's cute!")
    (lsp-rust-analyzer-inlay-hints-mode t) ;; "analyze me papa")
    (lsp-completion-provider :capf );; "favoured by the team")
    (lsp-file-watch-threshold nil);; "i don't care, leave me alone")
    (lsp-eslint-validate '(svelte typescript js javascript))
    (lsp-typescript-server-args (--stdio --allowJs))
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (lsp-keymap-prefix "C-c l")
  ;; (lsp-auto-configure nil)
  ;; (lsp-diagnostics-provider :flymake)
  ;; (lsp-modeline-diagnostics-enable t)
;;   (lsp-completion-provider :none)
;; ;;  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
;;   (lsp-log-io nil)
;; ;;  (lsp-keep-workspace-alive nil)
;;   (lsp-idle-delay 0.5)
;;   (lsp-enable-xref t)
 ;; (lsp-signature-doc-lines 1)
  ;; :init
  ;; (setq lsp-restart 'ignore)
  ;; (setq lsp-eldoc-enable-hover t)
  ;; (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-keep-workspace-alive nil)
  ;; (setq lsp-auto-execute-action nil)
  ;; (setq lsp-before-save-edits nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-diagnostics-provider :none)
  ;;(setq lsp-use-plists t)
  :config
    ;; https://github.com/minad/corfu/wiki
  (defun lsp:setup-completion-for-corfu ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  (add-hook 'lsp-mode-hook #'lsp:setup-completion-for-corfu))

(use-package lsp-ui
  :demand t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("M-<mouse-1>" . lsp-find-definition-mouse)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references ))
  :custom-face
  (lsp-ui-doc-background ((t (:background "#ffffee")))))

;; (use-package consult-lsp
;;   :after lsp-mode
;;   :bind
;;   (:map lsp-mode-map
;;            ([remap xref-find-apropos] . consult-lsp-symbols)))

;; (use-package dap-mode
;;   :hook
;;   ((dap-mode . corfu-mode)
;;    (dap-terminated . lc/hide-debug-windows)
;;    ;; (dap-session-created . (lambda (_arg) (projectile-save-project-buffers)))
;;    (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
;;   )

;; (use-package lsp-completion
;;   :straight nil
;;   :no-require
;;   :hook ((lsp-mode . lsp-completion-mode-maybe))
;;   :commands (lsp-completion-mode)
;;   :preface
;;   (defun lsp-completion-mode-maybe ()
;;     (unless (bound-and-true-p cider-mode)
;;       (lsp-completion-mode 1))))

;; (use-package lsp-treemacs
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :custom
;;   (lsp-treemacs-theme "Iconless"))

(use-package lsp-clojure
  :straight nil
  :demand t
  :no-require
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp)
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


;; capf
;; (defmacro +inject-capf/fn (&rest capfs)
;;   `(dolist (fn ',capfs)
;;      (add-to-list 'completion-at-point-functions fn t)))

;; (defun +yasnippet-capf-1-prefix ()
;;   (cape-capf-prefix-length #'yasnippet-capf 1))

;; ;; Default for buffers that do not override `completion-at-point-functions'.
;; (+inject-capf/fn cape-file
;;                  cape-dabbrev)

;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook #'(lambda ()
;;                      (+inject-capf/fn cape-file
;;                                       cape-dabbrev
;;                                       +yasnippet-capf-1-prefix))))

;; (add-hook 'git-commit-mode-hook (lambda ()
;;                                   (+inject-capf/fn cape-file
;;                                                    cape-dabbrev
;;                                                    +yasnippet-capf-1-prefix)
;;                                   (add-to-list 'completion-at-point-functions #'conventional-commit-capf))) ; Prepend

;; (defun +lsp-completion-at-point-nonexclusive ()
;;   (cape-wrap-nonexclusive #'lsp-completion-at-point))

;; (require 'cl-lib)
;; (add-hook 'lsp-managed-mode-hook
;;           #'(lambda ()
;;               (cl-nsubstitute '+lsp-completion-at-point-nonexclusive
;;                               'lsp-completion-at-point
;;                               completion-at-point-functions)))

(provide 'init-lsp-treesit)
