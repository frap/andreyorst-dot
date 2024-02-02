;;; lisp/init-editor.el --- Emacs Editor -*- lexical-binding: t -*-

(setq default-directory "~/")

(use-package breadcrumb
  ;; disable breadcrumb whel using lsp-mode, because lsp have this feature already.
  :hook (lsp-mode . (lambda () (breadcrumb-mode 0)))
  :config
  (breadcrumb-imenu-crumbs)
  (breadcrumb-mode))

;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :hook (on-first-buffer . save-place-mode))

;; auto-saving changed files
(use-package super-save
  :defer 1
  :delight
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Subword mode helps us move around camel-case languages, and is
;; mostly configured as a hook in those major modes. The only thing we
;; customize about it is not wanting it cluttering the mode line.
(use-package subword
  :defer t
  :delight)

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; Backups are placed into your Emacs directory, e.g. xxxx/backups
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Delete Selection Mode #
;; Typing over an active section should delete the section.
(use-package delsel
  :defer t
  :custom
  (delete-selection-mode))

;; set-mark-command-repeat-pop means we only need to hit C-u or C-x
;; once before subsequent C-SPC, which makes it much nicer to
;; navigate.
(setopt set-mark-command-repeat-pop t)

;; indent

;; Tabs are the devil’s whitespace.
;; Killing
;; Put the clipboard on the kill ring before killing something
;; else. Emacs isn’t as violent as it sometimes sounds, I swear.
;;
;; We also don’t want to clutter the ring with consecutively duplicate
;; values.
(setf save-interprogram-paste-before-kill t)
(setf kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)

;; editorconfig for emacs
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; multi cursor
(use-package multiple-cursors
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
   ("l" . mc/edit-ends-of-lines))
  :config
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package multiple-cursors-core
  :straight nil
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

(use-package vundo
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  ;;(vundo--window-max-height 20)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)
  (global-set-key (kbd "C-x u") #'vundo))

;; window selection with ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

(winner-mode +1)

(use-package writeroom-mode)

;;(use-package polymode)

(provide 'init-editor)
