;;; my-lisp/editor.el --- Emacs as an Editor -*- lexical-binding: t -*-

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

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

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

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

(use-package messages
  :no-require
  :preface
  (provide 'messages)
  :bind ( :map messages-buffer-mode-map
          ("C-c C-k" . messages-clear-buffer))
  :config
  (defun messages-clear-buffer ()
    "Clear the *Messages* buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))


(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

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


(use-package savehist
  :hook (after-init . savehist-mode))

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
(set-locale-environment "en_NZ.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(provide 'editor)
