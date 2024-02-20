;;; lisp/init-text.el --- Emacs wit Editor -*- lexical-binding: t -*-

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; DWIM case
;; These do-what-I-mean bindings are newer than the classic
;; keybindings, but a better default.
(use-package emacs
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :bind
  (("C-'" . 'avy-goto-char)
   ("C-:" . 'avy-goto-char-2)
   ;; ("M-" . 'avy-copy-line)
   ;; ("M-" . 'avy-copy-region)
   ;; ("C-c C-j" . 'avy-resume)
   (:map gas/goto
         ("c" . #'avy-goto-char)
         ("j" . #'avy-goto-word-0)
         ("e" . #'avy-goto-word-0)
         ("w" . #'avy-goto-word-1)
         ("l" . #'avy-goto-line)
         ("r" . 'avy-move-region)))

  :config
   (setq avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode exwm-mode))
   :custom
   (avy-timeout-seconds 0.5)
   (avy-style 'pre))

(use-package expand-region
  :bind ("C-c C-=" . er/expand-region)
        ("C-c C--" . er/contract-region))

(use-package flyspell
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

; Jinx is a just-in-time spell checker.
(use-package jinx
  :delight
  ;; I don't want it anywhere except I really want.
  ;; :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map ltl/toggles-map
   ("$" . jinx-mode)))

;; (use-package messages
;;   :straight nil
;;   :no-require
;;   :preface
;;   (provide 'messages)
;;   :bind ( :map messages-buffer-mode-map
;;           ("C-c C-k" . messages-clear-buffer))
;;   :config
;;   (defun messages-clear-buffer ()
;;     "Clear the *Messages* buffer."
;;     (interactive)
;;     (let ((inhibit-read-only t))
;;       (delete-region (point-min) (point-max)))))

(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

;; (use-package select
;;   :straight nil
;;   :no-require
;;   :when (display-graphic-p)
;;   :custom
;;   (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package simple
  :straight nil
  :bind ( ;;("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
      ;;   ("C-h C-f" . describe-face)
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
  (shell-command-default-error-buffer "*Erreurs du Shell de Commande*")
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

(provide 'init-text)
