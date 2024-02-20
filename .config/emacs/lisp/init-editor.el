;;; lisp/init-editor.el --- Emacs Editor -*- lexical-binding: t -*-

(use-package defaults
  :straight nil
  :no-require
  :preface
  (setq
   ;; my source directory
   default-directory "~/work/"
   ;; search should be case-sensitive by default
   case-fold-search nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Save existing clipboard text into the kill ring before replacing it.
   save-interprogram-paste-before-kill t
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; prefer newer elisp files
   load-prefer-newer t
   ;; when I say to quit, I mean quit
   confirm-kill-processes nil
   ;; unicode ellipses are better
   truncate-string-ellipsis "…"
   ;; I want to close these fast, so switch to it so I can just hit 'q'
   help-window-select t
   ;; this certainly can't hurt anything
   delete-by-moving-to-trash t
   ;; more info in completions
   completions-detailed t
   ;; don't keep duplicate entries in kill ring
   kill-do-not-save-duplicates t
   ;; select help window when opened
   help-window-select t
   redisplay-skip-fontification-on-input t
   ;;  tab-always-indent 'complete        ; smart tab behavior - indent or complete.
   ;; Flash the screen on error, don't beep.
   visible-bell t
   ;; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).)
   ;;  view-read-only t
   ;; don't automatically add new line, when scroll down at the bottom of a buffer.
   next-line-add-newlines nil
   ;; require final new line.
   require-final-newline t
   ;; highlight the stuff you are marking.
   transient-mark-mode t
   ;; Show Keystrokes in Progress Instantly
   echo-keystrokes 0.1

   save-place-forget-unreadable-files nil

   blink-matching-paren t             ; Blinking parenthesis.
  )
  ;; Never mix tabs and spaces. Never use tabs, period.
  ;; We need the setq-default here because this becomes
  ;; a buffer-local variable when set.
  (setq-default indent-tabs-mode nil
                tab-always-indent 'complete
                completion-cycle-threshold nil)

  ;; use UTF-8 period!
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)
  ;;We also need to turn on a few modes to have behaviour that's even remotely modern.
  ;; Typing over an active section should delete the section.
  (delete-selection-mode t)
  ;;(global-display-line-numbers-mode t)
  (column-number-mode)
  (savehist-mode)
  ;; Emacs 27 comes with fast current-line highlight functionality, but it can produce some visual feedback in ~vterm~ buffers
  (require 'hl-line)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)
   ;; excellent way to cause aggravation when the variable you keep trying to modify is being set in some ~custom-set-variables~ invocation
   (setq custom-file (make-temp-name "/tmp/"))
   ;;  Emacs stores theme-safety information in that file, we have to disable the warnings entirely
   (setq custom-safe-themes t))


;;________________________________________________________________
;;;;    Custom settings
;;________________________________________________________________
;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.

;; (setf custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (and custom-file
;;            (file-exists-p custom-file))
;;   (load custom-file nil :nomessage))

;; Separate Customisation from init file
;; (setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
;; (unless (file-exists-p custom-file)
;;   (with-temp-buffer
;;     (write-file custom-file)))
;;;; Load custom-files
;; (when (file-exists-p custom-file)
;;   (load custom-file 'noerror 'nomessage))

(use-package breadcrumb
  ;; disable breadcrumb when using lsp-mode, because lsp have this feature already.
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

;;  delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organisation and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

;;; Messaging

;; (use-package message-view-patch
;;   :ensure t
;;   :hook (gnus-part-display . message-view-patch-highlight))

;; multi cursor
(defun /mc/bind nil
  (bind-keys
   :map mc/keymap
   ("s-u" . mc/unmark-next-like-this)
   ("s-D" . mc/mark-previous-like-this)
   ("s-U" . mc/unmark-previous-like-this)
   ("s-F" . mc/mark-next-like-this)
   ("<down-mouse-1>" . mc/keyboard-quit)
   ("<mouse-1>" . mc/keyboard-quit)
   ("s-k" . mc/skip-to-next-like-this)
   ("s-K" . mc/skip-to-previous-like-this)
   ("s-n" . mc/mark-next-like-this)
   ("s-p" . mc/mark-previous-like-this)
   ("<RET>" . newline)
   ("<return>" . newline)
   ;; ("<return>" . nil)
   ("C-&" . mc/vertical-align-with-space)
   ("C-#" . mc/insert-numbers)
   :prefix-map chee/multiple-cursors-map
   :prefix "s-m"
   ("S" . mc/mark-next-symbol-like-this)
   ("s" . mc/mark-all-symbols-like-this)
   ("W" . mc/mark-next-word-like-this)
   ("w" . mc/mark-all-words-like-this)
   ("d" . mc/mark-all-words-like-this-in-defun)
   ("<down-mouse-1>" . mc/keyboard-quit))
  (remove-hook 'multiple-cursors-mode-hook '/mc/bind))

(use-package multiple-cursors
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   ("s-d" . mc/mark-next-like-this)
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
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (add-hook 'multiple-cursors-mode-hook '/mc/bind))

(use-package page
  :straight nil
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

(use-package rect
  :straight nil
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :preface
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

;; (use-package repeat-mode
;;   :hook (after-init . repeat-mode))

(use-package undo-tree
  :commands undo-tree-undo undo-tree-redo
  :config (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist '(("." . "~/config/undo")))
  :bind
  (("C-_" . 'undo-tree-undo)
   ("M-_" . 'undo-tree-undo)
   ("s-z" . 'undo-tree-undo)
   ("s-Z" . 'undo-tree-redo)
   ("s-y" . 'undo-tree-redo)))

(use-package vundo
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  ;;(vundo--window-max-height 20)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)
  (global-set-key (kbd "C-x u") #'vundo))

(use-package writeroom-mode)

;;(use-package polymode)

;; make sure some stuff not diabled
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)


(provide 'init-editor)
;;; init-editor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
