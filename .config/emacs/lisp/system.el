
;;; Setup
(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

;;;; Dired
(require 'dired)
(setq dired-listing-switches "-agho --group-directories-first"
      dired-omit-files "^\\.[^.].*"
      dired-omit-verbose nil
      dired-dwim-target t ; Copy and move files netween dired buffers
      dired-recursive-copies 'always ; "always" means no asking
      dired-recursive-deletes 'top   ; "top" means ask once for top level directory
      dired-ls-F-marks-symlinks t ; -F marks links with @
      dired-hide-details-hide-symlink-targets nil
      auto-save-list-file-prefix nil ; not create directory .emacs.d/auto-save-list
      ;; Auto refresh dired, but be quiet about it
      global-auto-revert-non-file-buffers t
      wdired-allow-to-change-permissions t
      auto-revert-verbose nil
      auto-revert-interval 1
      delete-by-moving-to-trash t)

(autoload 'dired-omit-mode "dired-x")

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (hl-line-mode 1)))
(define-key dired-mode-map "z" #'dired-omit-mode)
(define-key dired-mode-map "l" #'dired-up-directory)
(bind-keys :map dired-mode-map
           ("/" . dired-goto-file)
           ("," . dired-create-directory)
           ("." . dired-create-empty-file)
           ;; ("I" . dired-insert-subdir)
           ("K" . dired-kill-subdir)
           ;; ("O" . dired-find-file-other-window)
           ("[" . dired-prev-dirline)
           ("]" . dired-next-dirline)
           ;; ("^" . mode-line-other-buffer)
           ("x" . dired-do-delete)
           ("X" . dired-do-flagged-delete)
           ("y" . dired-do-copy))

;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/sxiv") ;or /usr/bin/gimp
;; (setq image-dired-marking-shows-next nil)
(setq image-dired-thumb-size 256)
;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c x") 'image-dired)
  (define-key dired-mode-map (kbd "M-<return>") 'image-dired-dired-display-external))

;; (use-package dired
;;   :bind ( :map dired-mode-map
;;           ("<backspace>" . dired-up-directory)
;;           ("M-<up>" . dired-up-directory)
;;           ("~" . dired-home-directory))
;;   :hook (dired-mode . dired-hide-details-mode)
;;   :custom
;;   (dired-listing-switches "-lAXhv --group-directories-first")
;;   :config
;;   (defun dired-home-directory ()
;;     (interactive)
;;     (dired (expand-file-name "~/"))))

;; github.com/doomemacs/doomemacs/blob/develop/core/core.el#L296
(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq
   gcmh-idle-delay 'auto ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :delight " Ⓖ")

(use-package novice
  :preface
  (defvar disabled-commands (locate-user-emacs-file "disabled.el")
    "File to store disabled commands, that were enabled permanently.")
  :config
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  (load disabled-commands 'noerror))


;;;; uniquify-files
(use-package uniquify-files
  :ensure t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(provide 'system)
