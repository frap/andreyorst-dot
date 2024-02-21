;;; init-files-buffers.el --- config abount files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ffap, short for “find file at point,” guesses a default file from
;; the point. ffap-bindings rebinds several commands with ffap
;; equivalents. aka smarter C-x C-f when point on path or URL
(use-package ffap
  :straight (:type built-in)
  :init
  ;; Don't ping things that look like domain names.
   (setq ffap-machine-p-known 'reject)
   :hook (on-first-input . ffap-bindings))

(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
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
  :init
  (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__|node_modules")
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (autoload 'dired-omit-mode "dired-x")
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq-default
    dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
    backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
    )
  (setq dired-omit-verbose nil
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
    (setq dired-dwim-target t))

(use-package files
  :straight (:type built-in)
  :preface
  (setq
   ;; more info in completions
   completions-detailed t
   ;; don't let the minibuffer muck up my window tiling
   read-minibuffer-restore-windows t
   ;; scope save prompts to individual projects
   save-some-buffers-default-predicate 'save-some-buffers-root
    ;;Emacs is super fond of littering filesystems with backups and autosaves. This was valid in 1980. It is no longer the case
   make-backup-files nil
   auto-save-default nil
   create-lockfiles nil )
  (setq-default
   x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
   save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
   apropos-do-all t                  ; Shows all options when running apropos
   message-log-max 1000
   fill-column 80

   column-number-mode t              ; show (line,column) in mode-line.
   cua-selection-mode t              ; delete regions.
   ;; allow commands to be run on minibuffers.
   enable-recursive-minibuffers t   )
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  ;; :custom
  ;; (backup-by-copying t)
  ;; (create-lockfiles nil)
  ;; (backup-directory-alist
  ;;  `(("." . ,backup-dir)))
  ;; (auto-save-file-name-transforms
  ;;  `((".*" ,auto-save-dir t)))
  ;; (auto-save-no-message t)
  ;; (auto-save-interval 100)
  ;; (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-revert-interval 0.5
     ;; Revert buffers like Dired
        global-auto-revert-non-file-buffers t
        ;; Don't ask when reverting
        auto-revert-verbose nil))

;; Use human readable Size column instead of original one
(eval-after-load 'ibuffer
  '(progn
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))))

(use-package ibuffer
  :straight (:type built-in)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-n" . nil)
        ;; ("M-o" . nil)
         ("M-p" . nil))
;;  :delight
  :config
  ;; modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Don't ask for confirmation to delete marked buffers
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (name . "\*mu4e\*"))
               ("coding" (or
                               (mode . python-mode)
                               (mode . clojure-mode)
                               (name . "^\\*scratch-clj\\*$")))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))

  ;; Switching to ibuffer puts the cursor on the most recent buffer
 (define-advice ibuffer
     (:around ibuffer-point-to-most-recent) ()
     "Open ibuffer with cursor pointed to most recent buffer name.
   This advice sets the cursor position to the name of the most recently
   visited buffer when ibuffer is called. This makes it easier to quickly
   switch back to a recent buffer without having to search for it in the list."
     (let ((recent-buffer-name (buffer-name)))
       ad-do-it
       (ibuffer-jump-to-buffer recent-buffer-name)))
 ;; ─────────────────────── Delete current file and buffer ──────────────────────
 ;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
 (defun delete-current-file-and-buffer ()
   "Kill the current buffer and deletes the file it is visiting."
   (interactive)
   (let ((filename (buffer-file-name)))
     (if filename
         (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
             (progn
               (delete-file filename)
               (message "Deleted file %s." filename)
               (kill-buffer)))
       (message "Not a file visiting buffer!"))))
 (setq ibuffer-default-sorting-mode 'recency)
 (add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default"))))

;; Show event history and command history of some or all buffers.
(use-package command-log-mode)

;; what does scratch do?
;;(use-package scratch)

;;;; remove old backup files
;; Automatically purge backup files not accessed in a week:
;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

(provide 'init-files-buffers)
;;; init-files.el ends here
