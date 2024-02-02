;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Red Elvis
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 29.1+ configuration.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

;; Avoid garbage collection during startup.
(defvar better-gc-cons-threshold 402653184
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;;; Load Path
;; Since all the configuration files are stored in a folder, they need to be added to `load-path' now.
;; (defun update-to-load-path (folder)
;;   "Update FOLDER and its subdirectories to `load-path'."
;;   (let ((base folder))
;;     (unless (member base load-path)
;;       (add-to-list 'load-path base))
;;     (dolist (f (directory-files base))
;;       (let ((name (concat base "/" f)))
;;         (when (and (file-directory-p name)
;;                    (not (equal f ".."))
;;                    (not (equal f ".")))
;;           (unless (member base load-path)
;;             (add-to-list 'load-path name)))))))

;; (update-to-load-path (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.
(setf custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))
;; Separate Customization from init file
;; (setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
;; (unless (file-exists-p custom-file)
;;   (with-temp-buffer
;;     (write-file custom-file)))
;;;; Load custom-files
;; (defun load-directory (dir)
;;   "Load all *.el files in a directory."
;;   (let ((load-it (lambda (f)
;;                    (load-file (concat (file-name-as-directory dir) f)))))
;;     (mapc load-it (directory-files dir nil "\\.el$"))))
;; load-path

;; (when (file-exists-p custom-file)
;;   (load custom-file 'noerror 'nomessage))


(require 'init-const)

;; compilations, enhence elisp.
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

(require 'init-elpa)

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))
            ;; doom normal emacs-startup-hook
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)
            ;; extra minibuffer
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

(defun gas/display-startup-time ()
  (message "Emacs chargé dans %s avec %d ramasse-miettes."
           (format "%.2f secondes"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gas/display-startup-time)


(use-package scratch)
;; Show event history and command history of some or all buffers.
;; (use-package command-log-mode)
;; load PATH from shell
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.config/shell/interactive ; printf $PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

(use-package subr
  :straight nil
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))

(require 'init-ui)
(require 'init-bindings)
(require 'init-files)
(require 'init-text)
(require 'init-org)
(require 'init-tools)
(require 'init-editor)
(require 'init-complete)
(require 'init-search)
(require 'init-coding)
(require 'init-personal)

(require 'init-copilot)


;; (use-package repeat-mode
;;   :hook (after-init . repeat-mode))


;;________________________________________________________________
;;;;    Custom settings
;;________________________________________________________________


;; Garbage collection on focus-out, Emacs should feel snappier
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t))))




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

;;; enable some major-mode
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Finish up
(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
