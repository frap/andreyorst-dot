;;; init.el --- Main configuration file -*- lexical-binding: t; coding: utf-8; no-byte-compile: t-*-

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
;; (defun load-directory (dir)
;;   "Load all *.el files in a directory."
;;   (let ((load-it (lambda (f)
;;                    (load-file (concat (file-name-as-directory dir) f)))))
;;     (mapc load-it (directory-files dir nil "\\.el$"))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; compilations, enhance elisp.
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

;; Constants
(require 'init-const)
;; package management
(require 'init-elpa)

;; AutoGC
(defun gas/display-startup-time ()
  (message "Emacs chargé dans %s avec %d ramasse-miettes."
           (format "%.2f secondes"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

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
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)
            ;; startup
            (add-hook 'emacs-startup-hook #'gas/display-startup-time)))

;; load PATH from shell
;; (if (not (getenv "TERM_PROGRAM"))
;;     (setenv "PATH"
;;             (shell-command-to-string "source $HOME/.config/shell/interactive ; printf $PATH")))
;; (setq exec-path (split-string (getenv "PATH") ":"))

(require 'init-bindings)
(require 'init-files-buffers)
(require 'init-ui)

(require 'init-tools)
(require 'init-search)
(require 'init-editor)
(require 'init-text)
(require 'init-org)



(require 'init-personal)

(require 'init-lsp-treesit)
(require 'init-copilot)
(require 'init-coding)
(require 'init-complete)


;; Garbage collection on focus-out, Emacs should feel snappier
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;;; Finish up
(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
