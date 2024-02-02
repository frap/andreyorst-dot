;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1)
(setq straight-vc-git-auto-fast-forward t)
;; (setq straight-vc-git-default-protocol "ssh")
;;; Core packages
(straight-use-package 'use-package)
;; load org early
(straight-use-package 'org)
(setq straight-use-package-by-default t)
(require 'use-package)

;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))


;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

;; utility hooks and functions from Doom Emacs
(use-package on
  :straight (on :type git :repo "ajgrf/on.el" :host github))
;; We also want to “delight” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
(use-package delight
  :doc "A feature that removes certain minor-modes from mode-line.
"
  :delight)
(delight '((abbrev-mode " Abv" abbrev)
           (auto-fill-function " AF")
           (visual-line-mode)
           (smart-tab-mode " \\t" smart-tab)
           (eldoc-mode nil "eldoc")
           (rainbow-mode)
           (clojure-mode "clj")
           (overwrite-mode " Ov" t)
           (emacs-lisp-mode "Ɛlisp" :major)))

;; Benchmark
;; benchmark-init is a simple package that may or may not carry its
;; weight versus usepackage-compute-statistics. Run
;; benchmark-init/show-durations-tabulated to check this one out.
(use-package benchmark-init
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	no-littering-var-directory "~/.cache/emacs/var/"))

(use-package recentf
  :straight nil
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))
    (add-to-list 'recentf-exclude
	     (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
	         (recentf-expand-file-name no-littering-etc-directory))))

(setq create-lockfiles nil)
(setq warning-minimum-level :error)

(provide 'init-elpa)
;;; init-elpa.el ends here
