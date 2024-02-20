;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

(defconst IS-GUI?     (display-graphic-p))
(defconst IS-MAC?     (eq system-type 'darwin)
  "Are we runnig on a Mac system?")
(defconst IS-LINIX?   (eq system-type 'gnu/linux)
  "are we running on a GNU/Linux system?")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(setq
 ;; Reduce debug output, well, unless we've asked for it.
 debug-on-error init-file-debug     
 jka-compr-verbose init-file-debug
 read-process-output-max (* 64 1024) ; 64kb
)

(provide 'init-const)
;;; init-const.el ends here
