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

(provide 'init-const)
;;; init-const.el ends here
