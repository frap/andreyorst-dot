;;; init-files.el --- config abount files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; ffap, short for “find file at point,” guesses a default file from
;; the point. ffap-bindings rebinds several commands with ffap
;; equivalents.
(use-package ffap
  ;; find-file-at-point, smarter C-x C-f when point on path or URL
  :hook (on-first-input . ffap-bindings))


(provide 'init-files)
;;; init-files.el ends here
