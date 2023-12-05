;;; extra.el --- Tweaks for some Additional Function configurations -*- lexical-binding: t; -*-
;;; Created on: 2022 Nov 25

;; Copyright (C) 2021-2022 Likhon Sapiens <likhonhere007@gmail.com>

;; Author: Likhon Sapiens <likhonhere007@gmail.com>
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my tweaks for Org that are meant for use in my
;; Emacs setup: https://github.com/Likhon-baRoy/.emacs.d.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:




;; ──────────────────────────────── Switch Theme ───────────────────────────────
;; (defun switch-theme (theme)
;;   "Disable any currently active themes and load THEME."
;;   ;; This interactive call is taken from `load-theme'
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapc 'symbol-name
;;                                    (custom-available-themes))))))
;;   (let ((enabled-themes custom-enabled-themes))
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (load-theme theme t)))

;; (defun disable-active-themes ()
;;   "Disable any currently active themes listed in `custom-enabled-themes'."
;;   (interactive)
;;   (mapc #'disable-theme custom-enabled-themes))



;; ─────────────────── Added functionality (Generic usecases) ──────────────────
;; Unfill paragraph
;; Might be good. For instance for canceling all of the paragraph quickly or for commenting it away.
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun comment-pretty ()
  "Comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
(insert comment-start))
(insert " ")
(dotimes (_ space-on-each-side) (insert comment-char))
(when (> comment-length 0) (insert " "))
(insert comment)
(when (> comment-length 0) (insert " "))
(dotimes (_ (if (= (% comment-length 2) 0)
                (- space-on-each-side 1)
              space-on-each-side))
  (insert comment-char))))))

;; ─────────────────────────────────── CURSOR ──────────────────────────────────
(set-mouse-color "white")
(blink-cursor-mode 1)
(setq x-stretch-cursor nil) ; make cursor the width of the character it is under i.e. full width of a TAB

(require 'mode-local)

(defun djcb-set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color "yellow")
    (setq cursor-type '(hbar . 3)))
   (overwrite-mode
    (set-cursor-color "red")
    (setq cursor-type 'hollow))
   (t
    (setq cursor-type 'box)
    (set-cursor-color "#ba55d3")
    (setq-mode-local prog-mode cursor-type 'bar)
    )
   ))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
;; (add-hook 'eshell-mode-hook (lambda () (interactive) (setq-local cursor-type '(hbar . 3))))









(ad-activate 'ibuffer)


;; ─────────────────────────────────── Dired ───────────────────────────────────
;; http://whattheemacsd.com/
(require 'dired)
(defun dired-back-to-top ()
  "Step back 3 lines from the very top."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 3))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  "Step up 1 line from the end."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;;;; move-quickly
(defun my-next-lines ()
  "Move to the next 5 lines."
  (interactive)
  (forward-line 5))

(defun my-previous-lines ()
  "Move to the previous 5 lines."
  (interactive)
  (forward-line -5))

(defun my-forward-chars ()
  "Move to the forward 5 chars."
  (interactive)
  (forward-char 5))

(defun my-backward-chars ()
  "Move to the backward 5 chars."
  (interactive)
  (forward-char -5))

(define-key global-map (kbd "C-S-n") #'my-next-lines)
(define-key global-map (kbd "C-S-p") #'my-previous-lines)
(define-key global-map (kbd "C-S-f") #'my-forward-chars)
(define-key global-map (kbd "C-S-b") #'my-backward-chars)

;; (global-set-key (kbd "C-S-n")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-line 5))))

;; (global-set-key (kbd "C-S-p")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-line -5))))

;; (global-set-key (kbd "C-S-f")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-char 5))))

;; (global-set-key (kbd "C-S-b")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (backward-char 5))))

;; ───────────────────────── Show LineNumber Temporary ─────────────────────────
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (forward-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

;; ─────────────────────── Open Any File With LineNumber ───────────────────────
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

;; ───────────────────────────────── Copy line ─────────────────────────────────
;; (defun copy-line (arg)
;;   "Copy lines (as many as prefix argument) in the kill ring.
;;       Ease of use features:
;;       - Move to start of next line.
;;       - Appends the copy on sequential calls.
;;       - Use newline as last char even on the last line of the buffer.
;;       - If region is active, copy its lines."
;;   (interactive "p")
;;   (let ((beg (line-beginning-position))
;;         (end (line-end-position arg)))
;;     (when mark-active
;;       (if (> (point) (mark))
;;           (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
;;         (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
;;     (if (eq last-command 'copy-line)
;;         (kill-append (buffer-substring beg end) (< end beg))
;;       (kill-ring-save beg end)))
;;   (beginning-of-line (or (and arg (1+ arg)) 2))
;;   (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; (global-set-key (kbd "M-k") 'copy-line)

;; ────────────────────────────────── flyspell ─────────────────────────────────
;; (defun flyspell-check-next-highlighted-word ()
;;   "Custom function to spell check next highlighted word."
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word))

;; (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; (eval-after-load "flyspell"
;;   '(progn
;;      (defun flyspell-goto-next-and-popup ( )
;;        "Goto the next spelling error, popup menu, and stop when the end of buffer is reached."
;;        (interactive)
;;        (while (< (point) (point-max))
;;          (flyspell-goto-next-error)
;;          (redisplay)
;;          (flyspell-correct-word-before-point))
;;        (message "No more spelling errors in buffer.")
;;        )
;;      ))
;; (global-set-key (kbd "C-<f8>") 'flyspell-goto-next-and-popup)
;; (define-key flyspell-mode-map (kbd "C-<f8>") 'flyspell-goto-next-and-popup)

;; (defvar @-dotenv-file-name ".env"
;;   "The name of the .env file."
;;   )

;; (defun @-find-env-file ()
;;   "Find the closest .env file in the directory hierarchy."

;;   (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
;;         (file-name (concat env-file-directory @-dotenv-file-name)))
;;     (when (file-exists-p file-name)
;;         file-name))
;;   )

;; (use-package load-env-vars
;;   :ensure t
;;   ;;  :hook ((lsp-mode . #'@-set-project-env)
;;   ;; (project-mode . #'@-set-project-env)
;;   ;; (cider-mode . #'@-set-project-env))
;;   )

;; (defun @-set-project-env ()
;;   "Export all environment variables in the closest .env file."

;;   (let ((env-file (@-find-env-file)))
;;     (when env-file
;;       (load-env-vars env-file)))
;;   )

(provide 'extra)
