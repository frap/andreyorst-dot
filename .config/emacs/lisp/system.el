

;;;; Dired
(require 'dired)


(autoload 'dired-omit-mode "dired-x")

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (hl-line-mode 1)))
(define-key dired-mode-map "z" #'dired-omit-mode)
(define-key dired-mode-map "l" #'dired-up-directory)


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
