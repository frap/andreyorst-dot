;;; early-init.el --- Early initialisation -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Gas
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 29+ early initialisation configuration.

;;; Code:


;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq
 package-enable-at-startup nil ; don't auto-initialise!
 package--init-file-ensured t ; don't add that `custom-set-variables' block to init
 package-quickstart nil ; prevent `package.el' loading packages prior to their init-file
 package-archives nil)

(setq
 gc-cons-threshold most-positive-fixnum  ; 2^61 bytes
 gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq restore-file-handler-alist 101
                  gc-con-threshold 33554432 ; 32 mb
                  gc-cons-percentage 0.1)))

(setq safe-local-variable-values
      '((org-src-preserve-indentation . t)
        (eval add-hook 'after-save-hook
              '(lambda nil
                 (org-babel-tangle))
              nil t)))

(setq
 site-run-file nil ; unset SRF
 use-file-dialog nil
 mode-line-format nil ; don't want a mode line while loading init
 load-prefer-newer nil
 create-lockfiles nil ; disable lockfiles
 make-backup-files nil ; disable backup files
 auto-save-list-file-prefix nil ; disable auto-save
 auto-mode-case-fold nil ; use case-sensitive `auto-mode-alist' for performance
 default-input-method nil
 utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
 initial-scratch-message nil ; empty the initial *scratch* buffer.
 command-line-x-option-alist nil ; remove irreleant command line options for faster startup
 use-short-answers t ; y/n for yes/no
 vc-follow-symlinks t ; Do not ask about symlink following
 inhibit-default-init t
 inhibit-splash-screen t
 inhibit-startup-screen t 		; do not show the startup message
 inhibit-startup-message t      ; reduce noise at startup
 inhibit-startup-buffer-menu t  ; stop `list-buffers' from showing when opening multiple files
 fast-but-imprecise-scrolling t ; more performant rapid scrolling over unfontified regions
 frame-inhibit-implied-resize t ; do not resize the frame at this early stage
 ffap-machine-p-known 'reject   ; don't ping things that look like domain names
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 idle-update-delay 1.0 ; slow down UI updates down
 select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
 ad-redefinition-action 'accept ; disable warnings from legacy advice system
 initial-major-mode 'fundamental-mode
 inhibit-startup-echo-area-message user-login-name)

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) initial-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
;; (setq comp-deferred-compilation nil)
(when (featurep 'native-compile)
  (defvar inhibit-automatic-native-compilation)
  (setq inhibit-automatic-native-compilation nil)
  (setq native-comp-deferred-compilation nil)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))


;; (setq-default
;;  default-frame-alist '((width . 170)
;;                        (height . 56)
;;                        (tool-bar-lines . 0)
;;                        (bottom-divider-width . 0)
;;                        (right-divider-width . 1))
;;  initial-frame-alist default-frame-alist
;;  frame-inhibit-implied-resize t
;;  x-gtk-resize-child-frames 'resize-mode
;;  fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(unless (or (daemonp) noninteractive)
  (let ((restore-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun restore-file-handler-alist ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 restore-file-name-handler-alist)))))


(defun edit-init-file ()
  "Edit `user-init-file'.
With prefix argument promtps to select a file from all Emacs Lisp
in `user-emacs-directory'."
  (interactive)
  (if current-prefix-arg
      (find-file
       (expand-file-name
        (completing-read
         "file"
         (directory-files user-emacs-directory nil "^[^.].*.el$"))
        user-emacs-directory))
    (find-file (expand-file-name "init.el" user-emacs-directory))))

(provide 'early-init)
;;; early-init.el ends here
