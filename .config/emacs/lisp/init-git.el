;;; lisp/init-git.el --- Git &  -*- lexical-binding: t -*-
;; Magit
;; I have known people to leave Emacs, but continuing to use Magit for
;; version control. It’s that good.
;;
;;when I do a git-pull I'd like to see what's new
(global-auto-revert-mode t)

;; I am giving built-ins the benefit of the doubt in this config, and
;; would like to get into vc-mode. But I’m an advanced enough Git user
;; that something tailor-made carries its weight here.
(use-package magit
  :defer 1
  :functions gas/magit-clone-read-args-a
  :bind
  ("C-x g" . magit-status)
  :mode (("COMMIT_EDITMSG" . git-commit-mode))
  :custom
  (magit-clone-default-directory "~/src/")
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers 'dontask)
  :config
  (defun gas/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'gas/magit-clone-read-args-a))

;; show todos
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; Git-Link
;; git-link grabs links to lines, regions, commits, or home pages.
(use-package git-link
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

(use-package git-timemachine)

(provide 'init-git)
