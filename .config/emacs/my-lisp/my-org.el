;;; my-lisp/org.el --- Emacs Org-mode -*- lexical-binding: t -*-
;;; Org

(use-package org
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link)
          ("M-Q" . split-pararagraph-into-lines)
          :map org-src-mode-map
          ("C-x w" . org-edit-src-exit)
          ("C-x C-s" . org-edit-src-exit))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
  :custom
  (org-tags-column -120)
  (org-startup-folded 'content)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-preview-latex-image-directory ".ltximg/")
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (setq
   ;; Default location of org files
   org-directory "~/logseq/"

   ;; Define stages for todo tasks
   org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))

   ;; When item enters DONE, add a CLOSED: property with current date-time stamp
   org-log-done 'time
   ;; Make TODO states easier to distinguish by using different colours
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   hl-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("DOING" . "DarkOrchid")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue"))

   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("DOING" . "DarkOrchid")
     ("BLOCKED" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("ARCHIVED" .  "SlateBlue"))
   )
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

;;(use-package ob-shell
;;:ensure t
;;:after org)

(use-package org-capture
  :straight nil
  :defer t
  ;; :after blog
  :bind ("C-c o c" . org-capture)
  ;;:custom
  ;;(org-directory blog-directory)
  ;; (org-capture-templates
  ;;  `(,blog-capture-template
  ;;    ,@(mapcar
  ;;       (lambda (spec)
  ;;         (seq-let (btn descr heading) spec
  ;;           `( ,btn ,descr entry
  ;;              (file+headline ,(expand-file-name "kb/index.org" blog-directory) ,heading)
  ;;              "* [[blog-html:%^{Link}][%^{Description}]]\n:properties:\n:blog-collapsable: t\n:end:"
  ;;              :immediate-finish t
  ;;              :before-finalize (org-hugo-export-to-md))))
  ;;       '(("a" "Article" "Articles")
  ;;         ("t" "Talk" "Talks")
  ;;         ("w" "Web page" "Various Web pages")
  ;;         ("b" "Books, courses" "Books, Courses")))))
  )

(use-package visual-fill-column
  :ensure t
  :config
  ;; Configure fill width
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(use-package org-present
  :ensure t
  :config
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))

  (defun my/org-present-start ()
    ;; Tweak font sizes
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Set a blank header line string to create blank space at the top
    (setq header-line-format " ")

    ;; Display inline images automatically
    (org-display-inline-images)

    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun my/org-present-end ()
    ;; Reset font customizations
    (setq-local face-remapping-alist '((default variable-pitch default)))

    ;; Clear the header line string so that it isn't displayed
    (setq header-line-format nil)

    ;; Stop displaying inline images
    (org-remove-inline-images)

    ;; Stop centering the document
    (visual-fill-column-mode 0)
    (visual-line-mode 0))

  ;; Turn on variable pitch fonts in Org Mode buffers
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; Register hooks with org-present
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
  )

;; (use-package org-tree-slide
;;   :ensure t
;;   :defer t
;;   :custom
;;   (org-tree-slide-slide-in-effect nil)
;;   (org-tree-slide-never-touch-face t))

;; (use-package org-modern
;;   :ensure t
;;   :defer t
;;   :hook (org-tree-slide-mode . org-modern-mode)
;;   :custom-face
;;   (org-modern-block-name ((t (:height 1.0))))
;;   (org-modern-label ((t (:height 1.0))))
;;   :custom
;;   (org-modern-hide-stars t)
;;   (org-modern-block-fringe nil))

;; (use-package ox-latex
;;   :ensure t
;;   :after ox)

(provide 'my-org)
