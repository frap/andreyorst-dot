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

(use-package ob-shell :after org)

(use-package org-capture
  :defer t
  :after blog
  :bind ("C-c o c" . org-capture)
  :custom
  (org-directory blog-directory)
  (org-capture-templates
   `(,blog-capture-template
     ,@(mapcar
        (lambda (spec)
          (seq-let (btn descr heading) spec
            `( ,btn ,descr entry
               (file+headline ,(expand-file-name "kb/index.org" blog-directory) ,heading)
               "* [[blog-html:%^{Link}][%^{Description}]]\n:properties:\n:blog-collapsable: t\n:end:"
               :immediate-finish t
               :before-finalize (org-hugo-export-to-md))))
        '(("a" "Article" "Articles")
          ("t" "Talk" "Talks")
          ("w" "Web page" "Various Web pages")
          ("b" "Books, courses" "Books, Courses"))))))

(use-package org-tree-slide
  :ensure t
  :defer t
  :custom
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-never-touch-face t))

(use-package org-modern
  :ensure t
  :defer t
  :hook (org-tree-slide-mode . org-modern-mode)
  :custom-face
  (org-modern-block-name ((t (:height 1.0))))
  (org-modern-label ((t (:height 1.0))))
  :custom
  (org-modern-hide-stars t)
  (org-modern-block-fringe nil))

(use-package ox-latex
  :after ox)
