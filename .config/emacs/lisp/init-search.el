;;; lisp/init-search.el --- Search -*- lexical-binding: t -*-


(use-package isearch
  :straight nil
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package phi-search
  :defer t)


;; better search in buffer
(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))


(provide 'init-search)
