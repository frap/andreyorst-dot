;;; lisp/init-copilot.el --- CoPiloting -*- lexical-binding: t -*-
;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.


(use-package copilot
  :straight
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-disable-predicates '(always))
  :bind
  ("M-`" . copilot-complete)
  :bind
  (:map gas/toggles-map
   ("`" . copilot-mode))
  :bind
  (:map copilot-completion-map
        ("C-g" .  #'copilot-clear-overlay)
        ("M-p" . #'copilot-previous-completion)
        ("M-n" . #'copilot-next-completion)
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("M-f" . #'copilot-accept-completion-by-word)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)
        ("M-<return>" . copilot-accept-completion-by-line))
  :hook (prog-mode . copilot-mode)
  :hook (yaml-mode . copilot-mode)
  :config
  (setq copilot-max-char -1))

(provide 'init-copilot)
;; init-copilot.el ends here
