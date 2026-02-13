;;; git.el --- Git integration -*- lexical-binding: t; -*-

;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Leader key bindings
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status))

(provide 'git)
;;; git.el ends here
