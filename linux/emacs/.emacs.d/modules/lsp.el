;;; lsp.el --- LSP support via Eglot -*- lexical-binding: t; -*-

;;; Code:

(use-package eglot
  :straight nil
  :hook ((elixir-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)

  ;; Configure Expert for Elixir
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "expert" "--stdio"))

  ;; Performance tuning
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.5))

;; Leader key bindings
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format)
  (evil-define-key 'normal 'global (kbd "<leader>ld") 'xref-find-definitions)
  (evil-define-key 'normal 'global (kbd "<leader>li") 'xref-find-references))

;; Project leader key bindings
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ps") 'project-shell)
  (evil-define-key 'normal 'global (kbd "<leader>pg") 'project-find-regexp))

(provide 'lsp)
;;; lsp.el ends here
