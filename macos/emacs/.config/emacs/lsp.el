;;; lsp.el --- LSP / Eglot configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot (built-in LSP client) configuration.
;; `read-process-output-max' is set globally in init.el for transport perf.

;;; Code:

(use-package eglot
  :straight nil  ; Built-in to Emacs 29+
  :hook ((elixir-ts-mode     . eglot-ensure)
         (python-ts-mode     . eglot-ensure)
         (lua-mode           . eglot-ensure)
         (js-ts-mode         . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-ts-mode       . eglot-ensure))
  :config
  (setq eglot-autoshutdown t                   ; Shutdown server when last buffer is killed
        eglot-sync-connect nil                 ; Don't block on connection
        eglot-events-buffer-size 0             ; Disable events buffer for performance
        eglot-send-changes-idle-time 0.5
        eglot-extend-to-xref t)                ; Cross-workspace xref
  ;; Configure Expert for Elixir
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "expert" "--stdio")))

;;; lsp.el ends here
