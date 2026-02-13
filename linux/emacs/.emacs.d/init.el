;;; init.el --- Terminal Emacs Configuration with Vim Keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal Emacs configuration for terminal use with vim keybindings (Evil mode).
;; Takes advantage of native compilation while staying close to default Emacs behavior.
;; This configuration is designed for terminal Emacs only (emacs -nw) with no GUI support.
;;
;; Modules:
;;   core.el       - Performance, UI basics, defaults, clipboard, project
;;   evil-config.el - Evil mode, evil-collection, all keybindings
;;   completion.el  - Vertico, orderless, consult, marginalia, embark, corfu, cape
;;   ui.el          - Theme, modeline, rainbow-delimiters, hl-todo, which-key
;;   git.el         - Magit + leader keys
;;   lsp.el         - Eglot + leader keys
;;   lang.el        - Treesitter, language modes
;;   org-config.el  - Org mode with Evil keybindings

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load modules
(require 'core)
(require 'evil-config)
(require 'completion)
(require 'ui)
(require 'git)
(require 'lsp)
(require 'lang)
(require 'org-config)

;;; init.el ends here
