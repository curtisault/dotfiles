;;; core.el --- Core settings -*- lexical-binding: t; -*-

;;; Code:

;; Performance tweaks for native compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

;; Basic UI - clean terminal experience
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)

;; Better defaults
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 require-final-newline t)

;; Backup and autosave files - keep them out of the way
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; Make directories if they don't exist
(make-directory (concat user-emacs-directory "backups") t)
(make-directory (concat user-emacs-directory "autosaves") t)

;; Better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Line numbers - relative in programming modes (matching nvim)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Highlight current line (matching nvim's cursorline)
(global-hl-line-mode 1)

;; Scrolloff - keep cursor away from edges (matching nvim's scrolloff=8)
(setq scroll-margin 8
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; No line wrapping by default (matching nvim's nowrap)
(setq-default truncate-lines t)

;; Better split behavior (matching nvim's splitright and splitbelow)
(setq split-width-threshold 160
      split-height-threshold nil)

;; Case-insensitive search (matching nvim's ignorecase)
(setq case-fold-search t)

;; Disable swap files (matching nvim's noswapfile)
(setq create-lockfiles nil)

;; Better whitespace handling
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Clipboard integration (matching nvim's clipboard = 'unnamedplus')
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

(when (eq system-type 'darwin)
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "pbpaste"))))

;; Better discoverability
(setq help-window-select t)

;; Better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Project management (built-in)
(use-package project
  :straight nil
  :bind (("C-x p f" . project-find-file)
         ("C-x p s" . project-shell)
         ("C-x p g" . project-find-regexp)
         ("C-x p d" . project-dired))
  :config
  (setq project-vc-extra-root-markers '(".project" ".projectile" "go.mod" "Cargo.toml" "mix.exs")))

(provide 'core)
;;; core.el ends here
