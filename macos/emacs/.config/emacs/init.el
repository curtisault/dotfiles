;;; init.el --- Terminal Emacs Configuration with Vim Keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal Emacs configuration for terminal use with vim keybindings (Evil mode).
;; Takes advantage of native compilation while staying close to default Emacs behavior.
;; This configuration is designed for terminal Emacs only (emacs -nw) with no GUI support.

;;; Code:

;; Performance tweaks for native compilation
(setq native-comp-async-report-warnings-errors nil) ; Silence native comp warnings
(setq native-comp-deferred-compilation t)           ; Compile packages in background

;; Restore GC + file-name-handler-alist after startup completes (deferred in
;; early-init.el).  16 MiB is a sane working threshold; gcmh takes over after.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist splash--file-name-handler-alist)))

;; LSP / subprocess perf — eglot reads big JSON payloads, default 4 KB stalls.
(setq read-process-output-max (* 4 1024 1024))

;; Symlinks (we live in stowed dotfiles — don't prompt every open).
(setq vc-follow-symlinks t)

;; Short answers everywhere (Emacs 28+).
(setq use-short-answers t)

;; Basic UI - clean terminal experience
(setq inhibit-startup-screen t)          ; Skip startup screen
(setq initial-scratch-message nil)       ; Clean scratch buffer
(menu-bar-mode -1)                       ; No menu bar (we're in terminal anyway)

;; GUI cleanup (when not in terminal)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

;; Better defaults
(setq-default
 indent-tabs-mode nil                    ; Use spaces, not tabs
 tab-width 4                             ; Tab width
 fill-column 80                          ; Wrap at 80 characters
 require-final-newline t)                ; Always end files with newline

;; No backup files / lockfiles — we use git.
(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil)

;; Better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Force font-lock on globally.  Default in modern Emacs, but explicit guards
;; against builds / environments that ship with it disabled.
(global-font-lock-mode 1)

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

;; Trim trailing whitespace, but only in code buffers (not in markdown/diff).
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook
                          'delete-trailing-whitespace nil 'local)))

;; History / position persistence
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(savehist-mode 1)
(save-place-mode 1)

;; Enable useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ============================================================================
;; Package Management - straight.el
;; ============================================================================

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

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

;; ============================================================================
;; Evil Mode - Vim Keybindings
;; ============================================================================

(use-package evil
  :init
  ;; Set these before loading evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ; Required for evil-collection
  (setq evil-want-C-u-scroll t)    ; C-u scrolls up
  (setq evil-want-C-d-scroll t)    ; C-d scrolls down
  (setq evil-undo-system 'undo-redo) ; Use built-in undo-redo
  (setq evil-respect-visual-line-mode t)

  ;; Cursor appearance
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor 'box)
  :config
  (evil-mode 1)

  ;; Terminal cursor shape changes (send escape sequences directly).
  ;; Skip in batch/daemon — `display-graphic-p' is nil there too, and we'd
  ;; otherwise spew control codes to stdout.
  (unless (or (display-graphic-p) noninteractive)
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (send-string-to-terminal "\033[6 q")))
    (add-hook 'evil-normal-state-entry-hook
              (lambda () (send-string-to-terminal "\033[2 q")))
    (add-hook 'evil-replace-state-entry-hook
              (lambda () (send-string-to-terminal "\033[4 q")))
    (add-hook 'evil-visual-state-entry-hook
              (lambda () (send-string-to-terminal "\033[2 q"))))

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set leader key in evil (Space)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Keep some Emacs bindings in insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-n") nil)  ; For completion
  (define-key evil-insert-state-map (kbd "C-p") nil)  ; For completion

  ;; Vim-style jump to definition (use xref which works with LSP)
  (define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "C-t") 'xref-go-back)) ; Jump back

;; Configure xref to not prompt when there's only one match
(setq xref-prompt-for-identifier nil)

;; Evil Collection - Evil bindings for many modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ============================================================================
;; Adaptive GC — gcmh raises threshold during work, lowers it on idle
;; ============================================================================

(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1))

;; ============================================================================
;; Clipboard Integration
;; ============================================================================

;; Use system clipboard (matching nvim's clipboard = 'unnamedplus').
;; Modern Emacs handles macOS pbcopy/pbpaste natively via NS bindings in the
;; GUI build, but in `emacs -nw' there is no NS connection — shell out to
;; pbcopy/pbpaste explicitly.
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

(when (and (eq system-type 'darwin) (not (display-graphic-p)))
  (defun my/pbcopy (text &optional _push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (defun my/pbpaste ()
    (shell-command-to-string "pbpaste"))
  (setq interprogram-cut-function   #'my/pbcopy
        interprogram-paste-function #'my/pbpaste))

;; ============================================================================
;; Theme - Catppuccin Frappé
;; ============================================================================

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'frappe)  ; Options: latte, frappe, macchiato, mocha
  (load-theme 'catppuccin :no-confirm))

;; ============================================================================
;; Icons (used by dashboard, completion UIs)
;; ============================================================================

;; First-time setup on a new machine: M-x nerd-icons-install-fonts
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

(load (expand-file-name "dashboard.el" user-emacs-directory))

;; ============================================================================
;; Completion stack (vertico/orderless/consult/marginalia/embark/corfu/cape)
;; Extracted to completion.el
;; ============================================================================

(load (expand-file-name "completion.el" user-emacs-directory))

;; ============================================================================
;; Which-Key - Show keybinding hints
;; ============================================================================

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)  ; Show hints after 0.5s
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom))

;; ============================================================================
;; Git Integration - Magit
;; ============================================================================

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ============================================================================
;; Database - pgmacs
;; ============================================================================

(use-package pgmacs
  :straight (pgmacs :type git :host github :repo "emarsden/pgmacs"))

;; Load sensitive DB config if present (not committed to version control)
(let ((db-config (expand-file-name "tss-db-config.el" user-emacs-directory)))
  (when (file-exists-p db-config)
    (load db-config)))

;; ============================================================================
;; Commenting - evil-nerd-commenter
;; ============================================================================

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
         ("C-c c" . evilnc-copy-and-comment-lines)
         ("C-c p" . evilnc-comment-or-uncomment-paragraphs)))

;; ============================================================================
;; Rainbow Delimiters - Colorized matching brackets
;; ============================================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
;; File Tree - treemacs
;; ============================================================================

(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 35
        treemacs-follow-after-init t
        treemacs-is-never-other-window t)
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

;; ============================================================================
;; Org Mode (extracted to org-config.el)
;; ============================================================================

(load (expand-file-name "org-config.el" user-emacs-directory))

;; ============================================================================
;; Modeline (extracted to modeline.el)
;; ============================================================================

(load (expand-file-name "modeline.el" user-emacs-directory))

;; ============================================================================
;; TODO Highlighting - hl-todo
;; ============================================================================

(use-package hl-todo
  :config
  (global-hl-todo-mode)
  ;; Match your nvim mini.hipatterns keywords
  (setq hl-todo-keyword-faces
        '(("TODO"      . "#7c7d7c")
          ("FIXME"     . "#cc6666")
          ("HACK"      . "#d27b53")
          ("IMPORTANT" . "#d27b53")
          ("NOTE"      . "#67b11d"))))

;; ============================================================================
;; Major Mode Packages (needed for treesit remap to work)
;; ============================================================================

;; elixir-ts-mode and heex-ts-mode are built-in as of Emacs 30.

(use-package lua-mode)
(use-package yaml-mode)

;; Rust mode (provides file detection, then remaps to rust-ts-mode)
(use-package rust-mode)

;; ============================================================================
;; Tree-sitter (extracted to treesit.el)
;; ============================================================================

(load (expand-file-name "treesit.el" user-emacs-directory))

;; ============================================================================
;; LSP / Eglot (extracted to lsp.el)
;; ============================================================================

(load (expand-file-name "lsp.el" user-emacs-directory))

;; ============================================================================
;; Project Management (Built-in)
;; ============================================================================

(use-package project
  :straight nil  ; Built-in to Emacs
  :bind (("C-x p f" . project-find-file)
         ("C-x p s" . project-shell)
         ("C-x p g" . project-find-regexp)
         ("C-x p d" . project-dired))
  :config
  ;; Add project roots (git repos are automatically detected)
  (setq project-vc-extra-root-markers '(".project" ".projectile" "go.mod" "Cargo.toml" "mix.exs")))

;; ============================================================================
;; Misc settings
;; ============================================================================

;; Focus help windows automatically (better discoverability)
(setq help-window-select t)

;; ============================================================================
;; Keybindings (mirrors nvim's remap.lua)
;; ============================================================================

(load (expand-file-name "keybindings.el" user-emacs-directory))

;;; init.el ends here
