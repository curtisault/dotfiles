;;; init.el --- Minimal Vanilla Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal, vanilla Emacs configuration that takes advantage of native compilation
;; while staying close to default Emacs behavior.

;;; Code:

;; Performance tweaks for native compilation
(setq native-comp-async-report-warnings-errors nil) ; Silence native comp warnings
(setq native-comp-deferred-compilation t)           ; Compile packages in background

;; Basic UI - clean terminal experience
(setq inhibit-startup-screen t)          ; Skip startup screen
(setq initial-scratch-message nil)       ; Clean scratch buffer
(menu-bar-mode -1)                       ; No menu bar (we're in terminal anyway)

;; Better defaults
(setq-default
 indent-tabs-mode nil                    ; Use spaces, not tabs
 tab-width 4                             ; Tab width
 fill-column 80                          ; Wrap at 80 characters
 require-final-newline t)                ; Always end files with newline

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

  ;; Terminal cursor shape changes (send escape sequences directly)
  (unless (display-graphic-p)
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
;; Clipboard Integration
;; ============================================================================

;; Use system clipboard (matching nvim's clipboard = 'unnamedplus')
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; On macOS, ensure proper clipboard integration
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

;; ============================================================================
;; Theme - Catppuccin Frappé
;; ============================================================================

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'frappe)  ; Options: latte, frappe, macchiato, mocha
  (load-theme 'catppuccin :no-confirm))

;; ============================================================================
;; Fuzzy Finding - Vertico + Consult + Orderless
;; ============================================================================

;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :config
  (setq vertico-cycle t))  ; Cycle through candidates

;; Orderless - flexible matching
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Consult - enhanced commands
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :config
  ;; Use ripgrep for grep commands
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

;; Marginalia - add annotations to completions
(use-package marginalia
  :init
  (marginalia-mode))

;; Embark - actions on completion candidates
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
;; Org Mode with Evil
;; ============================================================================

(use-package org
  :straight nil  ; Built-in
  :config
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'content)
  (setq org-log-done 'time)

  ;; Essential: TAB cycling (fixes Evil conflict)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "S-TAB") 'org-shifttab)
  (evil-define-key 'normal org-mode-map (kbd "<backtab>") 'org-shifttab)

  ;; Common Org operations in normal mode
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "T") 'org-insert-todo-heading)
  (evil-define-key 'normal org-mode-map (kbd ",") 'org-priority)
  (evil-define-key 'normal org-mode-map (kbd "o") 'org-open-below)
  (evil-define-key 'normal org-mode-map (kbd "O") 'org-open-above)

  ;; Structure editing with Meta + hjkl (Vim-style)
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
  (evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

  ;; Leader key shortcuts for Org mode
  (evil-define-key 'normal org-mode-map (kbd "<leader>ot") 'org-time-stamp)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oT") 'org-time-stamp-inactive)
  (evil-define-key 'normal org-mode-map (kbd "<leader>os") 'org-schedule)
  (evil-define-key 'normal org-mode-map (kbd "<leader>od") 'org-deadline)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oq") 'org-set-tags-command)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oa") 'org-archive-subtree)
  (evil-define-key 'normal org-mode-map (kbd "<leader>or") 'org-refile)
  (evil-define-key 'normal org-mode-map (kbd "<leader>ol") 'org-insert-link)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oe") 'org-export-dispatch)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oc") 'org-toggle-checkbox)
  (evil-define-key 'normal org-mode-map (kbd "<leader>oi") 'org-toggle-inline-images))

;; Evil bindings for Org mode are also provided by evil-collection

;; ============================================================================
;; Modeline - Telephone Line (terminal-friendly)
;; ============================================================================

(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-abs-left
        telephone-line-secondary-left-separator 'telephone-line-abs-hollow-left
        telephone-line-primary-right-separator 'telephone-line-abs-right
        telephone-line-secondary-right-separator 'telephone-line-abs-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

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

;; Elixir with tree-sitter support
(use-package elixir-ts-mode
  :straight (elixir-ts-mode :type git :host github :repo "wkirschbaum/elixir-ts-mode"))

(use-package lua-mode)
(use-package yaml-mode)

;; Rust mode (provides file detection, then remaps to rust-ts-mode)
(use-package rust-mode)

;; ============================================================================
;; Treesitter Support (Built-in Emacs 29+)
;; ============================================================================

;; Configure treesit auto mode mappings
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Helper function to install all grammars
(defun install-treesit-grammars ()
  "Install all tree-sitter grammars from treesit-language-source-alist."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((lang-name (car lang)))
      (unless (treesit-language-available-p lang-name)
        (message "Installing tree-sitter grammar for %s..." lang-name)
        (treesit-install-language-grammar lang-name)))))

;; Auto-install tree-sitter grammars when first opening a file
(defun auto-install-treesit-grammar ()
  "Automatically install tree-sitter grammar if missing."
  (when (and (treesit-available-p)
             (treesit-language-at (point))
             (not (treesit-language-available-p (treesit-language-at (point)))))
    (let ((lang (treesit-language-at (point))))
      (message "Installing tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang)
      (message "Grammar installed. Please reopen the file.")
      ;; Suggest reopening
      (when (y-or-n-p "Grammar installed. Reopen file now? ")
        (revert-buffer t t)))))

;; Add hook to auto-install grammars
(add-hook 'find-file-hook
          (lambda ()
            (when (and (derived-mode-p 'prog-mode)
                       (string-match-p "-ts-mode$" (symbol-name major-mode)))
              (run-with-timer 0.5 nil #'auto-install-treesit-grammar))))

;; Automatically use treesit modes when available
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; Note: elixir-ts-mode registers itself for .ex/.exs files directly
;; Note: rust-ts-mode is built-in and registers itself for .rs files

;; ============================================================================
;; LSP Support - Eglot (Built-in Emacs 29+)
;; ============================================================================

(use-package eglot
  :straight nil  ; Built-in to Emacs 29+
  :hook ((elixir-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)  ; Shutdown server when last buffer is killed
  (setq eglot-sync-connect nil)  ; Don't block on connection

  ;; Configure Expert for Elixir
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "expert" "--stdio"))

  ;; Performance tuning
  (setq eglot-events-buffer-size 0)  ; Disable events buffer for performance
  (setq eglot-send-changes-idle-time 0.5))

;; ============================================================================
;; Completion - Corfu
;; ============================================================================

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous`
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)         ;; Delay before auto completion
  (corfu-auto-prefix 2)          ;; Minimum prefix length for auto completion
  (corfu-quit-no-match 'separator) ;; Don't quit if there's no match
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :init
  ;; Add completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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
;; Leader Key Setup (SPC as leader, using Evil)
;; ============================================================================

;; Normal mode - clear highlights (matching nvim)
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)

;; Diagnostics (matching nvim's <leader>dt)
(evil-define-key 'normal 'global (kbd "<leader>dt") 'consult-flymake)

;; Window navigation with C-hjkl (matching nvim)
(evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)

;; Window resize with Shift+arrows (matching nvim)
(evil-define-key 'normal 'global (kbd "<S-up>") 'shrink-window)
(evil-define-key 'normal 'global (kbd "<S-down>") 'enlarge-window)
(evil-define-key 'normal 'global (kbd "<S-left>") 'shrink-window-horizontally)
(evil-define-key 'normal 'global (kbd "<S-right>") 'enlarge-window-horizontally)

;; Visual mode - keep selection after indent (matching nvim)
(evil-define-key 'visual 'global (kbd "<") (lambda () (interactive) (evil-shift-left (region-beginning) (region-end)) (evil-normal-state) (evil-visual-restore)))
(evil-define-key 'visual 'global (kbd ">") (lambda () (interactive) (evil-shift-right (region-beginning) (region-end)) (evil-normal-state) (evil-visual-restore)))

;; File operations
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)

;; Git operations
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)

;; Project operations
(evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ps") 'project-shell)
(evil-define-key 'normal 'global (kbd "<leader>pg") 'project-find-regexp)

;; LSP operations
(evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'xref-find-definitions)
(evil-define-key 'normal 'global (kbd "<leader>li") 'xref-find-references)

;; Window operations (matching nvim)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-vsplit)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-split)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'evil-window-delete)

;; Tab operations (matching nvim)
(evil-define-key 'normal 'global (kbd "tt") 'tab-new)
(evil-define-key 'normal 'global (kbd "th") 'tab-first)
(evil-define-key 'normal 'global (kbd "tj") 'tab-previous)
(evil-define-key 'normal 'global (kbd "tk") 'tab-next)
(evil-define-key 'normal 'global (kbd "tl") 'tab-last)
(evil-define-key 'normal 'global (kbd "tn") 'tab-new)
(evil-define-key 'normal 'global (kbd "td") 'tab-close)

;; Buffer navigation (matching old config)
(evil-define-key 'normal 'global (kbd "[") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "]") 'next-buffer)

;; Better discoverability
(setq help-window-select t)              ; Focus help windows automatically

;; More useful keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer) ; Better buffer list

;;; init.el ends here
