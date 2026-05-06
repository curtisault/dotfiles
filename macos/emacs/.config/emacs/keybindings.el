;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Mirrors the section structure of nvim/.config/nvim/lua/curtis/remap.lua.
;; Loaded by `init.el' after evil + evil-collection + relevant packages.

;;; Code:

;;;; ------------- Normal -------------
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)


;;;; ------------- Diagnostics -------------
(evil-define-key 'normal 'global (kbd "<leader>dt") 'consult-flymake)


;;;; Window Split
(evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-vsplit)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-split)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'evil-window-delete)


;;;; Window Nav
(evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)


;;;; Window Resize
(evil-define-key 'normal 'global (kbd "<S-up>")    'shrink-window)
(evil-define-key 'normal 'global (kbd "<S-down>")  'enlarge-window)
(evil-define-key 'normal 'global (kbd "<S-left>")  'shrink-window-horizontally)
(evil-define-key 'normal 'global (kbd "<S-right>") 'enlarge-window-horizontally)


;;;; Tab
(evil-define-key 'normal 'global (kbd "tt") 'tab-new)
(evil-define-key 'normal 'global (kbd "th") 'tab-first)
(evil-define-key 'normal 'global (kbd "tj") 'tab-previous)
(evil-define-key 'normal 'global (kbd "tk") 'tab-next)
(evil-define-key 'normal 'global (kbd "tl") 'tab-last)
(evil-define-key 'normal 'global (kbd "tn") 'tab-new)
(evil-define-key 'normal 'global (kbd "td") 'tab-close)


;;;; ------------- Visual -------------
(evil-define-key 'visual 'global (kbd "<")
  (lambda () (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore)))
(evil-define-key 'visual 'global (kbd ">")
  (lambda () (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore)))


;;;; ------ Packages ------
;; (no equivalent yet — straight handles updates via M-x straight-pull-all)


;;;; ------ Sessions ------
;; (placeholder — wire up when desktop-save-mode or perspective is added)


;;;; ------ Fuzzy ------
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)


;;;; ------ Git ------
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)


;;;; ------ File Tree ------
;; (placeholder — wire up when treemacs / dired-sidebar / neotree is added)


;;;; ------------- Emacs-specific -------------
;;;; Project
(evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ps") 'project-shell)
(evil-define-key 'normal 'global (kbd "<leader>pg") 'project-find-regexp)


;;;; LSP
(evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'xref-find-definitions)
(evil-define-key 'normal 'global (kbd "<leader>li") 'xref-find-references)


;;;; Buffer
(evil-define-key 'normal 'global (kbd "[") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "]") 'next-buffer)


;;;; Database
(evil-define-key 'normal 'global (kbd "<leader>db") 'pgmacs)


;;;; Misc
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; keybindings.el ends here
