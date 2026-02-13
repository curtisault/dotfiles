;;; evil-config.el --- Evil mode and keybindings -*- lexical-binding: t; -*-

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-respect-visual-line-mode t)

  ;; Cursor appearance
  (setq evil-insert-state-cursor 'bar)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor 'box)
  :config
  (evil-mode 1)

  ;; Terminal cursor shape changes
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

  ;; Set leader key (Space)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Keep some Emacs bindings in insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  ;; Jump to definition (use xref which works with LSP)
  (define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "C-t") 'xref-go-back))

;; Configure xref to not prompt when there's only one match
(setq xref-prompt-for-identifier nil)

;; Evil Collection - Evil bindings for many modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Commenting
(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
         ("C-c c" . evilnc-copy-and-comment-lines)
         ("C-c p" . evilnc-comment-or-uncomment-paragraphs)))

;; ---------------------------------------------------------------------------
;; Global keybindings (leader + navigation)
;; ---------------------------------------------------------------------------

;; Clear highlights
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)

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

;; Buffer navigation
(evil-define-key 'normal 'global (kbd "[") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "]") 'next-buffer)

(provide 'evil-config)
;;; evil-config.el ends here
