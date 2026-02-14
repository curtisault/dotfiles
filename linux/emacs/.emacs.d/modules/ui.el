;;; ui.el --- Theme, modeline, and visual enhancements -*- lexical-binding: t; -*-

;;; Code:

;; ---------------------------------------------------------------------------
;; Theme - Catppuccin Frappe
;; ---------------------------------------------------------------------------

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm))

;; ---------------------------------------------------------------------------
;; Which-Key - Show keybinding hints
;; ---------------------------------------------------------------------------

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom))

;; ---------------------------------------------------------------------------
;; Rainbow Delimiters
;; ---------------------------------------------------------------------------

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ---------------------------------------------------------------------------
;; TODO Highlighting - hl-todo
;; ---------------------------------------------------------------------------

(use-package hl-todo
  :config
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("TODO"      . "#7c7d7c")
          ("FIXME"     . "#cc6666")
          ("HACK"      . "#d27b53")
          ("IMPORTANT" . "#d27b53")
          ("NOTE"      . "#67b11d"))))

;; ---------------------------------------------------------------------------
;; Vundo - Visual undo tree
;; ---------------------------------------------------------------------------

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>u") 'vundo))

;; ---------------------------------------------------------------------------
;; Modeline
;; ---------------------------------------------------------------------------

(column-number-mode 1)

(defface mode-line-evil-normal
  '((t :inherit mode-line-emphasis :foreground "#303446" :background "#8caaee" :weight bold))
  "Face for Evil normal state.")

(defface mode-line-evil-insert
  '((t :inherit mode-line-emphasis :foreground "#303446" :background "#a6d189" :weight bold))
  "Face for Evil insert state.")

(defface mode-line-evil-visual
  '((t :inherit mode-line-emphasis :foreground "#303446" :background "#ca9ee6" :weight bold))
  "Face for Evil visual state.")

(defface mode-line-evil-replace
  '((t :inherit mode-line-emphasis :foreground "#303446" :background "#e78284" :weight bold))
  "Face for Evil replace state.")

(setq-default mode-line-format
  '("%e"

    ;; Evil state with colored background
    (:eval
     (let* ((state (if (bound-and-true-p evil-state)
                       evil-state
                     'emacs))
            (state-string (upcase (symbol-name state)))
            (face (pcase state
                    ('normal 'mode-line-evil-normal)
                    ('insert 'mode-line-evil-insert)
                    ('visual 'mode-line-evil-visual)
                    ('replace 'mode-line-evil-replace)
                    (_ 'mode-line-emphasis))))
       (propertize (concat " " state-string " ") 'face face)))

    " "

    ;; Git branch
    (:eval
     (when (and vc-mode buffer-file-name)
       (let ((branch (replace-regexp-in-string "^ Git[-:]" "" vc-mode)))
         (concat
          (propertize "" 'face 'font-lock-doc-face)
          " "
          (propertize branch 'face 'font-lock-string-face)
          " "))))

    ;; Buffer status (modified/read-only)
    (:eval
     (cond (buffer-read-only
            (propertize "" 'face 'error))
           ((buffer-modified-p)
            (propertize "●" 'face 'warning))
           (t "")))

    "  "

    ;; Filename with path
    (:eval
     (let ((filename (if buffer-file-name
                         (let ((path (file-relative-name buffer-file-name
                                                         (project-root (project-current)))))
                           (if (string-prefix-p "../" path)
                               (abbreviate-file-name buffer-file-name)
                             path))
                       "%b")))
       (propertize filename 'face 'mode-line-buffer-id)))

    ;; Spacer
    "  "
    (:eval (propertize " " 'display '((space :align-to (- right 50)))))

    ;; LSP indicator (Eglot)
    (:eval
     (when (bound-and-true-p eglot--managed-mode)
       (let* ((server (eglot-current-server))
              (server-name (if server
                               (plist-get (eglot--server-info server) :name)
                             "LSP")))
         (concat
          (propertize "" 'face 'success)
          " "
          (propertize (format "%s" server-name)
                      'face 'font-lock-builtin-face)
          "  "))))

    ;; Major mode
    (:eval
     (propertize (format "%s" mode-name)
                 'face 'font-lock-keyword-face))

    "  "

    ;; Line:Column
    (:eval
     (propertize (format "%d:%d"
                         (line-number-at-pos)
                         (current-column))
                 'face 'font-lock-constant-face))

    "  "

    ;; Percentage through file
    (:eval
     (let ((pct (if (> (point-max) (point-min))
                    (/ (* 100 (- (point) (point-min)))
                       (- (point-max) (point-min)))
                  0)))
       (propertize (format "%d%%%%" pct)
                   'face 'font-lock-comment-face)))

    " "))

;; Ensure mode-line updates in real-time
(add-hook 'post-command-hook 'force-mode-line-update)

;; Also update on Evil state changes
(add-hook 'evil-insert-state-entry-hook 'force-mode-line-update)
(add-hook 'evil-normal-state-entry-hook 'force-mode-line-update)
(add-hook 'evil-visual-state-entry-hook 'force-mode-line-update)
(add-hook 'evil-replace-state-entry-hook 'force-mode-line-update)

(provide 'ui)
;;; ui.el ends here
