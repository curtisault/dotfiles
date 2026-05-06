;;; modeline.el --- Custom mode-line -*- lexical-binding: t; -*-

;;; Commentary:
;; Vanilla mode-line customized with Catppuccin Frappé tinting, an Evil
;; state pill, vc/git branch, magit-style buffer status, eglot/flymake
;; diagnostics, and right-aligned major-mode + position info.

;;; Code:

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
    ;; Evil state pill
    (:eval
     (let* ((state (if (bound-and-true-p evil-state) evil-state 'emacs))
            (state-string (upcase (symbol-name state)))
            (face (pcase state
                    ('normal 'mode-line-evil-normal)
                    ('insert 'mode-line-evil-insert)
                    ('visual 'mode-line-evil-visual)
                    ('replace 'mode-line-evil-replace)
                    (_ 'mode-line-emphasis))))
       (propertize (concat " " state-string " ") 'face face)))

    " "

    ;; Macro recording indicator
    (:eval
     (when defining-kbd-macro
       (propertize " ● REC " 'face 'error)))

    ;; Org timer + misc info
    mode-line-misc-info

    ;; Git branch (vc-mode)
    (:eval
     (when (and vc-mode buffer-file-name)
       (let ((branch (replace-regexp-in-string "^ Git[-:]" "" vc-mode)))
         (concat
          (propertize "" 'face 'font-lock-doc-face)
          " "
          (propertize branch 'face 'font-lock-string-face)
          " "))))

    ;; Buffer status (read-only / modified)
    (:eval
     (cond (buffer-read-only       (propertize "" 'face 'error))
           ((buffer-modified-p)    (propertize "●" 'face 'warning))
           (t                       "")))

    "  "

    ;; Filename, project-relative when possible
    (:eval
     (let ((filename (if buffer-file-name
                         (let ((path (file-relative-name buffer-file-name
                                                         (project-root (project-current)))))
                           (if (string-prefix-p "../" path)
                               (abbreviate-file-name buffer-file-name)
                             path))
                       "%b")))
       (propertize filename 'face 'mode-line-buffer-id)))

    ;; Right-aligned section
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
          (propertize (format "%s" server-name) 'face 'font-lock-builtin-face)
          "  "))))

    ;; Flymake diagnostics
    (:eval
     (when (and (bound-and-true-p flymake-mode)
                (fboundp 'flymake--diag-type))
       (let* ((diags (flymake-diagnostics))
              (errors (length (seq-filter
                               (lambda (d) (eq (flymake--diag-type d) :error))
                               diags)))
              (warnings (length (seq-filter
                                 (lambda (d) (eq (flymake--diag-type d) :warning))
                                 diags))))
         (when (or (> errors 0) (> warnings 0))
           (concat
            (when (> errors 0)
              (propertize (format "E:%d " errors) 'face 'error))
            (when (> warnings 0)
              (propertize (format "W:%d" warnings) 'face 'warning))
            "  ")))))

    ;; Major mode
    (:eval (propertize (format "%s" mode-name) 'face 'font-lock-keyword-face))
    "  "

    ;; Line:Column
    (:eval
     (propertize (format "%d:%d" (line-number-at-pos) (current-column))
                 'face 'font-lock-constant-face))
    "  "

    ;; Percentage through file
    (:eval
     (let ((pct (if (> (point-max) (point-min))
                    (/ (* 100 (- (point) (point-min)))
                       (- (point-max) (point-min)))
                  0)))
       (propertize (format "%d%%%%" pct) 'face 'font-lock-comment-face)))

    " "))

;; Force redraws on state changes
(add-hook 'post-command-hook 'force-mode-line-update)
(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook  'force-mode-line-update)
  (add-hook 'evil-normal-state-entry-hook  'force-mode-line-update)
  (add-hook 'evil-visual-state-entry-hook  'force-mode-line-update)
  (add-hook 'evil-replace-state-entry-hook 'force-mode-line-update))

;;; modeline.el ends here
