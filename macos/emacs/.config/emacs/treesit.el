;;; treesit.el --- Tree-sitter setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-sitter language sources, grammar installer (interactive + first-run
;; auto), and major-mode remap from text-mode parents to *-ts-mode.

;;; Code:

(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex       "https://github.com/phoenixframework/tree-sitter-heex")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (lua        "https://github.com/Azganoth/tree-sitter-lua")
        (make       "https://github.com/alemuller/tree-sitter-make")
        (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

(defun install-treesit-grammars ()
  "Install all tree-sitter grammars from `treesit-language-source-alist'."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((lang-name (car lang)))
      (unless (treesit-language-available-p lang-name)
        (message "Installing tree-sitter grammar for %s..." lang-name)
        (treesit-install-language-grammar lang-name)))))

(defun auto-install-treesit-grammar ()
  "Install grammar for the current buffer's language if missing."
  (when (and (treesit-available-p)
             (treesit-language-at (point))
             (not (treesit-language-available-p (treesit-language-at (point)))))
    (let ((lang (treesit-language-at (point))))
      (message "Installing tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang)
      (message "Grammar installed. Please reopen the file.")
      (when (y-or-n-p "Grammar installed. Reopen file now? ")
        (revert-buffer t t)))))

(add-hook 'find-file-hook
          (lambda ()
            (when (and (derived-mode-p 'prog-mode)
                       (string-match-p "-ts-mode$" (symbol-name major-mode)))
              (run-with-timer 0.5 nil #'auto-install-treesit-grammar))))

;; First-run dependency: compile any missing grammars after startup completes.
;; No-op on subsequent launches.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 2 nil #'install-treesit-grammars)))

;; 1=keywords, 2=basic, 3=default, 4=all
(setq treesit-font-lock-level 4)

;; Remap text-mode parents → *-ts-mode when grammars are available.
(setq major-mode-remap-alist
      '((sh-mode         . bash-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (lua-mode        . lua-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

;; .ts / .tsx have no upstream major-mode registration — wire them up directly.
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Note: elixir-ts-mode registers itself for .ex/.exs/.heex directly.
;; Note: .rs is registered by the third-party `rust-mode' package, then
;; remapped to rust-ts-mode via major-mode-remap-alist above.

;;; treesit.el ends here
