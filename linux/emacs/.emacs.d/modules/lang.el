;;; lang.el --- Language modes and tree-sitter -*- lexical-binding: t; -*-

;;; Code:

;; ---------------------------------------------------------------------------
;; Major Mode Packages
;; ---------------------------------------------------------------------------

(use-package elixir-ts-mode
  :straight (elixir-ts-mode :type git :host github :repo "wkirschbaum/elixir-ts-mode"))

(use-package lua-mode)
(use-package yaml-mode)
(use-package rust-mode)

;; ---------------------------------------------------------------------------
;; Treesitter Support (Built-in Emacs 29+)
;; ---------------------------------------------------------------------------

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

(defun install-treesit-grammars ()
  "Install all tree-sitter grammars from treesit-language-source-alist."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((lang-name (car lang)))
      (unless (treesit-language-available-p lang-name)
        (message "Installing tree-sitter grammar for %s..." lang-name)
        (treesit-install-language-grammar lang-name)))))

(defun auto-install-treesit-grammar ()
  "Automatically install tree-sitter grammar if missing."
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

(provide 'lang)
;;; lang.el ends here
