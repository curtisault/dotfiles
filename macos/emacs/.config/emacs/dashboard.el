;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures the upstream `dashboard' package as the startup buffer with a
;; Doom-style menu feel.  Loaded by `init.el' via absolute path, so it does
;; not collide with the upstream package's own `dashboard.el'.

;;; Code:

;; Catppuccin Frappé palette helpers (used for face tinting below)
(defconst my-dashboard--mauve    "#ca9ee6")
(defconst my-dashboard--peach    "#ef9f76")
(defconst my-dashboard--blue     "#8caaee")
(defconst my-dashboard--subtext0 "#a5adce")
(defconst my-dashboard--text     "#c6d0f5")

;; Make sure ~/notes/ has a "notes" bookmark so it shows up in the
;; Bookmarks section and can be jumped to via `bookmark-jump'.
(require 'bookmark)
(unless (bookmark-get-bookmark "notes" 'noerror)
  (bookmark-store "notes"
                  `((filename . ,(expand-file-name "~/notes/")))
                  nil))

(defun my-dashboard--insert-shortcuts (_list-size)
  "Insert a cheatsheet of single-key shortcuts active in *dashboard*."
  (dashboard-insert-heading "Shortcuts:")
  (insert "\n")
  (let ((entries '(("p"   . "Switch project")
                   ("n"   . "Open ~/notes/")
                   ("a"   . "Org agenda")
                   ("r"   . "Recent files")
                   ("m"   . "Magit status")
                   ("q"   . "Close dashboard")
                   ("g r" . "Refresh dashboard"))))
    (dolist (entry entries)
      (insert (format "    %-4s  %s\n" (car entry) (cdr entry))))))

(use-package dashboard
  :after nerd-icons
  :init
  (setq dashboard-banner-logo-title "An operating system disguised as an editor."
        dashboard-startup-banner (expand-file-name "assets/banner.txt" user-emacs-directory)
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-icon-type 'nerd-icons
        dashboard-set-init-info t
        dashboard-set-footer t
        dashboard-footer-messages
        '("One does not simply C-x C-c."
          "All we have to decide is what to do with the time that is given us."
          "Even the smallest commit can change the course of the future."
          "Speak friend, and enter."
          "Not all those who wander are lost — some are just M-x apropos.")
        dashboard-projects-backend 'project-el
        dashboard-items '((recents   . 7)
                          (projects  . 5)
                          (bookmarks . 5)
                          (agenda    . 5)
                          (shortcuts . 7)))
  :config
  (add-to-list 'dashboard-item-generators '(shortcuts . my-dashboard--insert-shortcuts))
  ;; Catppuccin Frappé tints
  (set-face-attribute 'dashboard-banner-logo-title nil
                      :foreground my-dashboard--mauve :weight 'bold)
  (set-face-attribute 'dashboard-heading nil
                      :foreground my-dashboard--blue :weight 'bold)
  (set-face-attribute 'dashboard-navigator nil
                      :foreground my-dashboard--peach)
  (set-face-attribute 'dashboard-footer-face nil
                      :foreground my-dashboard--subtext0 :slant 'italic)
  (set-face-attribute 'dashboard-text-banner nil
                      :foreground my-dashboard--text)

  ;; Doom-style menu — one button per row, [letter] hints in the labels
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-octicon "nf-oct-file_directory") "[p] Projects" "Switch project (p)"
            (lambda (&rest _) (call-interactively #'project-switch-project))))
          ((,(nerd-icons-octicon "nf-oct-book") "[n] Notes" "Open ~/notes (n)"
            (lambda (&rest _) (find-file "~/notes/"))))
          ((,(nerd-icons-octicon "nf-oct-calendar") "[a] Agenda" "Org agenda (a)"
            (lambda (&rest _) (org-agenda nil "a"))))
          ((,(nerd-icons-octicon "nf-oct-history") "[r] Recent files" "Recent files (r)"
            (lambda (&rest _) (call-interactively #'consult-recent-file))))
          ((,(nerd-icons-octicon "nf-oct-git_branch") "[m] Magit" "Magit status (m)"
            (lambda (&rest _) (call-interactively #'magit-status))))))

  ;; Single-letter shortcuts inside *dashboard*
  (with-eval-after-load 'evil
    (evil-define-key '(normal motion) dashboard-mode-map
      "p" (lambda () (interactive) (call-interactively #'project-switch-project))
      "n" (lambda () (interactive) (find-file "~/notes/"))
      "a" (lambda () (interactive) (org-agenda nil "a"))
      "r" (lambda () (interactive) (call-interactively #'consult-recent-file))
      "m" (lambda () (interactive) (call-interactively #'magit-status))
      "q" #'kill-this-buffer
      (kbd "g r") #'dashboard-refresh-buffer))

  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create dashboard-buffer-name))))

;;; dashboard.el ends here
