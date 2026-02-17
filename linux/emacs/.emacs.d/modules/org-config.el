;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package org
  :straight nil
  :config
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'content)
  (setq org-log-done 'time)

  ;; Play sound when org-timer finishes
  (defun my/org-timer-done-sound ()
    "Play a system sound when org-timer finishes."
    (cond
     ;; Try paplay (PulseAudio) with freedesktop sound theme
     ((executable-find "paplay")
      (start-process "org-timer-sound" nil "paplay"
                     "/usr/share/sounds/freedesktop/stereo/complete.oga"))
     ;; Fallback to aplay (ALSA) if available
     ((executable-find "aplay")
      (start-process "org-timer-sound" nil "aplay"
                     "/usr/share/sounds/freedesktop/stereo/complete.oga"))
     ;; Try canberra-gtk-play with sound name
     ((executable-find "canberra-gtk-play")
      (start-process "org-timer-sound" nil "canberra-gtk-play" "-i" "complete"))))

  (add-hook 'org-timer-done-hook 'my/org-timer-done-sound)

  ;; Override org notification to avoid D-Bus errors
  ;; Just show message in minibuffer instead
  (setq org-show-notification-handler
        (lambda (msg)
          (message "Timer finished: %s" msg)))

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

(provide 'org-config)
;;; org-config.el ends here
