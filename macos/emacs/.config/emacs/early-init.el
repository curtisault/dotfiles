;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;; Runs before frame creation — used for frame appearance settings and
;; startup-time GC tuning.

;;; Code:

;; Defer garbage collection during startup; restored in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Avoid invoking file-name handlers (TRAMP, archive, etc.) at startup.
(defvar splash--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Frame appearance
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;;; early-init.el ends here
