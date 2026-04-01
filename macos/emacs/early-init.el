;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;; Runs before frame creation — used for frame appearance settings.

;;; Code:

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)

;;; early-init.el ends here
