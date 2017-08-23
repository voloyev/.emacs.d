;;; packaage --- Summary:
;;; Code:
;;; Commentary:
;;; cursor-active-mark-init doing this
;; themes
(setq custom-safe-themes t)
(load-theme 'zenburn t)
(enable-theme 'zenburn)

;;(load-theme 'sexy-monochrome t)
;;(enable-theme 'sexy-monochrome)

(defun cursor-active-mark-init ()
    "Change cursor color into red in mark mode."
    (set-cursor-color "#ff0000")
    (setq cursor-type 'bar))

(add-hook 'activate-mark-hook 'cursor-active-mark-init)
"Hook that call to change cursor color function."

(defun th-deactivate-mark-init ()
    "Hook that change back cursor to normal color, after mark mode is done."
    (set-cursor-color "light gray")
    (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)
"If mark-mode is off, do nothing"
(provide 'themes-module)
;;; themes-module ends here
