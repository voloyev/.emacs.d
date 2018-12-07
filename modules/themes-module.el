
;;; packaage --- Summary:
;;; Code:
;;; Commentary:
;;; cursor-active-mark-init doing this
;; themes
;; (setq custom-safe-themes t)
;;(add-to-list 'custom-theme-load-path "~/workspace/lisp/emacs-lisp/sexy-monochrome-theme")
;; (use-package sexy-monochrome-theme
;;     :ensure t
;;     :init
;;     (load-theme 'sexy-monochrome t)
;;     (enable-theme 'sexy-monochrome))

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

;;(load-theme 'zenburn t)
(provide 'themes-module)
;;; themes-module ends here
