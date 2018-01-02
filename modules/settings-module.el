;;; package --- Summary
;;; Commentary:
;;; php module
;;; Code:
;; here should be settings that can not be placet anywhere elese
(use-package vi-tilde-fringe
    :ensure t
    :config
    (global-vi-tilde-fringe-mode t))

;; Achievements mode
(use-package achievements
    :ensure t
    :config
    (achievements-mode 1))

(use-package switch-window
    ;; Switch window
    :ensure t
    :bind(("C-x o" . switch-window)))

(use-package focus
    :ensure t)

(provide 'settings-module)
;;; settings-module.el ends here
