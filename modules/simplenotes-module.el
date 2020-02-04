;;; package --- Summary
;;; Module for simplenotes functions
;;; Commentary:
;;; Code:
(use-package
    simplenote2
    :straight t
    :init
    (setq simplenote2-email
          (getenv "SIMPLENOTE2_EMAIL"))
    (setq simplenote2-password
          (getenv "SIMPLENOTE2_PASSWORD")))

(add-hook
 'simplenote2-create-note-hook
 (lambda ()
   (simplenote2-set-markdown)))
         
(add-hook 'simplenote2-note-mode-hook 'markdown-mode)
(add-hook 'simplenote2-note-mode-hook 'smartparens-strict-mode)

(provide 'simplenotes-module)
;;; simplenotes-module.el ends here
