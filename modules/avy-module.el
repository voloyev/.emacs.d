;;; package --- Summary:
;;; Code:
;;; Commentary:
;; avy module
(use-package avy
    :ensure t)
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "C-c g l") 'avy-goto-line)
(global-set-key (kbd  "C-c g w") 'avy-goto-word-0)

(provide 'avy-module)
;;; avy-module.el ends here
