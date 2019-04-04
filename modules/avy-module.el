;;; package --- Summary:
;;; Code:
;;; Commentary:
;; avy module
(use-package avy
    :ensure t
    :bind
    ("M-g ;" . avy-goto-char)
    ("M-g :" . avy-goto-char-2)
    ("M-g w g" . avy-goto-word-0)
    ("M-g g" . avy-goto-line))

(provide 'avy-module)
;;; avy-module.el ends here
