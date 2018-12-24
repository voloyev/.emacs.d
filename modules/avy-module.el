;;; package --- Summary:
;;; Code:
;;; Commentary:
;; avy module
(use-package avy
    :ensure t
    :bind
    ("C-;" . avy-goto-char)
    ("C-c :" . avy-goto-char-2)
    ("C-c g l" . avy-goto-line)
    ("C-c g w" . avy-goto-word-0))

(provide 'avy-module)
;;; avy-module.el ends here
