;;; package -- Summary
;;; Commentary:
;;;; hydra module
;;; Code:
(use-package hydra
    :ensure t)

(defhydra hydra-zoom (global-map "C-<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(provide 'hydra-module)
;;; hydra-module.el ends here
