;;; package --- Summary:
;;; Code:
;;; Commentary:
;; highlight indentation
(use-package highlight-indentation
    :ensure t
    :bind (("<f9>" . highlight-indentation-mode)
           ("M-<f9>" . highlight-indentation-current-column-mode))

    :config
    (progn
      (set-face-background 'highlight-indentation-face "grey20")
      (set-face-background 'highlight-indentation-current-column-face "grey40")))

(provide 'highlight-indentation-mode-module)
;;; highlight-indentation-mode-module ends here
