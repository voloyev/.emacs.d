;;; package --- Summary:
;;; Code:
;;; Commentary:
;; highlight indentation
(use-package highlight-indentation
    :straight t
    :bind (("<f9>" . highlight-indentation-mode)
           ("M-<f9>" . highlight-indentation-current-column-mode)))

(provide 'highlight-indentation-mode-module)
;;; highlight-indentation-mode-module ends here
