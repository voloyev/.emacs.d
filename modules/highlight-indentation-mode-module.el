;;; package --- Summary:
;;; Code:
;;; Commentary:
;; highlight indentation
(use-package highlight-indentation-current-column-mode
    :ensure t
    :bind
    ("<f9>" . highlight-indentation-current-column-mode)
    :config
    (set-face-background 'highlight-indentation-face "grey9")
    (set-face-background 'highlight-indentation-current-column-face "grey9"))
(provide 'highlight-indentation-mode-module)
;;; highlight-indentation-mode-module ends here
