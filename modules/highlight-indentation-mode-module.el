;;; package --- Summary:
;;; Code:
;;; Commentary:
;; highlight indentation
(highlight-indentation-current-column-mode 1)
;;(provide 'init)
;;add highlight ingentation
(global-set-key(kbd "<f9>") 'highlight-indentation-current-column-mode)
(set-face-background 'highlight-indentation-face "grey9")
(set-face-background 'highlight-indentation-current-column-face "grey9")
;;hooks ;)
;;; hooks for highlightion
(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'haml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'web-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'rust-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'lisp-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'slim-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'c-mode-hook 'highlight-indentation-current-column-mode)
(provide 'highlight-indentation-mode-module)
;;; highlight-indentation-mode-module ends here
