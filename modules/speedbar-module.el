;;; package --- Summary:
;;; Code:
;;; Commentary:
;; sr-speedbar

(use-package sr-speedbar
    :ensure t
    :init
    (add-hook 'speedbar-mode-hook
          (lambda()
              (speedbar-add-supported-extension "\\.rb")
              (speedbar-add-supported-extension "\\.ru")
              (speedbar-add-supported-extension "\\.erb")
              (speedbar-add-supported-extension "\\.rjs")
              (speedbar-add-supported-extension "\\.rhtml")
              (speedbar-add-supported-extension "\\.rake")
              (speedbar-add-supported-extension "\\.md")
              (speedbar-add-supported-extension "\\.py")
              (speedbar-add-supported-extension "\\.html")
              (speedbar-add-supported-extension "\\.css")
              (speedbar-add-supported-extension ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?")
              (speedbar-add-supported-extension  ".tex\\(i\\(nfo\\)?\\)?")
              (speedbar-add-supported-extension  "\\.todo")
              (speedbar-add-supported-extension  "\\.done")
              (speedbar-add-supported-extension  "\\.el")
              (speedbar-add-supported-extension  ".emacs")
              (speedbar-add-supported-extension  "\\.l")
              (speedbar-add-supported-extension  "\\.lsp")
              (speedbar-add-supported-extension  "\\.p")
              (speedbar-add-supported-extension  "\\.java")
              (speedbar-add-supported-extension  "\\.js")
              (speedbar-add-supported-extension  ".f\\(90\\|77\\|or\\)?")
              (speedbar-add-supported-extension  ".ad[abs]")
              (speedbar-add-supported-extension  ".p[lm]")
              (speedbar-add-supported-extension  "\\.tcl")
              (speedbar-add-supported-extension  ".m")
              (speedbar-add-supported-extension  "\\.scm")
              (speedbar-add-supported-extension  ".pm")
              (speedbar-add-supported-extension  "\\.g")
              (speedbar-add-supported-extension  "\\.\\(inc\\|php[s345]?\\|phtml\\)")
              (speedbar-add-supported-extension  ".s?html")
              (speedbar-add-supported-extension  ".ma?k")
              (speedbar-add-supported-extension  ".haml")
              (speedbar-add-supported-extension  "[Mm]akefile\\(\\.in\\)?")
              (speedbar-add-supported-extension  "\\.rs")))
    :config
    (setq sr-speedbar-width-x 20)
    (setq speedbar-show-unknown-files t)
    (sr-speedbar-refresh-turn-on)
    :bind
    ("<f6>" . sr-speedbar-toggle))

(provide 'speedbar-module)
;;; speedbar-module.el ends here
