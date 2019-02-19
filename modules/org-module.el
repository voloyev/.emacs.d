;;; package -- Summary
;;; Commentary:
;;; Org module
;;; Code:
(use-package org-install
    :init
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq org-agenda-files (list "~/.emacs.d/todo.org"))
  ;; "~/Dropbox/org/todo.org" "~/Dropbox/org/tasks.org"))
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq org-src-fontify-natively nil)
  (setq org-html-htmlize-output-type nil) ;; output without
  (defface org-block
      '((t (:background "#000000")))
    "Face used for the source block background.")
  :bind(("\C-cl" . org-store-link)
        ("\C-ca" . org-agenda)
        ("\C-cc" . org-capture)
        ("\C-cb" . org-iswitchb)))

(use-package ox-reveal
    :ensure t)

;; eval langs in go
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(provide 'org-module)
;;; org-module ends here
