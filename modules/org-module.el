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

(use-package org-brain
    :ensure t
    :init
    (setq org-brain-path "~/.emacs.d/brain")
    :config
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12))

;; eval langs in go
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)
   (lisp . t)
   (scheme . t)
   (clojure . t)
   (python . t)))

(setq org-log-done 'time)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(provide 'org-module)
;;; org-module ends here
