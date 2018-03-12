;;; package --- Summary
;;; Commentary:
;;; php module
;;; Code:
;; here should be settings that can not be placet anywhere elese
(use-package vi-tilde-fringe
    :ensure t
    :config
    (global-vi-tilde-fringe-mode t))

;; Achievements mode
(use-package achievements
    :ensure t
    :config
    (achievements-mode 1))

(use-package switch-window
    ;; Switch window
    :ensure t
    :bind(("C-x o" . switch-window)))

(use-package focus
    :ensure t
    :bind(("C-c f m" . focus-mode)))

(use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; racket
(use-package racket-mode
    :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Markdown
(use-package markdown-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (setq markdown-command "grip --export"))

;; line number
(use-package nlinum
    :bind (("C-c C-l" . nlinum-mode)))

;; gutter
(use-package git-gutter-fringe
    :config
    (global-git-gutter-mode t))

;; calendar app
(use-package calfw
    :ensure t)
(use-package calfw-org
    :ensure t)

;; Add haml and yaml modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;;copy without selection
(defadvice kill-ring-save (before slick-copy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive (if mark-active (list (region-beginning) (region-end))
                     (message "Copied line")
                     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
         (list (line-beginning-position)
               (line-beginning-position 2)))))

(provide 'settings-module)
;;; settings-module.el ends here
