;;; package --- Summary
;;; Commentary:
;;; php module
;;; Code:
;; here should be settings that can not be placed anywhere elese
(use-package vi-tilde-fringe
    :ensure t
    :config
    (global-vi-tilde-fringe-mode t))

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
(use-package flycheck-pycheckers
  :ensure t)
;;(with-eval-after-load 'flycheck
;; (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; Markdown
(use-package markdown-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (setq markdown-command "mark"))

(use-package markdown-preview-mode
    :ensure t)

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

;; which-key
(use-package which-key
    :config
    (which-key-mode t))

;; nginx
(use-package company-nginx
    :ensure t
    :config
    (eval-after-load 'nginx-mode
      '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

;; upcase region
(use-package fix-word
    :ensure t
    :bind(("M-u" . fix-word-upcase)
          ("M-l" . fix-word-downcase)
          ("M-c" . fix-word-capitalize)))

(use-package es-mode
    :ensure t)

;; Add haml and yaml modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

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
