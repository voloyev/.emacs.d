;;; package --- Summary
;;; Commentary:
;;; php module
;;; Code:
;; here should be settings that can not be placed anywhere elese

(use-package ibuffer
    :bind ("C-x C-b" . ibuffer)
    :init
    (autoload 'ibuffer "ibuffer" "List buffers." t)
    (defalias 'list-buffers 'ibuffer)
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                (ibuffer-auto-mode 1)))
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))

(use-package ace-window
    :ensure t
    :config
    (setq aw-dispatch-always t)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :bind(("M-o" . ace-window)))

(use-package focus
    :ensure t
    :bind(("C-c m f" . focus-mode)))

(use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

(use-package flycheck
    :ensure t
    :init
    (global-flycheck-mode))

(use-package flycheck-inline
    :ensure t
    :init
    (global-flycheck-inline-mode t))

(use-package flycheck-pycheckers
    :ensure t)

(use-package super-save
    :ensure t
    :config
    (setq super-save-auto-save-when-idle t)
    (super-save-mode +1))

(use-package markdown-mode
    :init (setq markdown-command "mark")
    :mode ("\\.text\\'" . markdown-mode)
    :mode ("\\.markdown\\'" . markdown-mode)
    :mode ("\\.md\\'" . markdown-mode))

(use-package markdown-preview-mode
    :ensure t)

(use-package linum
    :bind (("C-c C-l" . linum-mode)))

;; (use-package git-gutter-fringe
;;     :config
;;   (global-git-gutter-mode t))

;; calendar app
(use-package calfw
    :ensure t)

(use-package calfw-org
    :ensure t)

(use-package which-key
    :config
  (which-key-mode t))

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

(use-package yaml-mode
    :mode ("\\.yml\\'" . yaml-mode))

(use-package haml-mode
    :mode ("\\.haml\\'" . haml-mode))

(use-package restclient
    :ensure t
    :mode ("\\.restc\\'" . restclient-mode))

(use-package bfbuilder
    :ensure t
    :mode ("\\.bf\\'" . bfbuilder-mode))

(use-package fsharp-mode
    :ensure t
    :mode ("\\.fs[iylx]?$" . fsharp-mode))

(use-package nasm-mode
    :ensure t)

;; (use-package dired
;;     :commands dired
;;     :init
;;     (setq dired-listing-switches
;;           "-laGh1v --group-directories-first"))
;; work mouse in terminal
(xterm-mouse-mode t)

(use-package eyebrowse
    :ensure t
    :config
    (eyebrowse-mode t))

(use-package nov
    :ensure t
    :mode ("\\.epub\\'" . nov-mode))

(use-package auto-highlight-symbol
    :ensure t)

(add-hook 'js2-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'js2-jsx-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'elixir-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'ruby-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'rust-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'python-mode-hook 'auto-highlight-symbol-mode)

(global-auto-revert-mode t)

(use-package diff-hl
  :ensure t
  :config
  (diff-hl-margin-mode +1)
  (diff-hl-dired-mode +1)
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; Easy transition between buffers: M-arrow-keys
;; (if (equal nil (equal major-mode 'org-mode))
;;     (windmove-default-keybindings 'meta))

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
