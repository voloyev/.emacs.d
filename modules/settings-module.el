;;; package --- Summary
;;; Commentary:
;;; Settings module
;;; Code:
;; here should be settings that can not be placed anywhere elese

;; (global-display-line-numbers-mode t)
;; (setq display-line-numbers-type 'relative)

;; general configs
(xterm-mouse-mode t)
(global-auto-revert-mode t)
(global-set-key (kbd "C-c ]") 'next-buffer)
(global-set-key (kbd "C-c [") 'previous-buffer)
(setq redisplay-dont-pause t)

(use-package ibuffer
    :bind ("C-x C-b" . ibuffer)
    :init
    (autoload 'ibuffer "ibuffer" "List buffers." t)
    (defalias 'list-buffers 'ibuffer)
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                (ibuffer-auto-mode t)))
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))
"
x - delete window
m - swap windows
M - move window
c - copy window
j - select buffer
n - select the previous window
u - select buffer in the other window
c - split window fairly, either vertically or horizontally
v - split window vertically
b - split window horizontally
o - maximize current window
? - show these command bindings
"
(use-package ace-window
    :straight t
    :config
    (setq aw-dispatch-always t)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :bind(("M-o" . ace-window)))

(use-package rainbow-delimiters
    :straight t
    :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
    :straight t
    :config
    (editorconfig-mode t))

(use-package flycheck-pos-tip
    :straight t)

(use-package flycheck
    :straight t
    :init
    (global-flycheck-mode)
    :config
    (flycheck-pos-tip-mode))

(use-package flycheck-mix
    :straight t
    :init
    (flycheck-mix-setup))

(use-package flycheck-pycheckers
    :straight t)

(use-package super-save
    :straight t
    :config
    (setq super-save-auto-save-when-idle t)
    (super-save-mode +1)
    (setq auto-save-default nil))

(use-package markdown-mode
    :init (setq markdown-command "mark")
    :mode ("\\.text\\'" . markdown-mode)
    :mode ("\\.markdown\\'" . markdown-mode)
    :mode ("\\.md\\'" . markdown-mode))

(use-package which-key
    :straight t
    :config (which-key-mode t))

(use-package company-nginx
    :straight t
    :config
    (eval-after-load 'nginx-mode
      '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

;; upcase region
(use-package fix-word
    :straight t
    :bind(("M-u" . fix-word-upcase)
          ("M-l" . fix-word-downcase)
          ("M-c" . fix-word-capitalize)))

(use-package es-mode
    :straight t
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((elasticsearch . t)))
    :config (setq es-always-pretty-print t))

(use-package yaml-mode
    :mode ("\\.yml\\'" . yaml-mode))

(use-package haml-mode
    :mode ("\\.haml\\'" . haml-mode))

(use-package restclient
    :straight t
    :mode ("\\.restc\\'" . restclient-mode))

(use-package bfbuilder
    :straight t
    :mode ("\\.bf\\'" . bfbuilder-mode))

(use-package fsharp-mode
    :straight t
    :mode ("\\.fs[iylx]?$" . fsharp-mode))

(use-package nasm-mode
    :straight t)

(use-package auto-highlight-symbol
    :straight t)

(add-hook 'js2-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'js2-jsx-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'elixir-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'ruby-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'rust-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'python-mode-hook 'auto-highlight-symbol-mode)

(use-package diff-hl
  :straight t
  :config
  (diff-hl-margin-mode +1)
  (diff-hl-dired-mode +1)
  (global-diff-hl-mode +1)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package volatile-highlights
  :straight t
  :config
  (volatile-highlights-mode +1))

(use-package rich-minority
    :straight t
    :config
    (rich-minority-mode t))

(use-package projectile-rails
    :straight t)

(use-package projectile
    :straight t
    :config
    (projectile-global-mode)
    (projectile-rails-global-mode)
    (define-key projectile-mode-map
        (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-rails-mode-map
        (kbd "C-c r") 'hydra-projectile-rails/body)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-mode-line
          '(:eval (format " Projectile[%s]"
                   (projectile-project-name)))))

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

(when (memq window-system '(ns mac))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(provide 'settings-module)
;;; settings-module.el ends here
