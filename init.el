;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")t)
;; activate installed packages
(package-initialize)
(setq my-package-list '())
(mapc #'package-install my-package-list)
(require 'use-package)
(add-to-list 'load-path "~/.emacs.d/modules")

(desktop-save-mode 0)

;;; List of required modules
(require 'auto-install-packages)
(require 'ruby-module)
(require 'speedbar-module)
(require 'smartparens-module)
(require 'web-mode-module)
(require 'yasnippet-module)
(require 'helm-module)
(require 'ivy-module)
(require 'python-module)
(require 'highlight-indentation-mode-module)
(require 'looks-module)
(require 'themes-module)
(require 'js-module)
(require 'rust-module)
(require 'crystal-module)
(require 'elixir-module)
(require 'settings-module)
(require 'go-module)
(require 'clojure-module)
(require 'avy-module)

;; custom plugins path
(add-to-list 'load-path "~/.emacs.d/plugins/")

;; Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Delete selection
(delete-selection-mode t)

;; use bash
(setq shell-file-name "/bin/bash")

;; company mode
(use-package company
    :ensure t
    :init
    (with-eval-after-load 'company
        (add-hook 'after-init-hook 'global-company-mode)
        (add-to-list 'company-backends 'company-robe)
        (add-to-list 'company-backends 'sly-company)
        (add-to-list 'company-backends 'company-jedi))
    :bind("C-<tab>" . company-complete)
    :config
    (global-company-mode t)
    (company-quickhelp-mode t))

;;multiple cursors
(use-package multiple-cursors
    :bind (("C-S-c C-S-c"   . mc/edit-lines)
           ("C-."           . mc/mark-next-like-this)
           ( "C-,"          . mc/mark-previous-like-this)
           ("C-c C-|"       . mc/mark-all-like-this)
           ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;global line mode
(global-hl-line-mode)

;;projectile
(use-package projectile
    :config
    (projectile-global-mode)
    (projectile-rails-global-mode)
    (setq projectile-indexing-method 'native)
    (setq projectile-enable-caching t)
    (setq projectile-mode-line
          '(:eval (format " Projectile[%s]"
                   (projectile-project-name)))))

;; sly
(use-package sly
    :ensure t
    :init
    (setq inferior-lisp-program "sbcl")
    (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
    (add-hook 'lisp-mode-hook 'sly-editing-mode))

;; map of tagtables
(global-set-key (kbd "<f8>") 'visit-tags-table)
" | Combo | Function         | Description                |"
" |-------+------------------+----------------------------|"
" | <f3>  | visit-tags-table | Loads tags                 |"
" | M-.   | find-tag         | Jumps to the specified tag |"
" | C-M-. | pop-tag-mark     | Jumps back                 |"

;; Bookmark settings
(use-package bookmark
    :init
    (setq bookmark-save-flag t)
    (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
    (when (file-exists-p (concat user-emacs-directory "bookmarks"))
        (bookmark-load bookmark-default-file t))
    :bind(("C-c & M-b" . bookmark-set)
          ("C-c & b"   . bookmark-jump)
          ("<f4>"      . bookmark-bmenu-list)))

;; org-mode
(use-package org-install
    :init
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (setq org-agenda-files (list "~/.emacs.d/todo.org")); "~/Dropbox/org/todo.org" "~/Dropbox/org/tasks.org"))
    (add-hook 'org-mode-hook 'toggle-truncate-lines)
    (setq org-src-fontify-natively nil)
    (defface org-block
        '((t (:background "#000000")))
        "Face used for the source block background.")
    :bind(("\C-cl" . org-store-link)
          ("\C-ca" . org-agenda)
          ("\C-cc" . org-capture)
          ("\C-cb" . org-iswitchb)))

;;whitespace
(use-package whitespace
    :init
    (setq whitespace-line-column 250)
    (setq whitespace-display-mappings
          '((space-mark 32 [183] [46])
            (newline-mark 10 [8629 10])
            (tab-mark 9 [9655 9] [92 9])))
    :config
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground "gray30")
    (set-face-attribute 'whitespace-newline
                        nil :background nil
                        :foreground "gray30")
    :bind(("<f5>" . whitespace-mode)
          ("C-c <f5>" . whitespace-cleanup)))

;; emmet mode
(use-package emmet-mode
    :config
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))

;;(put 'upcase-region 'disabled nil)

;; work mouse in terminal
(xterm-mouse-mode t)

;; vimish folds
(use-package vimish-fold
    :bind(("C-c v f" . vimish-fold)
          ("C-c v t" . vimish-fold-toggle)
          ("C-c v u" . vimish-fold-unfold)
          ("C-c v v" . vimish-fold-delete)))

;; magit
(use-package magit
    :bind("C-x g" . magit-status)
    :config
    (global-auto-revert-mode 1))

;; undo tree
(global-undo-tree-mode t)
;; TODO do this realy need
;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; c-mode settings
(setq c-default-style "linux")

;; expand region mode
(use-package expand-region
    :bind("C-=" . er/expand-region))

;; neotree
(use-package  neotree
    :bind(("<f12>" . neotree-projectile-action)
          ("M-<f12>" . neotree-hide))
    :config
    (setq neo-theme  'arrow)
    (setq neo-smart-open t))

;;lein exec path
(add-to-list 'exec-path "~/bin")

;;quickrun
(use-package quickrun
    :ensure t)

;;golden ratio
(use-package golden-ratio
    :ensure t
    :bind("C-c & g" . golden-ratio-mode))

;; toggle quotes
(use-package toggle-quotes
    :bind("C-'" . toggle-quotes))

;;paradox token
(defvar paradox-token
  (getenv "PARADOX"))
(setq paradox-github-token 'paradox-token)

;;disable sound
(setq visible-bell 1)

;;ibuffer settings
(add-hook 'ibuffer-hook
          (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))

;; flyspell
(use-package flyspell
    :config
    (flyspell-mode t))

(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

;; resize buffers
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window)
           ("M-g b" . dumb-jump-back))
    :config
    (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    (setq dumb-jump-force-searcher 'ag)
    :ensure)

;; fzf
(use-package fzf
    :ensure t
    :bind
    (("C-x f" . fzf)))

;; hyde
(use-package hyde
    :ensure t)

;; disable modes for big files
(add-hook 'prog-mode-hook
          (lambda ()
              (when (> (buffer-size) 40000)
                  (turn-off-smartparens-mode)
                  (turn-off-show-smartparens-mode)
                  (company-mode 0)
                  (flycheck-mode 0))))

;;(global-set-key (kbd "<f2>") 'imenu-list))
(setq auto-window-vscroll nil)

;; language tool
(use-package langtool
    :ensure t
    :bind (("C-C C-c w" . langtool-check)
           ("C-C C-c W" . langtool-check-done)
           ("C-C C-c l" . langtool-switch-default-language)
           ("C-C C-c 4" . langtool-show-message-at-point)
           ("C-C C-c c" . langtool-correct-buffer))
    :config
    (setq langtool-language-tool-jar "~/bin/LanguageTool/languagetool-commandline.jar")
    langtool-default-language "en-US"
    langtool-disabled-rules '("WHITESPACE_RULE"
                              "EN_UNPAIRED_BRACKETS"
                              "COMMA_PARENTHESIS_WHITESPACE"
                              "EN_QUOTES"))
;; evil mode
;; (use-package evilual
;;     :ensure t
;;     :init
;;     (evil-mode 1))

;; emacs surround
(use-package emacs-surround
    ;;:ensure t
    :bind((("C-q" . emacs-surround))))

;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; elisp settings
(use-package elisp-slime-nav
    :ensure t
    :init
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode)))

;; exec shell
;; some magic happens here
;; DO NOT EDIT THIS SHIT!!!!!!!!!!!!!!!!!
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

(use-package exec-path-from-shell
    :ensure t
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))
;; DO NOT EDIT THIS SHIT ends here
(load custom-file)
;;; init.el ends here
