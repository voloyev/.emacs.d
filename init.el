;;; package --- My emaacs init-file --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:

;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(exec-path-from-shell-copy-env "GOPATH")
(setq message-log-max t)

(require 'package)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)

  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")t)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")

;; use zsh
(setq shell-file-name "/bin/zsh")
;; activate installed packages
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun recompile-config ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun er-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'er-remove-elc-on-save)

;; company mode
(use-package company
    :ensure t
    :init
    (global-company-mode t)
    :bind("C-<tab>" . company-complete))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Delete selection
(delete-selection-mode t)

;; desktop-save-mode
(desktop-save-mode 0)

(use-package company-quickhelp          ; Documentation popups for Company
    :ensure t
    :defer t
    :init
    (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; multiple cursors
(use-package multiple-cursors
    :ensure t)

;;projectile
(use-package projectile
    :ensure t
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

;; map of tagtables
(global-set-key (kbd "<f8>") 'visit-tags-table)
" | Combo | Function         | Description                |"
" |-------+------------------+----------------------------|"
" | <f8>  | visit-tags-table | Loads tags                 |"
" | M-.   | find-tag         | Jumps to the specified tag |"
" | C-M-. | pop-tag-mark     | Jumps back                 |"

(use-package bookmark
    :init
  (setq bookmark-save-flag t)
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
  (when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
  :bind(("C-c & M-b" . bookmark-set)
        ("C-c & b"   . bookmark-jump)
        ("<f4>"      . bookmark-bmenu-list)))

(use-package emmet-mode
    :ensure t
    :hook (web-mode)
    :hook (css-mode)
    :hook (scss-mode))

(use-package magit
    :ensure t
    :bind("C-x g" . magit-status))

(use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-auto-save-history t))

(use-package quickrun
    :ensure t)

(use-package toggle-quotes
    :ensure t
    :bind("C-'" . toggle-quotes))

(defvar paradox-token
  (getenv "PARADOX"))

(setq paradox-github-token 'paradox-token)

;; flyspell
(use-package flyspell
    :config
  (flyspell-mode t))

;; resize buffers
(global-set-key (kbd "C-c C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-c <right>") 'enlarge-window-horizontally)

(use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g q" . dumb-jump-quick-look)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window)
           ("M-g b" . dumb-jump-back))
    :config
    (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    (setq dumb-jump-force-searcher 'rg)
    :ensure t)

;; fzf
(use-package fzf
    :ensure t
    :bind
    (("C-x f" . fzf)))

;; disable modes for big files
(add-hook 'prog-mode-hook
          (lambda ()
            (when (> (buffer-size) 40000)
              (turn-off-smartparens-mode)
              (turn-off-show-smartparens-mode)
              (company-mode 0)
              (flycheck-mode 0))))

(use-package evil-nerd-commenter
    :ensure t
    :bind (( "M-;"     .  evilnc-comment-or-uncomment-lines)
           ( "C-c e l" . evilnc-quick-comment-or-uncomment-to-the-line)
           ( "C-c e c" . evilnc-copy-and-comment-lines)
           ( "C-c e p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package htmlize
    :ensure t)

(use-package irony
    :ensure t
    :hook (c++-mode)
    :hook (c-mode)
    :hook (objc-mode)
    :hook (irony-mode . irony-cdb-autosetup-compile-options))

(setq c-default-style "linux")

(use-package deadgrep
    :ensure t
    :bind("C-c o SPC" . deadgrep))

(use-package frog-jump-buffer
    :ensure t
    :bind("C-c SPC" . frog-jump-buffer))

(use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

(use-package easy-kill
    :ensure t
    :config
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; List of required modules
(use-package ruby-module)
(use-package smartparens-module)
(use-package web-mode-module)
(use-package yasnippet-module)
;(use-package helm-module)
(use-package ivy-module)
(use-package python-module)
(use-package highlight-indentation-mode-module)
(use-package looks-module)
(use-package themes-module)
(use-package js-module)
(use-package rust-module)
(use-package crystal-module)
(use-package elixir-module)
(use-package settings-module)
(use-package go-module)
(use-package clojure-module)
(use-package avy-module)
(use-package org-module)
(use-package evil-module)
(use-package lisp-module)
(use-package indent-module)
(use-package hydra-module)
(use-package lsp-module)
(use-package simplenotes-module)
;; custom plugins path

(load custom-file)
;;; init.el ends here
