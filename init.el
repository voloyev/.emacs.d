;;; package --- My emaacs init-file --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
;; (require 'package)
;; (add-to-list 'package-archives

;;              '("melpa" . "http://melpa.org/packages/") t)

;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org-plus-contrib)

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")
;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(exec-path-from-shell-copy-env "GOPATH")
(setq message-log-max t)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)

  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; use zsh
(setq shell-file-name "/bin/zsh")
(straight-use-package 'use-package)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
;; Delete selection
(delete-selection-mode t)
;; desktop-save-mode
(desktop-save-mode 0)

;; company mode
(use-package company
    :straight t
    :init
    (global-company-mode t)
    :bind("C-<tab>" . company-complete))

(use-package company-quickhelp
    :straight t
    :defer t
    :init
    (add-hook 'global-company-mode-hook #'company-quickhelp-mode))
(straight-use-package 'inflections)

;; multiple cursors
(use-package multiple-cursors
    :straight t)

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
    :straight t
    :hook (web-mode)
    :hook (css-mode)
    :hook (scss-mode))

(use-package magit
    :straight t
    :bind("C-x g" . magit-status))

(use-package undo-tree
    :straight t
    :config
    (global-undo-tree-mode t))

(use-package toggle-quotes
    :straight t
    :bind("C-'" . toggle-quotes))

(defvar paradox-token
  (getenv "PARADOX"))

(setq paradox-github-token 'paradox-token)


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
    :straight t)

;; fzf
(use-package fzf
    :straight t
    :bind
    (("C-x f" . fzf)))

(use-package evil-nerd-commenter
    :straight t
    :bind (( "M-;"     .  evilnc-comment-or-uncomment-lines)
           ( "C-c e l" . evilnc-quick-comment-or-uncomment-to-the-line)
           ( "C-c e c" . evilnc-copy-and-comment-lines)
           ( "C-c e p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package htmlize
    :straight t)

(use-package irony
    :straight t
    :hook (c++-mode)
    :hook (c-mode)
    :hook (objc-mode)
    :hook (irony-mode . irony-cdb-autosetup-compile-options))

(setq c-default-style "linux")

(use-package deadgrep
    :straight t
    :bind("C-c SPC d" . deadgrep))

(use-package frog-jump-buffer
    :straight t
    :bind("C-c SPC j" . frog-jump-buffer))

(use-package exec-path-from-shell
    :straight t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

(use-package easy-kill
    :straight t
    :config
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; List of required modules
(use-package ruby-module)
(use-package smartparens-module)
(use-package web-mode-module)
(use-package yasnippet-module)
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
