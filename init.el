;;; package --- My emaacs init-file --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(setq load-prefer-newer t)

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")
;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; Performance hacks
(setq message-log-max t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(defvar voloyev--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun voloyev-reset-file-handler-alist-h ()
  (setq file-name-handler-alist voloyev--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'voloyev-reset-file-handler-alist-h)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq frame-inhibit-implied-resize t)
(setq ffap-machine-p-known 'reject)
;; end of performance hacks
;; use zsh
(setq shell-file-name "/bin/zsh")
(package-install 'use-package)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Delete selection
(delete-selection-mode t)
;; desktop-save-mode
(desktop-save-mode 0)

;;Indent settings
(setq-default indent-tabs-mode nil)
(setq tab-width                  2)
(setq-default tab-width          2)
(setq-default standart-indent    2)
(setq-default lisp-body-indent   2)

;; css and sccs indent level
(setq css-indent-offset 2)
(setq scss-indent-offset 2)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

;; company mode
(use-package company
  :ensure t
  :init
  (global-company-mode t)
  :bind("C-<tab>" . company-complete))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t)

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
    :bind("C-c SPC g" . magit-status))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

(use-package toggle-quotes
  :ensure t
  :bind("C-'" . toggle-quotes))

(setq paradox-github-token (getenv "PARADOX"))

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
  :bind("C-c SPC d" . deadgrep))

(use-package frog-jump-buffer
  :ensure t
  :bind("C-c SPC j" . frog-jump-buffer))

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
(use-package looks-module)
(use-package ruby-module)
(use-package smartparens-module)
(use-package python-module)
(use-package js-module)
(use-package rust-module)
(use-package elixir-module)
(use-package settings-module)
(use-package go-module)
(use-package avy-module)
(use-package org-module)
(use-package evil-module)
(use-package lisp-module)
(use-package lsp-module)
;; custom plugins path

(load custom-file)
;;; init.el ends here
