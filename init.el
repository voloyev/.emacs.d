;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")t)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/snails") ; remove when it will appear in melpa

;; use zsh
(setq shell-file-name "/bin/zsh")
;; activate installed packages
(package-initialize)

(package-install 'use-package)
(require 'use-package)
(use-package snails)

;;; List of required modules
(use-package ruby-module)
(use-package smartparens-module)
(use-package web-mode-module)
(use-package yasnippet-module)
(use-package helm-module)
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
(use-package elfeed-module)
(use-package simplenotes-module)
;; custom plugins path

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
    :ensure t
    :init
    (global-company-mode t)
    (with-eval-after-load 'company
      (add-hook 'after-init-hook 'global-company-mode)
      (add-to-list 'company-backends 'company-robe)
      (add-to-list 'company-backends 'company-jedi))
    :bind("C-<tab>" . company-complete))

(use-package company-quickhelp          ; Documentation popups for Company
    :ensure t
    :defer t
    :init
    (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; multiple cursors
(use-package multiple-cursors :ensure t)

;;global line mode
(global-hl-line-mode)

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
    :hook (web-mode-hook)
    :hook (css-mode-hook))

(use-package vimish-fold
    :bind(("C-c n f" . vimish-fold)
          ("C-c n t" . vimish-fold-toggle)
          ("C-c n u" . vimish-fold-unfold)
          ("C-c n v" . vimish-fold-delete)))

(use-package magit
    :bind("C-x g" . magit-status))

(use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode t)
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t))

;; c-mode settings
(setq c-default-style "linux")

;; expand region mode
(use-package expand-region
    :bind("C-=" . er/expand-region))

(use-package quickrun
    :ensure t)

(use-package golden-ratio
    :ensure t
    :bind("C-c & g" . golden-ratio-mode))

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

;; language tool
(use-package langtool
    :ensure t
    :bind (("C-C C-c w" . langtool-check)
           ("C-C C-c W" . langtool-check-done)
           ("C-C C-c l" . langtool-switch-default-language)
           ("C-C C-c 4" . langtool-show-message-at-point)
           ("C-C C-c c" . langtool-correct-buffer))
    :config
    (setq langtool-language-tool-jar
          "~/bin/LanguageTool/languagetool-commandline.jar")
    langtool-default-language "en-US"
    langtool-disabled-rules '("WHITESPACE_RULE"
                              "EN_UNPAIRED_BRACKETS"
                              "COMMA_PARENTHESIS_WHITESPACE"
                              "EN_QUOTES"))

;; emacs surround
(use-package emacs-surround
    ;;:ensure t
    :bind((("C-q" . emacs-surround))))

;; Emacs key bindings
(use-package evil-nerd-commenter
    :ensure t
    :bind (( "M-;" .  evilnc-comment-or-uncomment-lines)
           ( "C-c e l" . evilnc-quick-comment-or-uncomment-to-the-line)
           ( "C-c e c" . evilnc-copy-and-comment-lines)
           ( "C-c e p" . evilnc-comment-or-uncomment-paragraphs)))

;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(use-package htmlize
    :ensure t)

(use-package multi-term
    :ensure t)

(use-package irony
    :ensure t)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package calfw-cal
    :ensure t)

(use-package calfw
    :ensure t)

(use-package calfw-org
    :ensure t)

(use-package howm
    :ensure t)

(use-package calfw-howm
    :ensure t)

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

(use-package ivy-posframe
    :ensure t
    :config
    (setq ivy-posframe-display-functions-alist
          '((t . ivy-posframe-display)))
    (ivy-posframe-mode t))

(use-package dimmer
    :ensure t
    :config
    (dimmer-mode t))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "GOPATH")

(load custom-file)
;;; init.el ends here
