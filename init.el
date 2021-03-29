;;; -*- lexical-binding: t -*-
;;; package --- My emaacs init-file --- Load the full configuration
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
;; -(setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6)

;; save customization in separate file
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq default-directory "~/")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (package-initialize)

(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package gcmh :init (gcmh-mode 1))

(use-package no-littering)

;; Performance hacks
(setq message-log-max t)
(setq large-file-warning-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq idle-update-delay 1)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
(setq make-backup-files         nil)
(setq auto-save-default         nil)
(setq auto-save-list-file-name  nil)
(setq inhibit-splash-screen       0)
(setq ingibit-startup-message     0)
(setq auto-window-vscroll       nil)
(setq x-select-enable-clipboard   t)
(setq search-highlight            t)
(setq query-replace-highlight     t)
(setq frame-title-format "GNU Emacs: %b")
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq frame-inhibit-implied-resize t)
(setq ffap-machine-p-known 'reject)
(when (memq window-system '(ns mac))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))
(setq shell-file-name "/bin/zsh")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

(xterm-mouse-mode        t)
(global-auto-revert-mode t)
(delete-selection-mode   t)
(desktop-save-mode       0)
(tool-bar-mode          -1)
(menu-bar-mode          -1)
(scroll-bar-mode        -1)

(fset 'yes-or-no-p 'y-or-n-p)

(cond ((memq window-system '(ns mac))
       (set-face-attribute 'default nil :font "mononoki Nerd Font 16"))
      ((memq window-system '(x))
       (set-face-attribute 'default nil :font "mononoki Nerd Font 14")))

(setq-default line-spacing 1)

(if (memq window-system '(ns mac))
    (progn
      (add-to-list 'default-frame-alist
                   '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist
                   '(ns-appearance . light))))

;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;;Indent settings
(setq-default indent-tabs-mode   nil)
(setq-default tab-width            2)
(setq-default standart-indent      2)
(setq-default lisp-body-indent     2)
(setq-default css-indent-offset    2)
(setq-default scss-indent-offset   2)
(setq-default python-indent-offset 4)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 12)
  (set-face-attribute 'mode-line nil  :height 80)
  (set-face-attribute 'mode-line-inactive nil :height 80))

(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

(use-package flycheck :init (global-flycheck-mode t))

(use-package company
    :init (setq company-idle-delay 0.1)
    :hook (after-init . global-company-mode))

(use-package direnv :defer t :config (direnv-mode))

;;Display the name of the current buffer in the title bar
(use-package fill-column-indicator
    :init
    (fci-mode 1)
    (setq fci-rule-width 3))

(use-package fixmee
    :defer t
    :config
    (require 'button-lock)
    (global-fixmee-mode 1))

(use-package whitespace
    :ensure nil
    :defer t
    :config
    (setq whitespace-line-column 250)
    (setq whitespace-display-mappings
          '((space-mark 32 [183] [46])
            (newline-mark 10 [8629 10])
            (tab-mark 9 [9655 9] [92 9])))
    :bind(("C-c SPC w s" . whitespace-mode)
          ("C-c SPC w c" . whitespace-cleanup)))

(use-package hydra :defer t)
(use-package avy :defer t)
(use-package multiple-cursors :defer t)

(defhydra hydra-multiple-cursors (global-map "C-c SPC m")
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

;; map of tagtables
(global-set-key (kbd "<f8>") 'visit-tags-table)

(use-package bookmark
    :defer t
    :ensure nil
    :config
    (setq bookmark-save-flag t)
    (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
    (when (file-exists-p (concat user-emacs-directory "bookmarks"))
      (bookmark-load bookmark-default-file t))
    :bind(("C-c SPC b s" . bookmark-set)
          ("C-c SPC b j" . bookmark-jump)
          ("C-c SPC b l" . bookmark-bmenu-list)))

(use-package emmet-mode
    :defer t
    :hook (web-mode  . emmet-mode)
    :hook (css-mode  . emmet-mode)
    :hook (scss-mode . emmet-mode))

(use-package magit
    :defer t
    :bind("C-c SPC g" . magit-status))

(use-package undo-fu
    :defer t
    :config
    (global-undo-tree-mode nil)
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "C-z")     'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z")   'undo-fu-only-redo))

(use-package toggle-quotes
    :bind("C-'" . toggle-quotes))

;; resize buffers
(global-set-key (kbd "C-c C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-c <right>") 'enlarge-window-horizontally)

(use-package dumb-jump
    :defer t
    :config
    (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    (setq dumb-jump-force-searcher 'rg))

(defhydra hydra-dump-jump (:exit t :hint nil global-map "C-, j")
  "
   ^Goto^                    ^Config^
   ----------------------------------------------------------
   [_o_] goto other window   [_z_] dumb-jump-go-prefer-external-other-window
   [_j_] goto                [_b_] dumb-jump-back
   [_q_] quick look          [_x_] dumb-jump-go-prefer-external
   [_i_] goto prompt
"
  ("o" dumb-jump-go-other-window)
  ("j" dumb-jump-go)
  ("q" dumb-jump-quick-look)
  ("i" dumb-jump-go-prompt)
  ("x" dumb-jump-go-prefer-external)
  ("z" dumb-jump-go-prefer-external-other-window)
  ("b" dumb-jump-back))

;; fzf
(use-package fzf
    :defer t
    :bind
    (("C-c SPC f f" . fzf)))

(use-package evil-nerd-commenter
    :defer t
    :bind (( "M-;"     . evilnc-comment-or-uncomment-lines)
           ( "C-c e l" . evilnc-quick-comment-or-uncomment-to-the-line)
           ( "C-c e c" . evilnc-copy-and-comment-lines)
           ( "C-c e p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package htmlize)

(use-package irony
    :defer t
    :hook (c++-mode)
    :hook (c-mode)
    :hook (objc-mode)
    :hook (irony-mode . irony-cdb-autosetup-compile-options))

(setq c-default-style "linux")

(use-package deadgrep
    :defer t
    :bind("C-c SPC d" . deadgrep))

(use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-envs '("LANG" "GPG_AGENT_INFO" "SSH_AUTH_SOCK"))))

(use-package easy-kill
    :defer t
    :config
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark))

(defhydra hydra-avy (global-map "C-;" :exit t :hint nil)
  "
  ^Line^       ^Region^        ^Goto^
  ----------------------------------------------------------
  [_y_] yank   [_Y_] yank      [_c_] timed char
  [_m_] move   [_M_] move      [_w_] word
  [_k_] kill   [_K_] kill      [_l_] line
                               [_;_] char
                               [_W_] any word
                               [_L_] end of line
"
  ("c" avy-goto-char-timer)
  (";" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(defhydra hydra-zoom (global-map "C-, SPC z")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-yasnippet (global-map "C-, SPC y")
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
           _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

;; themes
;; (setq custom-safe-themes t)
;; (add-to-list 'custom-theme-load-path "~/workspace/lisp/emacs-lisp/sexy-monochrome-theme")
;; (use-package sexy-monochrome-theme
;;     :ensure t
;;     :init
;;     (load-theme 'sexy-monochrome t)
;;     (enable-theme 'sexy-monochrome))

(use-package zenburn-theme :init (load-theme 'zenburn t))

(use-package lispy
    :defer t
    :bind(("C-c SPC SPC e i" . lispy-mode)))

(use-package geiser
    :defer t
    :init
    (setq geiser-default-implementation 'racket))

(use-package chruby :defer t)
(use-package rake :defer t)
(use-package bundler :defer t)
(use-package rspec-mode :defer t)
(use-package hyde :defer t)
(use-package easy-jekyll :defer t)

(use-package ruby-mode
    :ensure f
    :defer t
    :init   (setq ruby-insert-encoding-magic-comment nil)
    :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
    :hook (robe-mode)
    :hook (yard-mode)
    :bind(("C-c SPC SPC r r"      . inf-ruby-console-auto)
          ("C-c SPC SPC r h r"    . enh-ruby-mode)))

(use-package ruby-tools
    :defer t
    :init
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil))

(use-package slim-mode
    :defer t
    :mode ("\\.slim\\'" . slim-mode))

(use-package haml-mode
    :defer t
    :mode ("\\.haml\\'" . haml-mode))

(setq easy-jekyll-markdown-extension "markdown")
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'ruby-mode-hook 'chruby-use-corresponding)

;;;; js mode
(use-package vue-mode
    :defer t
    :mode ("\\.vue\\'" . vue-mode)
    :config
    (add-hook 'mmm-mode-hook
              (lambda ()
                (set-face-background 'mmm-default-submode-face nil))))

;;pretier
(use-package prettier-js
    :defer t
    :init
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.vue?\\'" . prettier-js-mode))))
    :hook (js-mode      . prettier-js-mode)
    :hook (vue-mode     . prettier-js-mode))

(use-package js-mode
    :ensure f
    :mode ("\\.js\\'" . js-mode)
    :mode ("\\.jsx\\'" . js-mode))

(custom-set-variables '(coffee-tab-width 2))
(custom-set-variables '(js2-basic-offset 2))
(custom-set-variables '(js-basic-offset 2))
(custom-set-variables '(jsx-basic-offset 2))
(custom-set-variables '(rjsx-basic-offset 2))
(custom-set-variables '(vue-basic-offset 2))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq vue-indent-level 2)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(global-set-key (kbd "C-c SPC ]") 'next-buffer)
(global-set-key (kbd "C-c SPC [") 'previous-buffer)

(use-package bufler
    :defer t
    :bind ("C-x C-b" . bufler))

(use-package ibuffer
    :defer t
    :ensure f
    ;; :bind ("C-x C-b" . ibuffer)
    :init
    (autoload 'ibuffer "ibuffer" "List buffers." t)
    (defalias 'list-buffers 'ibuffer)
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                (ibuffer-auto-mode t)))
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-vc :ensure t)

(use-package ace-window
    ;; x - delete window
    ;; m - swap windows
    ;; M - move window
    ;; c - copy window
    ;; j - select buffer
    ;; n - select the previous window
    ;; u - select buffer in the other window
    ;; c - split window fairly, either vertically or horizontally
    ;; v - split window vertically
    ;; b - split window horizontally
    ;; o - maximize current window
    ;; ? - show these command bindings
    :ensure t
    :config
    (setq aw-dispatch-always t)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :bind(("M-o" . ace-window)))

(use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package editorconfig
    :defer t
    :ensure t
    :config
    (editorconfig-mode t))

(use-package markdown-mode
    :defer t
    :init (setq markdown-command "mark")
    :mode ("\\.text\\'" . markdown-mode)
    :mode ("\\.markdown\\'" . markdown-mode)
    :mode ("\\.md\\'" . markdown-mode))

(use-package which-key
    :ensure t
    :config (which-key-mode t))

(use-package fix-word
    :ensure t
    :bind(("M-u" . fix-word-upcase)
          ("M-l" . fix-word-downcase)
          ("M-c" . fix-word-capitalize)))

(use-package es-mode
    :ensure t
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((elasticsearch . t)))
    :config (setq es-always-pretty-print t))

(use-package yaml-mode
    :defer t
    :ensure t
    :mode ("\\.yml\\'" . yaml-mode))

(use-package restclient
    :defer t
    :ensure t
    :mode ("\\.restc\\'" . restclient-mode))

(use-package bfbuilder
    :defer t
    :ensure t
    :mode ("\\.bf\\'" . bfbuilder-mode))

(use-package nasm-mode
    :defer t
    :ensure t)

(use-package diff-hl
    :ensure t
    :config
    :hook (dired-mode . diff-hl-dired-mode)
    :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(diff-hl-margin-mode t)
(diff-hl-dired-mode t)
(global-diff-hl-mode t)

(use-package volatile-highlights
    :ensure t
    :config
    (volatile-highlights-mode t))

(use-package projectile
    :ensure t
    :config
    (projectile-mode t)
    (define-key projectile-mode-map
        (kbd "C-c SPC p") 'projectile-command-map)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-mode-line
          '(:eval (format " Projectile[%s]"
                   (projectile-project-name))))
    (setq projectile-require-project-root t))
(add-hook 'projectile-mode-hook 'chruby-use-corresponding)

(setq projectile-project-root-files (remove "WORKSPACE" projectile-project-root-files))

(add-hook 'php-mode-hook (lambda () c-basic-offset 2))
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

(use-package yasnippet
    :defer t
    :config
    (message "Here!!!!")
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode))


(use-package highlight-indentation
    :defer t
    :bind (("<f9>" . highlight-indentation-mode)
           ("M-<f9>" . highlight-indentation-current-column-mode)))

(use-package ivy
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s"           . swiper)
          ("M-x"           . counsel-M-x)
          ("M-y"           . counsel-yank-pop)
          ("C-x b"         . ivy-switch-buffer)
          ("C-c SPC i d f" . counsel-describe-function)
          ("C-c SPC i d v" . counsel-describe-variable)))

(use-package ivy-rich :after ivy :init (ivy-rich-mode 1))
(use-package prescient :defer t)
(use-package ivy-prescient :defer t)
(use-package company-prescient :defer t :after company)

(use-package web-mode
    :defer t
    :config
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-php-indent-offset 2)
    ;;snippets fo autoclose tags
    (setq web-mode-extra-snippets
          '(("erb" . (("name" . ("beg" . "end"))))))
    (setq web-mode-extra-auto-pairs
          '(("erb" . (("open" "close")))))
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

    (add-hook 'web-mode-hook 'emmet-mode))

;; crystal mode
(use-package crystal-mode :defer t)

(use-package cider :defer t)

(use-package clojure-mode
    :defer t
    :config
    (setq clojure-indent-style 'always-indent))

;; (use-package org-plus-contrib
;;     :ensure t)
(use-package org
    :defer f
    :config
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (setq org-agenda-files (list (getenv "ORG_TODO_PATH")))
    (add-hook 'org-mode-hook 'toggle-truncate-lines)
    (setq org-src-fontify-natively nil)
    (setq org-html-htmlize-output-type nil) ;; output without
    (defface org-block
        '((t (:background "#000000")))
      "Face used for the source block background.")
    :bind(("C-c SPC o l" . org-store-link)
          ("C-c SPC o a" . org-agenda)
          ("C-c SPC o c" . org-capture)
          ("C-c SPC o b" . org-iswitchb)))

(use-package walkman
    :defer t
    :config
    (setq walkman-keep-headers t))

(use-package systemd :defer t)
(use-package ox-reveal :defer t)

(add-to-list 'exec-path "~/.local/share/elixir-ls/release")
(use-package lsp-mode
    :defer t
    :diminish lsp-mode
    :init
    (setq lsp-auto-guess-root t)
    (setq lsp-diagnostic-package 'flycheck) ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq lsp-prefer-capf t)
    (setq lsp-enable-indentation nil)
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    :hook ((vue-mode    . lsp)
           (ruby-mode   . lsp)
           (rust-mode   . lsp)
           (js-mode     . lsp)
           (python-mode . lsp)
           (go-mode     . lsp)
           (latex-mode  . lsp)
           (elixir-mode . lsp)
           (lsp-mode    . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred))
(setq lsp-log-io t)
(setq lsp-keymap-prefix "C-c SPC .")
(global-set-key (kbd "C-c SPC .") 'lsp-mode-map)

(add-hook 'lsp-before-initialize-hook 'chruby-use-corresponding)

(use-package lsp-ui
    :commands lsp-ui-mode
    :after lsp-mode
    :config
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-sideline-show-diagnostics nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-doc-enable nil))

(global-set-key (kbd "C-c SPC f d") 'lsp-ui-peek-find-definitions)
(global-set-key (kbd "C-c SPC f r") 'lsp-ui-peek-find-references)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :after lsp)
(use-package lsp-treemacs
    :after lsp
    :commands lsp-treemacs-errors-list
    :config
    (setq treemacs-no-png-images t))

(use-package lsp-dart
    :after lsp
    :defer t
    :hook (dart-mode . lsp-deferred))

(use-package dap-mode
    :defer t
    :init
    (require 'dap-python)
    (require 'dap-ruby)
    (require 'dap-go))

;;;; go settings
(use-package go-mode)

(defun voloyev/lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'voloyev/lsp-go-install-save-hooks)




;;;;; elixir module
(use-package elixir-mode
    :ensure t
    :init
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
    :config
    (add-to-list
     'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
    (add-to-list
     'auto-mode-alist '("\\.ex\\'" . elixir-mode))
    (add-to-list
     'auto-mode-alist '("\\.exs\\'" . elixir-mode)))

;; haskell
(use-package intero
    :hook (haskell-mode . intero-mode))

;;;; rust settings
(use-package rust-mode
    :mode "\\.rs\\'"
    :config (setq rust-format-on-save t))

(use-package smart-semicolon :defer t)

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :ensure t
    :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
    :mode (("\\.toml\\'" . toml-mode)))

;;;;; python
(use-package python-black
    :demand t
    :after python-mode
    :bind("C-c SPC SPC p b r" . python-black-region)
    :bind("C-c SPC SPC p b b" . python-black))

(use-package python-mode :defer t) 

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'python-flake8)))

(use-package pyvenv)
(use-package auto-virtualenv
    :hook ((python-mode . auto-virtualenv-set-virtualenv)
           ;; Activate on changing buffers
           (window-configuration-change . auto-virtualenv-set-virtualenv)
           ;; Activate on focus in
           (focus-in-hook . auto-virtualenv-set-virtualenv)
           (python-mode . highlight-indentation-mode)
           (python-mode . pyvenv-mode)))


;;;;;;; lisp
(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(use-package sly
    :defer t
    :config
    (setq org-babel-lisp-eval-fn 'sly-eval)
    (setq inferior-lisp-program "ros -Q run")
    :hook (lisp-mode . sly-editing-mode))

;; (use-package racket-mode
;;     :ensure t)

(use-package smartparens
    :ensure t)

(require 'smartparens-config)
(--each '(restclient-mode-hook
          js-mode-hook
          vue-mode-hook
          js2-mode-hook
          python-mode-hook
          ruby-mode-hook
          markdown-mode-hook
          org-mode-hook
          rust-mode-hook
          toml-mode-hook
          cc-mode-hook
          lisp-mode-hook
          haml-mode-hook
          c-mode-hook
          go-mode-hook
          elixir-mode-hook
          enh-ruby-mode-hook
          crystal-mode-hook
          slim-mode-hook
          yaml-mode-hook
          nginx-mode-hook
          scss-mode-hook
          web-mode-hook
          emacs-lisp-mode-hook
          clojure-mode-hook
          conf-mode-hook
          dockerfile-mode-hook
          haskell-mode-hook
          erlang-mode-hook
          irony-mode-hook
          geiser-mode-hook
          sh-mode-hook)
  (add-hook it #'smartparens-mode))

(show-smartparens-global-mode +1)

;; deal with escaping
(setq sp-escape-quotes-after-insert nil)

(eval-after-load 'flycheck
  (if (display-graphic-p)
      (flycheck-popup-tip-mode)
      (flycheck-pos-tip-mode)))

;; eval langs in go
(with-eval-after-load 'org
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
  (org-clock-persistence-insinuate))


(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package dockerfile-mode :ensure t)

(use-package docker
  :bind ("C-c SPC SPC d" . docker))

(use-package helpful
    :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-callable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


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

(load custom-file)
;;; init.el ends here
