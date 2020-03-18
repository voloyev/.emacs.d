;;; -*- lexical-binding: t -*-
;;; package --- My emaacs init-file --- Load the full configuration
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/plugins")
(package-install 'use-package)
(use-package gcmh
    :ensure t)

(gcmh-mode 1)

;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(xterm-mouse-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(desktop-save-mode 0)

;; Performance hacks
(setq message-log-max t)
(setq large-file-warning-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq idle-update-delay 1)

(defvar voloyev--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq redisplay-dont-pause t)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))

(package-quickstart-refresh)
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

(when (memq window-system '(ns mac))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; use zsh
(setq shell-file-name "/bin/zsh")
(package-install 'use-package)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;Indent settings
(setq-default indent-tabs-mode nil)
(setq tab-width                  2)
(setq-default tab-width          2)
(setq-default standart-indent    2)
(setq-default lisp-body-indent   2)
(setq-default css-indent-offset  2)
(setq-default scss-indent-offset 2)

(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

(defun set-font ()
  "Set font for operating system."
  (cond ((memq window-system '(ns mac)) "Inconsolata 17")
        ((memq window-system '(x)) "Inconsolata 16")))

(if (memq window-system '(ns mac))
    (progn
      (add-to-list 'default-frame-alist
                   '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist
                   '(ns-appearance . light))))

(set-face-attribute 'default nil :font (set-font))
(set-face-attribute 'mode-line nil :font "Inconsolata 14")

(set-frame-font (set-font))
(setq-default line-spacing 1)

(setq ring-bell-function 'ignore)
;; (setq-default with-editor-emacsclient-executable "emacsclient")

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   0)
(setq ingibit-startup-message 0)

(setq auto-window-vscroll nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode   -1)
(setq scroll-step 1)

;; short answer
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search result
(setq search-highlight        t)
(setq query-replace-highlight t)
(setq frame-title-format "GNU Emacs: %b")


;;Display the name of the current buffer in the title bar
(use-package fill-column-indicator
    :ensure t
    :init
    (fci-mode 1)
    (setq fci-rule-width 3))

(use-package fixmee
    :ensure t
    :init (require 'button-lock)
    :config (global-fixmee-mode 1))

;;whitespace
(use-package whitespace
    :init
  (setq whitespace-line-column 250)
  (setq whitespace-display-mappings
        '((space-mark 32 [183] [46])
          (newline-mark 10 [8629 10])
          (tab-mark 9 [9655 9] [92 9])))
  :bind(("C-c SPC w s" . whitespace-mode)
        ("C-c SPC w c" . whitespace-cleanup)))

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

(use-package hydra
    :ensure t)

(use-package avy
    :ensure t)

(use-package company-nginx
    :ensure t
    :config
    (eval-after-load 'nginx-mode
      '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

;; multiple cursors
(use-package multiple-cursors
    :ensure t)

(defhydra hydra-multiple-cursors ()
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
(global-set-key (kbd "C-c SPC m") 'hydra-multiple-cursors/body)

;; map of tagtables
(global-set-key (kbd "<f8>") 'visit-tags-table)

(use-package bookmark
    :init
  (setq bookmark-save-flag t)
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
  (when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
  :bind(("C-c SPC b s" . bookmark-set)
        ("C-c SPC b j" . bookmark-jump)
        ("C-c SPC b l" . bookmark-bmenu-list)))

(use-package emmet-mode
    :ensure t
    :hook (web-mode)
    :hook (css-mode)
    :hook (scss-mode))

(use-package magit
    :ensure t
    :bind("C-c SPC g" . magit-status))

(use-package undo-fu
    :ensure t
    :config
    (global-undo-tree-mode nil)
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x u"))
    (global-set-key (kbd "C-x u")   'undo-fu-only-undo)
    (global-set-key (kbd "C-z")     'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z")   'undo-fu-only-redo))

(use-package toggle-quotes
    :ensure t
    :bind("C-'" . toggle-quotes))

(use-package paradox
    :ensure t
    :init
    (paradox-enable))

;; resize buffers
(global-set-key (kbd "C-c C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-c <right>") 'enlarge-window-horizontally)

(use-package dumb-jump
    :bind (("C-c SPC j o" . dumb-jump-go-other-window)
           ("C-c SPC j j" . dumb-jump-go)
           ("C-c SPC j q" . dumb-jump-quick-look)
           ("C-c SPC j i" . dumb-jump-go-prompt)
           ("C-c SPC j x" . dumb-jump-go-prefer-external)
           ("C-c SPC j z" . dumb-jump-go-prefer-external-other-window)
           ("C-c SPC j b" . dumb-jump-back))
    :config
    (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    (setq dumb-jump-force-searcher 'rg)
    :ensure t)

(defhydra hydra-dump-jump (:exit t)
  ("o" dumb-jump-go-other-window)
  ("j" dumb-jump-go)
  ("q" dumb-jump-quick-look)
  ("i" dumb-jump-go-prompt)
  ("x" dumb-jump-go-prefer-external)
  ("z" dumb-jump-go-prefer-external-other-window)
  ("b" dumb-jump-back))
(global-set-key (kbd "C-c SPC j") 'hydra-dump-jump/body)

;; fzf
(use-package fzf
    :ensure t
    :bind
    (("C-c SPC f f" . fzf)))

(use-package evil-nerd-commenter
    :ensure t
    :bind (( "M-;"     . evilnc-comment-or-uncomment-lines)
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

(use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-envs '("LANG" "GPG_AGENT_INFO" "SSH_AUTH_SOCK"))))

(use-package easy-kill
    :ensure t
    :config
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark))

(use-package flycheck
    :ensure t
    :init
    (global-flycheck-mode)
    :config
    (flycheck-pos-tip-mode nil)
    (setq flycheck-checker-error-threshold nil))

(use-package flycheck-pos-tip
    :ensure t)

(use-package flycheck-pycheckers
    :ensure t)

(use-package lsp-mode
    :ensure t
    :init
    :init (setq lsp-keymap-prefix "C-c SPC SPC l")
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq lsp-prefer-capf t)
    (setq lsp-enable-indentation nil)
    ;; :hook (js-mode   . lsp-deferred)
    :hook ((vue-mode  . lsp-deferred)
           (ruby-mode . lsp-deferred)
           (rust-mode . lsp-deferred)
           (lsp-mode  . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred))

(add-hook 'lsp-before-initialize-hook 'chruby-use-corresponding)

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :ensure t
    :commands company-lsp
    :ensure t)

(push 'company-lsp company-backends)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :ensure t)

(defhydra hydra-avy (global-map "C-c ;" :exit t :hint nil)
  ;; ^Line^       ^Region^        ^Goto^
  ;; ----------------------------------------------------------
  ;; [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
  ;; [_m_] move   [_M_] move      [_w_] word        [_W_] any word
  ;; [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line

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

(defhydra hydra-zoom (global-map "C-c SPC z")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-yasnippet ()
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
(global-set-key (kbd "C-c SPC SPC y") 'hydra-yasnippet/body)

;; haskell
(use-package intero
    :ensure t
    :init (intero-global-mode 1))

(use-package smartparens
    :ensure t
    :config
    (show-smartparens-global-mode 1)

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
      (add-hook it 'turn-on-smartparens-mode)))

;; deal with escaping
(setq sp-escape-quotes-after-insert nil)

(use-package evil
    :ensure t
    :bind
    (("C-c SPC SPC e l" . evil-local-mode)
     ("C-c SPC SPC e g" . evil-mode)))

(use-package evil-matchit
    :ensure t
    :config
    (add-hook 'evil-local-mode 'turn-on-evil-matchit-mode)
    (add-hook 'evil-mode 'turn-on-evil-matchit-mode))

;; use emacs keybindings in insert mode
(setcdr evil-insert-state-map nil)

;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evil-set-initial-state 'eshell-mode 'emacs)

;; themes
;; (setq custom-safe-themes t)
;; (add-to-list 'custom-theme-load-path "~/workspace/lisp/emacs-lisp/sexy-monochrome-theme")
(use-package sexy-monochrome-theme
    :ensure t
    :init
    (load-theme 'sexy-monochrome t)
    (enable-theme 'sexy-monochrome))

;;;; go settings
(use-package go-mode
    :ensure t)

(use-package company-go
    :ensure t)

(use-package flycheck-gometalinter
    :ensure t
    :config
    (progn
      (flycheck-gometalinter-setup)))

;; company mode
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-go))
            (company-mode)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))


(add-to-list 'load-path (concat
                         (getenv "GOPATH")
                         "/src/github.com/golang/lint/misc/emacs"))
(use-package golint
    :ensure t)

;;;;; elixir module
;; Highlights *.elixir2 as well
(use-package elixir-mode
    :ensure t
    :init
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
    :config
    (add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)))

(use-package alchemist
    :ensure t
    :init
    ;; (setq alchemist-mix-command "mix")
    ;; (setq alchemist-mix-test-task "espec")
    (setq alchemist-key-command-prefix (kbd "C-c SPC e"))
    (setq alchemist-mix-test-default-options '()) ;; default
    (setq alchemist-iex-program-name "iex") ;; default: iex
    (setq alchemist-execute-command "elixir") ;; default: elixir
    (setq alchemist-compile-command "elixirc") ;; default: elixirc
    (setq alchemist-hooks-compile-on-save t)
    (setq alchemist-goto-erlang-source-dir "~/source/otp")
    (setq alchemist-goto-elixir-source-dir "~/source/elixir"))

(use-package flycheck-credo
    :ensure t)

(use-package flycheck-mix
    :ensure t
    :init
    (flycheck-mix-setup))

(eval-after-load 'flycheck
  '(flycheck-credo-setup))

(add-hook 'elixir-mode-hook 'flycheck-mode)

;;;; rust settings
(use-package rust-mode
    :mode "\\.rs\\'"
    :ensure t
    :init
    (setq rust-format-on-save t))

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :ensure t
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
    :ensure t
    :mode (("\\.toml\\'" . toml-mode)))

;;;;; python
(use-package python-black
    :demand t
    :ensure t
    :after python
    :bind("C-c SPC SPC p b r" . python-black-region))

(use-package python-mode
    :ensure t)

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))

(use-package poetry
    :ensure t
    :config (poetry-tracking-mode t))

(use-package pyvenv
    :ensure t)

(use-package auto-virtualenvwrapper
    :ensure t
    :init
    (setq auto-virtualenvwrapper-verbose nil))

(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
;; Activate on changing buffers
(add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
(add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)
(add-hook 'python-mode-hook 'highlight-indentation-mode)

;;;;;;; lisp
(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(use-package sly
    :ensure t
    :init
    (setq org-babel-lisp-eval-fn 'sly-eval)
    (setq inferior-lisp-program "ros -Q run")
    :hook (lisp-mode . sly-editing))

;; (use-package elisp-slime-nav
;;     :ensure t
;;     :init
;;     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;       (add-hook hook 'elisp-slime-nav-mode)))

;; (use-package racket-mode
;;     :ensure t)

(use-package lispy
    :ensure t
    :bind(("C-c SPC SPC e i" . lispy-mode)))

(use-package geiser
    :ensure t
    :init
    (setq geiser-default-implementation 'racket))

;;;; ruby
(use-package rake
    :ensure t)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package ruby-mode
    :init   (setq ruby-insert-encoding-magic-comment nil)
    :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
    :hook (robe-mode)
    :hook (yard-mode)
    :bind(("C-c SPC SPC r r"      . inf-ruby-console-auto)
          ("C-c SPC SPC r h r"    . enh-ruby-mode)))

(use-package chruby
    :ensure t)

(use-package ruby-tools
    :ensure t
    :init
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil))

(use-package bundler
    :ensure t)

(use-package slim-mode
    :ensure t
    :mode ("\\.slim\\'" . slim-mode))

(use-package haml-mode
    :mode ("\\.haml\\'" . haml-mode))

;; jekyll mode
(use-package hyde
    :ensure t)

(use-package rspec-mode
    :ensure t)

(add-hook 'inf-ruby-mode-hook 'chruby-use-corresponding)

;;;; js mode
(use-package vue-mode
    :ensure t
    :mode ("\\.vue\\'" . vue-mode)
    :config
    (add-hook 'mmm-mode-hook
              (lambda ()
                (set-face-background 'mmm-default-submode-face nil))))

;;pretier
(use-package prettier-js
    :ensure t
    :init
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.vue?\\'" . prettier-js-mode))))
    :hook (js2-mode     . prettier-js-mode)
    :hook (js-mode      . prettier-js-mode)
    :hook (vue-mode     . prettier-js-mode)
    :hook (js2-jsx-mode . prettier-js-mode)
    :hook (rjsx-mode    . prettier-js-mode))

(use-package js-mode
    :mode ("\\.js\\'" . js-mode)
    :mode ("\\.jsx\\'" . js-mode)
    :hook (j2-minore-mode . js-mode))

;; (use-package elm-mode
;;     :ensure t
;;     :mode "\\.elm\\'"
;;     :config (setq elm-format-on-save t)
;;     :hook (smartparens-mode . elm-mode))

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
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-recency)))))

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
    :ensure t
    :config
    (editorconfig-mode t))

(use-package super-save
    :ensure t
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
    :ensure t
    :config (which-key-mode t))

;; upcase region
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
    :ensure t
    :mode ("\\.yml\\'" . yaml-mode))

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

(use-package auto-highlight-symbol
    :ensure t)

(add-hook 'js2-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'js2-jsx-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'elixir-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'ruby-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'rust-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'python-mode-hook 'auto-highlight-symbol-mode)

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
                   (projectile-project-name)))))

(add-hook 'php-mode-hook (lambda () c-basic-offset 2))
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

(use-package yasnippet
    :ensure t
    :config
    (add-to-list 'load-path
             "~/.emacs.d/snippets")
    (yas-load-directory "~/.emacs.d/snippets")
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode))


(use-package highlight-indentation
    :ensure t
    :bind (("<f9>" . highlight-indentation-mode)
           ("M-<f9>" . highlight-indentation-current-column-mode)))

(use-package ivy
    :ensure t
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s"           . swiper)
          ("M-y"           . counsel-yank-pop)
          ("C-x b"         . ivy-switch-buffer)
          ("C-c SPC i d f" . counsel-describe-function)
          ("C-c SPC i d v" . counsel-describe-variable)))


(use-package web-mode
    :ensure t
    :config
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
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-php-indent-offset 2)

    ;;snippets fo autoclose tags
    (setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))))
    (setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))))
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'"))))

;; crystal mode
(use-package crystal-mode
    :ensure t)

(use-package cider
    :ensure t
    :init
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package clojure-mode
    :ensure t
    :config
    (setq clojure-indent-style 'always-indent))

;; (use-package org-plus-contrib
;;     :ensure t)
(use-package org-install
    :init
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
    :ensure t
    :config
    (setq walkman-keep-headers t))

(use-package ox-reveal
    :ensure t)

;; eval langs in go
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
(org-clock-persistence-insinuate)

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
