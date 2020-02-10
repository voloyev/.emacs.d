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

(use-package hydra
    :ensure t)

(use-package avy
    :ensure t)

;; multiple cursors
(use-package multiple-cursors
    :ensure t)
(defhydra hydra-multiple-cursors (global-map "C-c SPC m c" :hint nil)
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
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("LANG" "GPG_AGENT_INFO" "SSH_AUTH_SOCK"))))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package lsp-mode
    :ensure t
    :init
    ;; (setq lsp-keymap-prefix "C-c C-c l")
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq lsp-enable-indentation nil)
    :hook (js-mode   . lsp-deferred)
    :hook (vue-mode  . lsp-deferred)
    :hook (ruby-mode . lsp-deferred)
    :commands (lsp lsp-deferred))


(use-package lsp-ivy
    :ensure t
    :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
    :ensure t
    :commands lsp-treemacs-errors-list)

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :ensure t
    :commands company-lsp
    :ensure t)

(defhydra hydra-lsp (global-map "C-c SPC l" :exit t)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_._] definition   [_t_] type            [_R_] rename
 [_x_] execute action   [_M-s_] describe session   [_/_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("." lsp-ui-peek-find-definitions)
  ("/" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("R" lsp-rename)
  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)
  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(defhydra hydra-avy (global-map "C-c SPC ;" :exit t :hint nil)
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

(defhydra hydra-yasnippet (:color blue :hint nil)
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
            irony-mode-hook)
    (add-hook it 'turn-on-smartparens-mode)))

;; deal with escaping
(setq sp-escape-quotes-after-insert nil)

(use-package evil
    :ensure t
    :bind
    (("C-c SPC e l" . evil-local-mode)
     ("C-c SPC e g" . evil-mode)))

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

;; gorepl-mode
(use-package gorepl-mode
    :ensure t
    :init
    (add-hook 'go-mode-hook #'gorepl-mode))

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
(use-package company-racer
    :ensure t)

(add-to-list 'company-backends 'company-racer)

(use-package rust-mode
    :mode "\\.rs\\'"
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq rust-format-on-save t))

(use-package flycheck-rust
    :ensure t
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
    :commands racer-mode
    :diminish racer-mode
    :ensure t)

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
    :mode (("\\.toml\\'" . toml-mode)))

;;;; Lisp settings
;;; package -- Summary
;;; Commentary:
;;; Lisp settings
;;; Code:
;; (load (expand-file-name "~/.roswell/helper.el"))
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

(use-package racket-mode
    :ensure t)

(use-package lispy
    :ensure t
    :bind(("C-c SPC e i" . lispy-mode)))

(use-package geiser
    :ensure t
    :init
    (setq geiser-default-implementation 'racket))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; List of required modules
(use-package looks-module)
(use-package settings-module)
(use-package ruby-module)
(use-package python-module)
(use-package js-module)
(use-package org-module)
;; custom plugins path

(load custom-file)
;;; init.el ends here
