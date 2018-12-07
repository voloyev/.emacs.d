;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")t)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; activate installed packages
(package-initialize)
(require 'use-package)
(add-to-list 'load-path "~/.emacs.d/modules")

;;; List of required modules
(require 'ruby-module)
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
(require 'hydra-module)
(require 'org-module)
(require 'evil-module)
(require 'lisp-module)
(require 'indent-module)

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

;; desktop-save-mode
(desktop-save-mode 0)

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

;; multiple cursors
(use-package multiple-cursors :ensure t)
;; hydra for mc
(defhydra multiple-cursors-hydra (global-map "C-c m c" :hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))

;;global line mode
(global-hl-line-mode)

;;projectile
(use-package projectile
    :config
    (projectile-global-mode)
    (projectile-rails-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ;(setq projectile-indexing-method 'native)
    (setq projectile-enable-caching t)
    (setq projectile-mode-line
          '(:eval (format " Projectile[%s]"
                   (projectile-project-name)))))

;; map of tagtables
(global-set-key (kbd "<f8>") 'visit-tags-table)
" | Combo | Function         | Description                |"
" |-------+------------------+----------------------------|"
" | <f3>  | visit-tags-table | Loads tags                 |"
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
    :bind("C-x g" . magit-status)
    :config
    (global-auto-revert-mode t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

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
(global-set-key (kbd "<C-c C-c up>") 'shrink-window)
(global-set-key (kbd "<C-c C-c down>") 'enlarge-window)
(global-set-key (kbd "<C-c C-c left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-c C-c right>") 'enlarge-window-horizontally)

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
    (setq langtool-language-tool-jar "~/bin/LanguageTool/languagetool-commandline.jar")
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
