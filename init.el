;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
         (add-to-list 'company-backends 'company-go)
         (add-to-list 'company-backends 'sly-company)
         (add-to-list 'company-backends 'company-jedy))
     :bind("C-<tab>" . company-complete)
     :config
     (global-company-mode t)
     (company-quickhelp-mode t))

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

;;multiple cursors
(use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C-." . mc/mark-next-like-this)
           ( "C-," . mc/mark-previous-like-this)
           ("C-c C-|" . mc/mark-all-like-this)
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
;; Add haml and yaml modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; slime
;;; (setq slime-lisp-implementations
;;       '((closure ("lein" "repl"))
;;         (sbcl ("/usr/local/bin/sbcl")))))
;; (setq slime-contribs '(slime-fancy))

;;sly
(use-package sly
    :ensure t
    :init
    (setq inferior-lisp-program "sbcl")
    (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
    (add-hook 'lisp-mode-hook 'sly-editing-mode))

;; racket
(use-package racket-mode
    :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Markdown
(use-package markdown-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (setq markdown-command "grip --export"))

;; line number
(use-package nlinum
    :bind (("C-c C-l" . nlinum-mode)))

;; gutter
(use-package git-gutter-fringe
    :config
    (global-git-gutter-mode t))

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
          ("C-c & b" . bookmark-jump)
          ("<f4>" . bookmark-bmenu-list)))

;; whichkey
(use-package which-key
    :config
    (which-key-mode t))

;; org-mode
(use-package org-install
    :init
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (setq org-agenda-files (list "~/Dropbox/org/todo.org" "~/Dropbox/org/tasks.org"))
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

;; evil modes
;;(global-set-key (kbd "<f6>") 'evil-mode)

;; emmet mode
(use-package emmet-mode
    :config
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))

;; calendar app
(use-package calfw)
(use-package calfw-org)

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

;; (unless (display-graphic-p)
;;     (add-to-list 'default-frame-alist '(background-color . "#000000")))

;; neotree
(use-package  neotree
    :bind(("<f12>" . neotree-projectile-action)
          ("M-<f12>" . neotree-hide))
    :config
    (setq neo-theme  'arrow)
    (setq neo-smart-open t))

;;lein exec path
(add-to-list 'exec-path "~/Programs/leiningen")

;;quickrun
(use-package quickrun)

;;golden ratio
(use-package golden-ratio
    :bind("C-c & g" . golden-ratio-mode))

;; toggle quotes
(use-package toggle-quotes
    :bind("C-'" . toggle-quotes))

;; css and sccs indent level
(setq css-indent-offset 2)
(setq scss-indent-offset 2)

;;paradox token
(defvar paradox-token
    (getenv "PARADOX"))
(setq paradox-github-token 'paradox-token)

;; ido
;; (use-package ido
;;     :config
;;     (ido-mode t))

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

;; god mode
(use-package god-mode
    :init
    (setq god-exempt-major-modes nil)
    (setq god-exempt-predicates nil)
    :config
    (add-to-list 'god-exempt-major-modes 'dired-mode)
    (add-to-list 'god-exempt-major-modes 'magit-mode)
    (add-to-list 'god-exempt-major-modes 'undo-tree-mode)
    (add-to-list 'god-exempt-major-modes 'project-explorer-mode)
;; bindings
    (global-set-key (kbd "<escape>") 'god-local-mode)
    (define-key god-local-mode-map (kbd "z") 'repeat)
    (define-key god-local-mode-map (kbd "i") 'god-local-mode))

(defun my-update-cursor ()
    (setq cursor-type (if
                          (or god-local-mode buffer-read-only)
                          'hbar
                          'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

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

;;ztree
(use-package ztree
    :bind (("C-c C-c z" . ztree-dir)))

;; beacon
(use-package beacon
    :config
    (beacon-mode 1)
    :bind
    (("C-c C-c C-c" . beacon-blink)))

;; fiplr
(use-package fiplr
    :ensure
    :config
    (setq fiplr-root-markers '(".git" ".svn"))
    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))))
    :bind
    (("C-x f" . fiplr-find-file)))

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
;; emacs surround
(add-to-list 'load-path "~/.emacs.d/plugins/")
(use-package emacs-surround
    ;;:ensure t
    :bind((("C-q" . emacs-surround))))
;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
(load custom-file)
;;; init.el ends here
