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
(require 'elixir-mode)

;; Achievements mode
(use-package achievements
    :config
    (achievements-mode 1))

;; cask
;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)

;; Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Delete selection
(delete-selection-mode t)

;; use bash
(setq shell-file-name "/bin/bash")

;; Switch window
(global-set-key (kbd "C-x o") 'switch-window)

;; company mode
(use-package company
    :init
    (with-eval-after-load 'company
        (add-hook 'after-init-hook 'global-company-mode)
        (add-to-list 'company-backends 'company-robe)
        (add-to-list 'company-backends 'tern)
        (add-to-list 'company-backends 'company-go)
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

(use-package smartparens
    :config
    (show-smartparens-global-mode 1))

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
    (projectile-rails-global-mode))

;; Add haml and yaml modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; slime
(setq slime-lisp-implementations
      '((closure ("lein" "repl"))
        (sbcl ("/usr/local/bin/sbcl"))))
(setq slime-contribs '(slime-fancy))

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
    :bind (("\C-cl" . nlinum-mode))
    :init
    (add-hook 'lisp-mode-hook 'nlinum-mode)
    (add-hook 'c-mode-hook 'nlinum-mode)
    (add-hook 'java-mode-hook 'nlinum-mode)
    (add-hook 'web-mode-hook 'nlinum-mode)
    (add-hook 'emacs-lisp-mode-hook 'nlinum-mode))

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
(require 'bookmark)
(setq bookmark-save-flag t) ;; автоматически сохранять закладки в файл
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
(global-set-key (kbd "C-c M-b") 'bookmark-set)
(global-set-key (kbd "C-c & b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

;; whichkey
(use-package which-key
    :config
    (which-key-mode t))

;; org-mode
(use-package org-install
    :init
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (setq org-agenda-files (list "~/Documents/todo.org"))
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
(global-set-key (kbd "<f5>") 'whitespace-mode)
(global-set-key (kbd "C-c <f5>") 'whitespace-cleanup)

;; evil modes
;;(global-set-key (kbd "<f6>") 'evil-mode)

;; emmet mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; calendar app
(require 'calfw)
(require 'calfw-org)

;;(put 'upcase-region 'disabled nil)

;; work mouse in terminal
(xterm-mouse-mode t)

;; vimish folds
(use-package vimish-fold
    :bind(("C-c v f" . vimish-fold)
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

(unless (display-graphic-p)
    (add-to-list 'default-frame-alist '(background-color . "#000000")))

;; email wanderlust
(autoload 'wl "wl" "Wanderlust" t)

;; neotree
(use-package  neotree
    :bind(("<f12>" . neotree-projectile-action)
          ("M-<f12>" . neotree-hide))
    :config
    (setq neo-theme  'arrow)
    (setq neo-smart-open t))

;;lein exec path
(add-to-list 'exec-path "/home/nuncostans/Programs/leiningen")

;;quickrun
(require 'quickrun)

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
(use-package ido
    :config
    (ido-mode t))

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

(global-set-key (kbd "<f7>") 'project-explorer-open)
;; god mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'magit-mode)
(add-to-list 'god-exempt-major-modes 'undo-tree-mode)
(add-to-list 'god-exempt-major-modes 'project-explorer-mode)

;; disable modes for big files
(add-hook 'prog-mode-hook
          (lambda ()
            (when (> (buffer-size) 40000)
                (turn-off-smartparens-mode)
                (turn-off-show-smartparens-mode)
                (company-mode 0))))

;; visual bookmarks
(setq bm-restore-repository-on-load t)
(use-package bm
         :ensure t
         :demand t

         :init
         ;; restore on load (even before you require bm)
         (setq bm-restore-repository-on-load t)
         (setq bm-buffer-restore-all t)


         :config
         ;; Allow cross-buffer 'next'
         (setq bm-cycle-all-buffers t)

         ;; where to store persistant files
         (setq bm-repository-file "~/.emacs.d/bm-repository")

         ;; save bookmarks
         (setq-default bm-buffer-persistence t)

         ;; Loading the repository from file when on start up.
         (add-hook 'after-init-hook 'bm-repository-load)

         ;; Restoring bookmarks when on file find.
         (add-hook 'find-file-hooks 'bm-buffer-restore-all)

         ;; Saving bookmarks
         (add-hook 'kill-buffer-hook #'bm-buffer-save)

         ;; Saving the repository to file when on exit.
         ;; kill-buffer-hook is not called when Emacs is killed, so we
         ;; must save all bookmarks first.
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)
         (add-hook 'bm-annotate-on-create 'bm-toggle)

         ;; Restoring bookmarks
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


         :bind (("<f6>" . bm-next)
                ("S-<f6>" . bm-previous)
                ("C-<f6>" . bm-toggle)
                ("C-c C-<f6>" . bm-show-all)))
(dumb-jump-mode)

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :config
    (dashboard-setup-startup-hook)
    (add-to-list 'dashboard-items '(agenda) t)
    (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5))))

(use-package ztree
    :bind (("C-c C-c z" . ztree-dir)))

;; beacon
(beacon-mode 1)

;; fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)
    (add-hook 'racket-mode-hook #'parinfer-mode)))
;; save customization in separate file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)
;;; init.el ends here
