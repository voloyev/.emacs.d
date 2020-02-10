;;; Package --- Summary
;;; Code:
;;; Commentary:
;;; fonts and general looks
(defun set-font ()
  "Set font for operating system."
  (cond ((memq window-system '(ns mac)) "Hack 14")
        ((memq window-system '(x)) "Hack 14")))

(set-face-attribute 'default nil :font (set-font))
(set-frame-font (set-font))
(setq line-spacing 0.2)

(setq ring-bell-function 'ignore)
(setq-default with-editor-emacsclient-executable "emacsclient")

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   0)
(setq ingibit-startup-message 0)

(setq auto-window-vscroll nil)

;; toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;;disable scrollbar
(scroll-bar-mode   -1)

;; scrolling
(setq scroll-step 1)

;; short answer
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search result
(setq search-highlight        t)
(setq query-replace-highlight t)
(setq frame-title-format "GNU Emacs: %b")

;; Use visual-line-mode in gfm-mode
(defun my-gfm-mode-hook ()
  (visual-line-mode 1))

(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;Display the name of the current buffer in the title bar
(use-package fill-column-indicator
    :straight t
    :init
    (fci-mode 1)
    (setq fci-rule-width 3))

(use-package nyan-mode
    :straight t
    :init
    (nyan-mode t)
    (nyan-start-animation))

(use-package fixmee
    :straight t
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

(use-package smart-mode-line
    :straight t
    :init
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup))

(use-package cider
    :straight t
    :init
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package clojure-mode
    :straight t
    :config
    (setq clojure-indent-style 'always-indent))

;; ;;lein exec path
;; (add-to-list 'exec-path "~/bin")

(provide 'looks-module)
;;; looks-module ends here
