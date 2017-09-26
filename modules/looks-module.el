;;; Package --- Summary
;;; Code:
;;; Commentary:
;;; fonts
(set-face-attribute 'default nil :font "Hack 10")
(set-frame-font "Hack 10")

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;;move backups
(setq backup-directory-alist '(("." . "~/.saves")))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   0)
(setq ingibit-startup-message 0)

;;sexy mode line
(setq sml/no-confirm-load-theme 1)
(setq sml/theme 'light)
(sml/setup t)
(setq sml/name-width '40)
(setq sml/shorten-modes 'full)
(nyan-mode t)
(setq nyan-animate-nyancat t)
;;(add-hook 'nyan-start-animation 'nyan-mode)

;; toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;;disable scrollbar
(scroll-bar-mode   -1)

;; show buffers
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;;(add-to-list 'ibuffer-never-show-regexps "^\\*")
(defalias 'list-buffers 'ibuffer)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))

(global-set-key (kbd "<f2>") 'bs-show)

;;scrolling
(setq scroll-step 1)
(windmove-default-keybindings 'meta)

;;short answer
(fset 'yes-or-no-p 'y-or-n-p)

;;Indent settings
(setq-default indent-tabs-mode nil)
(setq tab-width                  4)
(setq-default tab-width          2)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(custom-set-variables '(coffee-tab-width 2))
(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search result
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Use visual-line-mode in gfm-mode
(defun my-gfm-mode-hook ()
    (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")
(fci-mode 1)
(setq fci-rule-width 3)

(provide 'looks-module)
;;; looks-module ends here
