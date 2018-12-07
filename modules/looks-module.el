;;; Package --- Summary
;;; Code:
;;; Commentary:
;;; fonts and general looks
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Hack 13")
  (set-frame-font "Hack 13"))

(unless (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Hack 9")
  (set-frame-font "Hack 9"))

 (setq ring-bell-function 'ignore)

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; move backups
;; uncomment if you really need this
;; (setq backup-directory-alist '(("." . "~/.saves")))

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

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme 1)
  (setq sml/theme 'light)
  (sml/setup t)
  (setq sml/name-width '40)
  (setq sml/shorten-modes 'full))

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
    :ensure t
    :init
    (fci-mode 1)
    (setq fci-rule-width 3))

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode t)
  (nyan-start-animation))

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
    :config
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground "gray30")
    (set-face-attribute 'whitespace-newline
                        nil :background nil
                        :foreground "gray30")
    :bind(("<f5>" . whitespace-mode)
          ("C-c <f5>" . whitespace-cleanup)))

(provide 'looks-module)
;;; looks-module ends here
