;;; package --- Summary
;;; Commentary:
;;; Code:
(setq package-list '(
                     achievements
                     apel
                     async
                     company
                     dash
                     dired+
                     emmet-mode
                     epl
                     f
                     findr
                     flim
                     flycheck
                     ggtags
                     git-commit
                     git-gutter
                     imenu-list
                     inf-ruby
                     inflections
                     jump
                     keyfreq
                     let-alist
                     macrostep
                     markdown-mode
                     material-theme
                     monochrome-theme
                     multiple-cursors
                     nyan-mode
                     php-mode
                     pkg-info
                     popup
                     projectile
                     ;;quasi-monochrome
                     racer
                     rich-minority
                     rinari
                     rspec-mode
                     ruby-additional
                     ruby-block
                     ruby-compilation
                     ruby-dev
                     ruby-hash-syntax
                     ruby-tools
                     rust-mode
                     s
                     semi
                     seq
                     slime
                     smart-mode-line
                     smartparens
                     smex
                     sr-speedbar
                     ssh
                     wanderlust
                     web-mode
                     which-key
                     with-editor
                     xcscope
                     yasnippet
                     ))
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

                                        ; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

                                        ; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))

;;------------------------------;;

;;themes
(load-theme 'quasi-monochrome t)
;;(load-theme 'monochrome t)
;;(load-theme 'material t)
;;(load-theme 'sanityinc-solarized-dark t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;;move backups
(setq backup-directory-alist '(("." . "~/.saves")))

;; Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;;toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Delete selection
(delete-selection-mode t)

;;disable scrollbar
(scroll-bar-mode   -1)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;;copy without selection
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
           (interactive (if mark-active (list (region-beginning) (region-end)) (message
                                                                                "Copied line") (list (line-beginning-position) (line-beginning-position
                                                                                                                                2)))))
(defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
         (list (line-beginning-position)
               (line-beginning-position 2)))))

;;paren mode
(show-paren-mode 1)
(setq show-paren-style 'expression)
(set-face-foreground 'show-paren-match "black")

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
    "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
    (interactive)
    (let* ((cb (char-before (point)))
           (matching-text (and cb
                               (char-equal (char-syntax cb) ?\) )
                               (blink-matching-open))))
        (when matching-text (message matching-text))))

;;sexy mode line
(setq sml/no-confirm-load-theme 1)
(setq sml/theme 'dark)
(setq sml/name-width '40)
(setq sml/shorten-modes 'full)
(sml/setup t)
(nyan-mode t)
(add-hook 'nyan-start-animation 'nyan-mode)

;; show buffers
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;;multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;;global line mode
(global-hl-line-mode)

;;projectile
(projectile-global-mode)

;;scrolling
(setq scroll-step 1)
(windmove-default-keybindings 'meta)

;;short answer
(fset 'yes-or-no-p 'y-or-n-p)

;;ruby
(require 'ruby-tools)
(setq ruby-indent-level 2)

;;Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; Delete trailing whitespaces, format buffer and untabify when save buffer
(defun format-current-buffer()
    (indent-region (point-min) (point-max)))
(defun untabify-current-buffer()
    (if (not indent-tabs-mode)
        (untabify (point-min) (point-max)))
    nil)
(add-to-list 'write-file-functions 'format-current-buffer)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;;slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(add-hook 'speedbar-mode-hook
          (lambda()
              (speedbar-add-supported-extension "\\.rb")
              (speedbar-add-supported-extension "\\.ru")
              (speedbar-add-supported-extension "\\.erb")
              (speedbar-add-supported-extension "\\.rjs")
              (speedbar-add-supported-extension "\\.rhtml")
              (speedbar-add-supported-extension "\\.rake")
              (speedbar-add-supported-extension "\\.md")
              (speedbar-add-supported-extension "\\.py")
              (speedbar-add-supported-extension "\\.html")
              (speedbar-add-supported-extension "\\.css")
              (speedbar-add-supported-extension  ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?")
              (speedbar-add-supported-extension  ".tex\\(i\\(nfo\\)?\\)?")
              (speedbar-add-supported-extension  "\\.todo")
              (speedbar-add-supported-extension  "\\.done")
              (speedbar-add-supported-extension  "\\.el")
              (speedbar-add-supported-extension  ".emacs")
              (speedbar-add-supported-extension  "\\.l")
              (speedbar-add-supported-extension  "\\.lsp")
              (speedbar-add-supported-extension  "\\.p")
              (speedbar-add-supported-extension  "\\.java")
              (speedbar-add-supported-extension  "\\.js")
              (speedbar-add-supported-extension  ".f\\(90\\|77\\|or\\)?")
              (speedbar-add-supported-extension  ".ad[abs]")
              (speedbar-add-supported-extension  ".p[lm]")
              (speedbar-add-supported-extension  "\\.tcl")
              (speedbar-add-supported-extension  ".m")
              (speedbar-add-supported-extension  "\\.scm")
              (speedbar-add-supported-extension  ".pm")
              (speedbar-add-supported-extension  "\\.g")
              (speedbar-add-supported-extension  "\\.\\(inc\\|php[s345]?\\|phtml\\)")
              (speedbar-add-supported-extension  ".s?html")
              (speedbar-add-supported-extension  ".ma?k")
              (speedbar-add-supported-extension  "[Mm]akefile\\(\\.in\\)?")
              (speedbar-add-supported-extension  "\\.rs")))

;;yanisppet
(require 'yasnippet)
(yas-global-mode t)
(add-to-list 'load-path
             "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;;flycheck
(package-install 'flycheck)
(global-flycheck-mode)

;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Use visual-line-mode in gfm-mode
(defun my-gfm-mode-hook ()
    (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)

;;Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Default setup of smartparens
(require 'smartparens-config)
(--each '(restclient-mode-hook
          js-mode-hook
          python-mode-hook
          web-mode-hook
          ruby-mode-hook
          markdown-mode-hook
          org-mode-hook
          rust-mode-hook
          cc-mode-hook
          lisp-mode-hook)
    (add-hook it 'turn-on-smartparens-mode))

;;line number
(require 'linum)
;;(global-set-key "\C-c l" 'linum-mode)
(defun enable-linum-mode ()
    (linum-mode t))
;;linum mode for my modes
(add-hook 'ruby-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'lisp-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'rust-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'python-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'web-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'c-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'javascript-mode-hook '(lambda () (linum-mode 1)))
;; format linum
(setq linum-format "%d ")

;;gutter
(global-git-gutter-mode +1)
(git-gutter:linum-setup)
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)

;;web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)

;;настройка отступов
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;сниппеты и автозакрытие парных скобок
(setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))))

;;company mode
(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)

;;map
(global-set-key (kbd "<f8>") 'visit-tags-table)
;; | Combo | Function         | Description                |
;; |-------+------------------+----------------------------|
;; | <f3>  | visit-tags-table | Loads tags                 |
;; | M-.   | find-tag         | Jumps to the specified tag |
;; | C-M-. | pop-tag-mark     | Jumps back                 |

;; Bookmark settings
(require 'bookmark)
(setq bookmark-save-flag t) ;; автоматически сохранять закладки в файл
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
(global-set-key (kbd "C-M-b") 'bookmark-set)
(global-set-key (kbd "M-C-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

;;fonts
(set-face-attribute 'default nil :font "Terminus Re33 12" )
(set-frame-font "Terminus Re33 12")


;;whichkey
(package-install 'which-key)
(require 'which-key)
(which-key-mode t)

;;ido
(require 'ido)
(ido-mode t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;;racer
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;;wanderlust
(autoload 'wl "wl" "Wanderlust" t)

;;org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/Mega/git/note/main.org"
                             "~/Mega/git/note/todo.org"
                             "~/Mega/git/note/aikisite.org"))

;;whitespace
(global-set-key (kbd "<f5>") 'whitespace-mode)


;;github markdown preview
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(markdown-command "/home/nuncostans/Programs/flavor.rb")
 '(show-paren-mode t)
 '(show-paren-style (quote expression)))

;;emmet mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "dark gray" :foreground "black")))))
