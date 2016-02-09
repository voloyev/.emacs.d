(require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
(package-initialize)

;;toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode 1)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t

;; Delete selection
      (delete-selection-mode t)

      ;;disable scrollbar
      (scroll-bar-mode   -1)

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
(setq show-paren-style 'expression)
(show-paren-mode 1)
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
(sml/setup t)
(setq sml/theme 'light)
(setq sml/no-confirm-load-theme t)
(nyan-mode t)

;; show buffers
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "<f2>") 'bs-show)

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

;; Indent settings
(setq-default indent-tabs-mode nil) ;; отключить возможность ставить отступы TAB'ом
(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
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
(setq speedbar-supported-extension-expressions '(
  ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
  ".tex\\(i\\(nfo\\)?\\)?"
  ".todo"
  ".done"
  ".md"
  ".el"
  ".emacs"
  ".l"
  ".lsp"
  ".p"
  ".java"
  ".js"
  ".f\\(90\\|77\\|or\\)?"
  ".ad[abs]"
  ".p[lm]"
  ".tcl"
  ".m"
  ".scm"
  ".pm"
  ".py"
  ".g"
  ".rb"
  "\\.\\(inc\\|php[s345]?\\|phtml\\)"
  ".s?html"
  ".ma?k"
  "[Mm]akefile\\(\\.in\\)?"))
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
	    (speedbar-add-supported-extension "\\.css")))

;;yanisppet
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
;;(yas/load-directory "~/.emacs.d/yasnippet/snippets")

(package-install 'flycheck)
(global-flycheck-mode)

;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)

;;themes
(load-theme 'quasi-monochrome t)

;;Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;;autopair
(require 'autopair)
(autopair-global-mode)
(global-git-gutter-mode +1)
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
(setq web-mode-extra-nippets '(("erb" . (("name" . ("beg" . "end"))))
                                ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))

;;autocomplete
(ac-config-default)


;;map
   (global-set-key (kbd "<f8>") 'visit-tags-table)

  ;; | Combo | Function         | Description                |
  ;; |-------+------------------+----------------------------|
  ;; | <f3>  | visit-tags-table | Loads tags                 |
  ;; | M-.   | find-tag         | Jumps to the specified tag |
  ;; | C-M-. | pop-tag-mark     | Jumps back                 |

(global-set-key (kbd "C-M-b") 'bookmark-set)
(global-set-key (kbd "M-C-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

;;fonts
(set-face-attribute 'default nil :font "Terminus Re33 13" )
(set-frame-font "Terminus Re33 13" nil t)

;;line nunbersppp
(global-linum-mode 1)
(setq linum-format " %d ")

;;autopair
(require 'autopair)
(autopair-global-mode t)

;;whichkey
(package-install 'which-key)
(require 'which-key)
(which-key-mode t)

;;ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
