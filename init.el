;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
;;; Initialize packages for installation
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/init-modules")
;;; List of required modules
(require 'auto-install-packages)
(require 'melpa-module)
(require 'ruby-module)
(require 'speedbar-module)
(require 'smartparens-module)
(require 'web-mode-module)
(require 'yasnippet-module)
(require 'helm-module)
(require 'highlight-indentation-mode-module)
;;(require 'ivy-module)
(require 'looks-module)
(require 'themes-module)
;; Achievements mode
(require 'achievements)
(achievements-mode 1)

;; Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Delete selection
(delete-selection-mode t)

;;company mode
(global-company-mode t)
(company-quickhelp-mode t)
(add-hook 'after-init-hook 'global-company-mode)

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
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;;multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-|") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;;global line mode
(global-hl-line-mode)

;;projectile
(projectile-global-mode)
(projectile-rails-global-mode)
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic))))

;; Add haml and yaml modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; flycheck
(package-install 'flycheck)
(global-flycheck-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; line number
(require 'linum)
(global-set-key "\C-cl" 'linum-mode)
(setq linum-format "%d ")

;; gutter
(global-git-gutter-mode +1)
;;(git-gutter:linum-setup)
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

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
(global-set-key (kbd "C-M-b") 'bookmark-set)
(global-set-key (kbd "M-C-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

;; whichkey
(package-install 'which-key)
(require 'which-key)
(which-key-mode t)

;; rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; racer
(setq racer-cmd "/usr/local/bin/racer")
(setq racer-rust-src-path "/home/nuncostans/.rust/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; Use company-racer in rust mode
(set (make-local-variable 'company-backends) '(company-racer))

;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/Mega/TODO/become_programer.org"
                             "~/Mega/must_notes.org"))
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;;whitespace
(global-set-key (kbd "<f5>") 'whitespace-mode)
(global-set-key (kbd "C-c <f5>") 'whitespace-cleanup)

;; evil modes
(global-set-key (kbd "C-M-e") 'evil-mode)

;; emmet mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; calendar app
(require 'calfw)
(require 'calfw-org)

;;(put 'upcase-region 'disabled nil)

;; work mouse in terminal
(xterm-mouse-mode t)

;; imenu anywhere
(global-set-key (kbd "C-c i") #'imenu-anywhere)

;; vimish folds
(require 'vimish-fold)
(global-set-key (kbd "C-c v f") #'vimish-fold)
(global-set-key (kbd "C-c v v") #'vimish-fold-delete)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; persp-mode
(persp-mode)

;; undo tree
(global-undo-tree-mode t)

;; avto revert files after
;; change git branch
(setq auto-revert-check-vc-info t)

;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
;; dealing with smartparens
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

;; python
(defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; c-mode settings
(setq c-default-style "linux")

;; expand region mode
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

;; simple  httpd
(require 'simple-httpd)
(setq httpd-root "/home/nuncostans/workspace/js/")
;;(httpd-start)

(unless (display-graphic-p)
    (add-to-list 'default-frame-alist '(background-color . "#000000")))

;; email wanderlust
(autoload 'wl "wl" "Wanderlust" t)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; avy config
(avy-setup-default)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "44cc408f88144aa9b41435dc9f3bc86a74b95b1a0bcd1e8a19a86c79e42d954e" "b2db1708af2a7d50cac271be91908fffeddb04c66cb1a853fff749c7ad6926ae" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(org-agenda-files (quote ("~/Mega/git/note/cursor.org")))
 '(package-selected-packages
   (quote
    (angular-mode avy counsel-projectile counsel swiper all-the-icons-dired slim-mode ranger smarty-mode password-store wanderlust flycheck zenburn-theme yari yaml-mode xcscope which-key weechat web-mode vimish-fold thrift ssh sr-speedbar smartparens smart-mode-line slime skewer-mode semi scss-mode sass-mode rvm ruby-tools ruby-hash-syntax ruby-dev ruby-block ruby-additional rubocop rspec-mode rsense robe rinari restclient rbenv racer projectile-speedbar projectile-rails projectile-codesearch php-mode phoenix-dark-mono-theme perspective org-page nyan-mode neotree nav multiple-cursors migemo markdown-mode magit know-your-http-well imenu-list imenu-anywhere ibuffer-vc ibuffer-tramp ibuffer-rcirc ibuffer-projectile ibuffer-git hydra highlight-indentation helm-swoop helm-projectile helm-git-grep helm-ag golint go-mode git-gutter ggtags flymd flycheck-rust flycheck-elixir expand-region evil emmet-mode elscreen elixir-yasnippets dired+ ctags-update ctags company-racer company-quickhelp company-jedi company-inf-ruby company-dict company-c-headers coffee-mode cmake-mode cider chef-mode calfw bundler alchemist achievements)))
 '(server-done-hook (quote ((lambda nil (kill-buffer nil)) delete-frame)))
 '(server-switch-hook
   (quote
    ((lambda nil
         (let
             (server-buf)
             (setq server-buf
                   (current-buffer))
             (bury-buffer)
             (switch-to-buffer-other-frame server-buf))))))
 '(speedbar-show-unknown-files t))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
