;;; package --- My emaacs init-file
;;; Commentary:
;;; Name: My Emacs config
;;; Autor: Volodymyr Yevtushenko
;;; Code:
;;; Initialize packages for installation
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/modules")
;;; List of required modules
(require 'auto-install-packages)
(require 'melpa-module)
(require 'ruby-module)
(require 'speedbar-module)
(require 'smartparens-module)
(require 'web-mode-module)
(require 'yasnippet-module)
(require 'helm-module)
(require 'python-module)
(require 'highlight-indentation-mode-module)
;;(require 'ivy-module)
(require 'looks-module)
(require 'themes-module)
;; Achievements mode
(require 'achievements)
(achievements-mode 1)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
;; Emacs server
(require 'server)
(unless (server-running-p)
    (server-start))

;; Delete selection
(delete-selection-mode t)

;; use bash
(setq shell-file-name "/bin/bash")

;; company mode
(require 'company)
(global-company-mode t)
(company-quickhelp-mode t)
(global-set-key (kbd "C-<tab>") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-robe)
(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-jedy)
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
;;(package-install 'flycheck)
(global-flycheck-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "grip --export")
;; line number
(require 'nlinum)
(global-set-key "\C-cl" 'nlinum-mode)
(add-hook 'ruby-mode-hook 'nlinum-mode)
(add-hook 'python-mode-hook 'nlinum-mode)
(add-hook 'lisp-mode-hook 'nlinum-mode)
(add-hook 'c-mode-hook 'nlinum-mode)
(add-hook 'js2-mode-hook 'nlinum-mode)
(add-hook 'js2-jsx-mode-hook 'nlinum-mode)
(add-hook 'rust-mode-hook 'nlinum-mode)
(add-hook 'java-mode-hook 'nlinum-mode)
(add-hook 'web-mode-hook 'nlinum-mode)
(add-hook 'emacs-lisp-mode-hook 'nlinum-mode)

;; gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode +1)
;;(git-gutter:linum-setup)
;;(add-hook 'ruby-mode-hook 'git-gutter-mode)
;;(add-hook 'python-mode-hook 'git-gutter-mode)
;;(set-face-background 'git-gutter:modified "purple") ;; background color
;;(set-face-foreground 'git-gutter:added "green")
;;(set-face-foreground 'git-gutter:deleted "red")

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
(setq org-src-fontify-natively nil)
(defface org-block
    '((t (:background "#000000")))
    "Face used for the source block background.")

;;: foreground #00007f  height 0.9
;;org-block-begin-line, org-block-end-line: foreground #545454 background #e0dfd1

;;whitespace
(global-set-key (kbd "<f5>") 'whitespace-mode)
(global-set-key (kbd "C-c <f5>") 'whitespace-cleanup)

;; evil modes
(global-set-key (kbd "<f6>") 'evil-mode)

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
(setq neo-theme  'arrow)

;;slim-mode
(require 'slim-mode)
(add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode))
;;lein exec path
(add-to-list 'exec-path "/home/nuncostans/Programs/leiningen")

;;quickrun
(require 'quickrun)

;;all-the-icons
(require 'all-the-icons)
;; css and sccs indent level
(setq css-indent-offset 2)
(setq scss-indent-offset 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#000000" "light gray" "dark gray" "light slate gray"])
 '(ansi-term-color-vector
   [unspecified "#000000" "#d54e53" "#afd75f" "#e7c547" "#5f87d7" "#af87d7" "#5f87d7" "#dadada"])
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (smart-mode-line-light sexy-monochrome)))
 '(custom-safe-themes
   (quote
    ("ca8e634fa3e088ef5e19a6e609f9e79fd407a6188fcb5bc3de17801ed38f8afa" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b32487a336b756ef2353018ccc58c27356727ddf2b72f28f922306fb9f95d01b" "4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "2a18e54e84857de0f8703671661609ad9d287c25175594a282f91444841e92e5" default)))
 '(fci-rule-character-color "#1c1c1c")
 '(fci-rule-color "#1c1c1c")
 '(neo-theme (quote arrow))
 '(package-selected-packages
   (quote
    (spacemacs-theme sql-indent sqlite sqlplus sqlup-mode systemd tao-theme rainbow-mode rake jdee jedi auto-virtualenv pyenv-mode-auto pyenv-mode elpy ein company-restclient company-erlang realgud-byebug realgud-rdb2 all-the-icons quickrun git-gutter-fringe nlinum realgud jekyll-modes ample-theme 0blayout react-snippets cargo simple-httpd slime-company company-go company-tern flycheck-ycmd company-ycmd cask-mode package-build shut-up epl git commander f dash s cask stylus-mode json-mode js2-mode company-web angular-mode avy counsel-projectile counsel swiper slim-mode ranger smarty-mode password-store wanderlust flycheck zenburn-theme yari yaml-mode xcscope which-key weechat web-mode vimish-fold thrift ssh sr-speedbar smartparens smart-mode-line slime skewer-mode semi scss-mode sass-mode rvm ruby-tools ruby-hash-syntax ruby-dev ruby-block ruby-additional rubocop rspec-mode rsense robe rinari restclient rbenv racer projectile-speedbar projectile-rails projectile-codesearch php-mode phoenix-dark-mono-theme perspective org-page nyan-mode neotree nav multiple-cursors migemo markdown-mode magit know-your-http-well imenu-list imenu-anywhere ibuffer-vc ibuffer-tramp ibuffer-rcirc ibuffer-projectile ibuffer-git hydra highlight-indentation helm-swoop helm-projectile helm-git-grep helm-ag golint go-mode git-gutter ggtags flymd flycheck-rust flycheck-elixir expand-region evil emmet-mode elscreen elixir-yasnippets dired+ ctags-update ctags company-racer company-quickhelp company-jedi company-inf-ruby company-dict company-c-headers coffee-mode cmake-mode cider chef-mode calfw bundler alchemist achievements)))
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
 '(sml/no-confirm-load-theme 1)
 '(sml/theme (quote light))
 '(speedbar-show-unknown-files t))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-current-column-face ((t (:background "gray9")))))
