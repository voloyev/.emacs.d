;;; package --- My emaacs init-file
;;; Commentary:
;;; Code:
;;; Initialize packages for installation
(setq package-list '(
                     achievements       
                     alchemist          
                     apel               
                     async              
                     bundler            
                     calfw              
                     cider              
                     clojure-mode       
                     codesearch         
                     coffee-mode        
                     company            
                     company-dict       
                     company-inf-ruby   
                     company-jedi       
                     company-quickhelp  
                     company-racer      
                     concurrent         
                     ctable             
                     ctags              
                     ctags-update       
                     danneskjold-theme  
                     dash               
                     deferred           
                     dired+             
                     elixir-mode        
                     elixir-yasnippets  
                     elscreen           
                     emmet-mode         
                     epc                
                     epl                
                     evil               
                     evil-matchit       
                     evil-rails         
                     evil-tabs          
                     f                  
                     findr              
                     flim               
                     flycheck           
                     flycheck-elixir    
                     flycheck-rust      
                     flymd              
                     ggtags             
                     git                
                     git-commit         
                     git-gutter         
                     google-maps        
                     goto-chg           
                     haml-mode          
                     helm               
                     helm-ag            
                     helm-core          
                     helm-projectile    
                     helm-swoop         
                     highlight-indentation
                     ht                 
                     htmlize            
                     hydra              
                     ibuffer-git        
                     ibuffer-projectile 
                     ibuffer-rcirc      
                     ibuffer-tramp      
                     ibuffer-vc         
                     imenu-anywhere     
                     imenu-list         
                     inf-ruby           
                     inflections        
                     jedi-core          
                     jump               
                     keyfreq            
                     know-your-http-well 
                     let-alist          
                     macrostep          
                     magit              
                     magit-popup        
                     markdown-mode      
                     material-theme     
                     migemo             
                     minimal-theme      
                     monochrome-theme   
                     multiple-cursors   
                     mustache           
                     nav                
                     neotree            
                     nyan-mode          
                     org                
                     org-page           
                     paradox            
                     parent-mode        
                     perspective        
                     phoenix-dark-mono-theme
                     php-mode           
                     pkg-info           
                     popup              
                     pos-tip            
                     projectile         
                     projectile-codesearch
                     projectile-rails   
                     projectile-speedbar
                     python-environment 
                     quasi-monochrome-theme
                     queue              
                     racer              
                     rake               
                     rbenv              
                     restclient         
                     rich-minority      
                     rinari             
                     robe               
                     rsense             
                     rspec-mode         
                     rubocop            
                     ruby-additional    
                     ruby-block         
                     ruby-compilation   
                     ruby-dev           
                     ruby-hash-syntax   
                     ruby-tools         
                     rust-mode          
                     rvm                
                     s                  
                     sass-mode          
                     scss-mode          
                     semi               
                     seq                
                     simple-httpd       
                     slime              
                     smart-mode-line    
                     smartparens        
                     spinner            
                     sr-speedbar        
                     ssh                
                     tao-theme          
                     thrift             
                     tracking           
                     undo-tree          
                     vimish-fold        
                     web-mode          
                     weechat           
                     which-key         
                     with-editor       
                     xcscope           
                     yaml-mode         
                     yari              
                     yasnippet         
                     zenburn-theme  ))
;;Init MELPA represitory
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))


;; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))

;;------------------------------;;
;;Achievements mode
(require 'achievements)
(achievements-mode 1)

;;themes
;;(load-theme 'zenburn t)
(load-theme 'phoenix-dark-mono t)
;;(load-theme 'tao-yin t)
;;(load-theme 'monochrome t)
;;(load-theme 'danneskjold t)
;;(load-theme 'quasi-monochrome t)
;;(load-theme 'minimal t)
;;(set-frame-parameter nil 'background-mode 'dark)
;;(set-terminal-parameter nil 'background-mode 'dark)

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
(set-face-attribute 'weight 'extra-bold)
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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;;(add-to-list 'ibuffer-never-show-regexps "^\\*")
(defalias 'list-buffers 'ibuffer)
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)))

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
;;(require 'projectile-speedbar)
(require 'projectile-codesearch)
(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;;(setq speedbar-show-unknown-files t) ; show all files
;;(setq sr-speedbar-right-side nil) ; to the left side
;;(sr-speedbar-refresh-turn-on)
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic))))
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;;scrolling
(setq scroll-step 1)
(windmove-default-keybindings 'meta)

;;short answer
(fset 'yes-or-no-p 'y-or-n-p)

;;ruby
;;rvm
(require 'rvm)
(rvm-use-default)
(rvm-activate-corresponding-ruby)
;;rbenv
;;(require 'rbenv)
;;(global-rbenv-mode)
(require 'ruby-tools)
(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(setq ruby-deep-indent-paren nil)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
;;(defadvice inf-ruby (before activate-rvm-for-robe activate)
;;    (rvm-activate-corresponding-ruby))
(eval-after-load 'company
    '(push 'company-robe company-backends))
;; shortkey for company-complete
(global-set-key (kbd "<f6>") 'company-complete)
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
(global-set-key (kbd "C-c r r") 'inf-ruby)
(require 'bundler)


;; rinari
(setq rinari-tags-file-name "TAGS")

;;Add custome modes extension
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;;Indent settings
(setq-default indent-tabs-mode nil)
(setq tab-width                  4)
;;(setq-default c-basic-offset     4)
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
;; (defun format-current-buffer()
;;     (indent-region (point-min) (point-max)))
;; (defun untabify-current-buffer()
;;     (if (not indent-tabs-mode)
;;         (untabify (point-min) (point-max)))
;;     nil)
;; (add-to-list 'write-file-functions 'format-current-buffer)
;; (add-to-list 'write-file-functions 'untabify-current-buffer)
;; (add-to-list 'write-file-functions 'delete-trailing-whitespace)

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
              (speedbar-add-supported-extension  ".haml")
              (speedbar-add-supported-extension  "[Mm]akefile\\(\\.in\\)?")
              (speedbar-add-supported-extension  "\\.rs")))
(setq sr-speedbar-width-x 20)

;;yanisppet
(require 'yasnippet)
(defun enable-yas-mode ()
     (yas-minor-mode t))
(eval-after-load 'rspec-mode
    '(rspec-install-snippets))

;;yas-mode for my modes
(add-hook 'ruby-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'rust-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'python-mode-hook '(lambda () (yas-minor-mode 1)))
;;(yas-global-mode t)
(add-to-list 'load-path
             "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;;flycheck
(package-install 'flycheck)
(global-flycheck-mode)

;; Highlight search result
(setq search-highlight        t)
(setq query-replace-highlight t)

;;Markdown
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
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

(--each '(restclient-mode-hook
          js-mode-hook
          python-mode-hook
          web-mode-hook
          ruby-mode-hook
          markdown-mode-hook
          org-mode-hook
          rust-mode-hook
          cc-mode-hook
          lisp-mode-hook
          emacs-lisp-mode-hook
          haml-mode-hook
          c-mode-hook)
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
(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

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

;;;; Display ido results vertically, rather than horizontally
;;(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;;(add-hook 'ido-setup-hook 'ido-define-keys)

;;rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;racer
(setq racer-cmd "/usr/local/bin/racer")
(setq racer-rust-src-path "/home/nuncostans/.rust/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; Use company-racer in rust mode
(set (make-local-variable 'company-backends) '(company-racer))

;;org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/Mega/git/note/cursor.org"
                             "~/Mega/TODO/become_programer.org"
                             "~/Mega/workspace/org_notes/views_and_controllers.org"))
(add-hook 'org-mode-hook 'toggle-truncate-lines)
;;whitespace
(global-set-key (kbd "<f5>") 'whitespace-mode)

;; evil modes
(global-set-key (kbd "C-M-e") 'evil-mode)
;;(add-hook 'evil-mode-hook evil-matchit-mode)
;;(require 'evil-matchit)
;;(global-evil-matchit-mode t)
;;(global-evil-tabs-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b2db1708af2a7d50cac271be91908fffeddb04c66cb1a853fff749c7ad6926ae" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(org-agenda-files
   (quote
    ("~/Mega/git/note/cursor.org" "~/Dropbox/TODO/become_programer.org" "~/Dropbox/workspace/org_notes/views_and_controllers.org" "~/Dropbox/workspace/org_notes/aikido.org")))
 '(paradox-automatically-star t)
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
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t))

;;emmet mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "dark gray" :foreground "black")))))

;;show all files in speedbar

;;paradox github integration
(setq paradox-github-token "1a8089f872abd9efa1a25b56bf0f256db641f431")

;;calendar app
(require 'calfw)
(require 'calfw-org)

;;(put 'upcase-region 'disabled nil)

;; work mouse in terminal
(xterm-mouse-mode t)

;;helm
(require 'helm-config)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Locate the helm-swoop folder to your path
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; If you would like to use migemo, enable helm's migemo feature
;;(helm-migemo-mode 1)
;;emacs-neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; imenu anywhere
(global-set-key (kbd "C-.") #'imenu-anywhere)

;;vimish folds
(require 'vimish-fold)
(global-set-key (kbd "C-c v f") #'vimish-fold)
(global-set-key (kbd "C-c v v") #'vimish-fold-delete)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)

;;
(persp-mode)

;;undo tree
(global-undo-tree-mode t)

;; highlight indentation
(highlight-indentation-current-column-mode 1)
(provide 'init)

;;hooks ;)
;;; hooks for highlightion
(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'haml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'web-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'rust-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'lisp-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)

;;add highlight ingentation
(global-set-key(kbd "<f9>") 'highlight-indentation-current-column-mode)
(set-face-background 'highlight-indentation-face "#444444")
(set-face-background 'highlight-indentation-current-column-face "#444444")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)
(global-set-key [f7] 'livedown:preview)
;;; hooks for ruby mode
;;(add-hook 'ruby-mode-hook 'inf-ruby-mode)
;;(add-hook 'inf-ruby-mode-hook 'robe-start)
(setq auto-revert-check-vc-info t)
;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
;;dealing with smartparens 
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

;;python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;;resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;c-mode settings
(setq c-default-style "linux")
;;; init.el ends here
