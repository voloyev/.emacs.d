;;; package --- Summary:
;;; Commentary:
;;; Lol
;;; Code:
(setq package-list '(sly sly-company projectile-variable gitconfig-mode racket-mode arch-packer indium fill-column-indicator smartparens csv-mode d-mode erlang tern haskell-mode gist pydoc pallet jenkins jira js2-closure js2-highlight-vars js2-refactor js3-mode jsx-mode brainfuck-mode clojure-mode common-lisp-snippets company company-lua company-php rjsx-mode rust-mode nvm paradox toml toml-mode golden-ratio google-c-style flycheck-nim nim-mode sql-indent sqlite sqlplus sqlup-mode systemd rainbow-mode rake jdee pyenv-mode-auto pyenv-mode ein company-restclient company-erlang realgud-byebug realgud-rdb2 quickrun git-gutter-fringe nlinum realgud jekyll-modes react-snippets cargo simple-httpd slime-company company-go company-tern cask-mode package-build shut-up epl git commander f dash s cask stylus-mode json-mode js2-mode company-web angular-mode avy counsel-projectile counsel swiper slim-mode ranger smarty-mode password-store wanderlust flycheck zenburn-theme yari yaml-mode xcscope which-key weechat web-mode vimish-fold thrift ssh sr-speedbar smart-mode-line slime skewer-mode semi scss-mode sass-mode rvm ruby-tools ruby-hash-syntax ruby-dev ruby-block ruby-additional rubocop rspec-mode rsense robe rinari restclient rbenv racer projectile-speedbar projectile-rails projectile-codesearch php-mode phoenix-dark-mono-theme org-page nyan-mode neotree nav multiple-cursors migemo markdown-mode magit know-your-http-well imenu-list imenu-anywhere ibuffer-vc ibuffer-tramp ibuffer-rcirc ibuffer-projectile ibuffer-git hydra highlight-indentation golint go-mode git-gutter ggtags flymd flycheck-rust flycheck-elixir expand-region evil emmet-mode elscreen elixir-yasnippets dired+ s company-racer company-quickhelp company-jedi company-inf-ruby company-dict company-c-headers coffee-mode cmake-mode cider chef-mode calfw bundler alchemist achievements gitignore-mode gitconfig haml-mode sexy-monochrome-theme use-package))

;; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))
(package-initialize)

(provide 'auto-install-packages)
;;; auto-install-packages.el ends here
