;;; package --- Summary:
;;; Code:
;;; Commentary:
;; elixir module

;; Highlights *.elixir2 as well
(use-package elixir-mode
    :ensure t
    :init
    (add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
    :config
    (add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)))

(use-package alchemist
    :ensure t
    :init
    ;; (setq alchemist-mix-command "mix")
    ;; (setq alchemist-mix-test-task "espec")
    (setq alchemist-key-command-prefix (kbd "C-c ."))
    (setq alchemist-mix-test-default-options '()) ;; default
    (setq alchemist-iex-program-name "iex") ;; default: iex
    (setq alchemist-execute-command "elixir") ;; default: elixir
    (setq alchemist-compile-command "elixirc") ;; default: elixirc
    (setq alchemist-hooks-compile-on-save t)
    (setq alchemist-goto-erlang-source-dir "~/source/otp")
    (setq alchemist-goto-elixir-source-dir "~/source/elixir"))

(use-package flycheck-credo
    :ensure t)

(eval-after-load 'flycheck
  '(flycheck-credo-setup))
(add-hook 'elixir-mode-hook 'flycheck-mode)

(provide 'elixir-module)
;;; elixir-module.el ends here
