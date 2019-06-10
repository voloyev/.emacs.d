;;; package --- Summary:
;;; Code:
;;; Commentary:
;; yanisppet
(use-package yasnippet
    :ensure t)

(defun enable-yas-mode ()
  (yas-minor-mode t))
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; yas-mode for my modes
(add-hook 'ruby-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'rust-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'python-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'web-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'html-mode-hook '(lambda () (yas-minor-mode 1)))
(add-hook 'js2-mode-hook '(lambda () (yas-minor-mode 1)))

(add-to-list 'load-path
             "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode t)

(provide 'yasnippet-module)
;;; yasnippet-module ends here
