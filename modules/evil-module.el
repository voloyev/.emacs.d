;; evil mode
(use-package evil
     :ensure t
     :bind
     (("C-c e e"   . evil-local-mode)
      ("<f2> <f2>" . evil-mode)))

(use-package evil-matchit
    :ensure t
    :config
    (add-hook 'evil-local-mode 'turn-on-evil-matchit-mode)
    (add-hook 'evil-mode 'turn-on-evil-matchit-mode))

;; use emacs keybindings in insert mode
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evil-set-initial-state 'eshell-mode 'emacs)

(provide 'evil-module)
;;; evil-module ends here
