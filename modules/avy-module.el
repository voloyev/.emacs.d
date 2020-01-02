;;; package --- Summary:
;;; Code:
;;; Commentary:
;; avy module
(use-package avy
    :ensure t)

(defhydra hydra-avy (global-map "C-;" :exit t :hint nil)
 ;; ^Line^       ^Region^        ^Goto^
 ;; ----------------------------------------------------------
 ;; [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 ;; [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 ;; [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line

  ("c" avy-goto-char-timer)
  (";" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(provide 'avy-module)
;;; avy-module.el ends here
