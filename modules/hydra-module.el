;;; package -- Summary
;;; Commentary:
;;;; hydra module
;;; Code:
(use-package hydra
    :ensure t)

(use-package avy
    :ensure t)

(defhydra hydra-avy (global-map "C-c ;" :exit t :hint nil)
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

(defhydra hydra-zoom (global-map "C-<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;; hydra for mc

(defhydra hydra-multiple-cursors (global-map "C-c m c" :hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

;; hydra for ptojectile
(defhydra hydra-lsp (global-map "M-g ." :exit t)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_._] definition   [_t_] type            [_R_] rename
 [_x_] execute action   [_M-s_] describe session   [_/_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("." lsp-ui-peek-find-definitions)
  ("/" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("R" lsp-rename)
  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)
  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))


(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(provide 'hydra-module)
;;; hydra-module.el ends here
