;;; helm-popwin.el --- Helm interface for popup window at bottom use popwin.

;; Copyright (C) 2016 zw963(Billy.Zheng)

;; Author: zw963 <vil963@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (popwin "1.0.0") (helm "1.7.7"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package make Helm window always at the bottom using popwin.

;; Most of code stolen from https://github.com/emacs-helm/helm/wiki/Popwin
;; For the problem which resolved by this package,  please see:
;; https://www.reddit.com/r/emacs/comments/33qj0p/make_helm_window_always_at_the_bottom_using/

;; In this post, It offer another two working solution.

;; But the first solution disable popwin completely while using helm will
;; result in performance loss, and cause `help-go-back' and `help-go-forwrd'
;; not work in help-mode.

;; And another solution which use `shackle-mode' is not as good user experience
;; as `popwin' for me.
;;

;; For the best user experience with, try `golden-ratio' package, this package
;; make helm window always have a reasonable height when popup from bottom.

;; Example config:
;;
;;   (require 'helm-popwin)

;;; Code:

(require 'popwin)
(require 'helm)

(add-to-list 'popwin:special-display-config '("^\\*helm.*\\*$" :regexp t))

(defun helm-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (popwin:display-buffer helm-buffer t)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))

(defun helm-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

(add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

(when (featurep 'golden-ratio)
  (add-to-list 'golden-ratio-inhibit-functions 'helm-alive-p))

(provide 'helm-popwin)

;;; helm-popwin ends here
