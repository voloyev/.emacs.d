;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module
;;"TODO:
;;- Add ability to automatic change python version a-la pyenv
;;- Add integration with elpy
;;"
(use-package elpy
  :ensure t)
(package-initialize)
(elpy-enable)

(provide 'python-module)
;;; python-module ends here
