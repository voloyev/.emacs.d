;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module
;;"TODO:
;;- Add ability to automatic change python version a-la pyenv
;;- Add integration with elpy
;;"
;;(defun use-pyenv-python360 ()
;;  "Point to Python 3 for `elpy-mode', `flycheck-mode', and `python-mode'."
;;  (interactive)
;;  (setq
;;   elpy-rpc-python-command "~/.pyenv/versions/3.6.0/bin/python3.6m"
;;   elpy-rpc-pythonpath "~/.pyenv/versions/3.6.0/lib/python3.6/site-packages"
;;   flycheck-python-flake8-executable "~/.pyenv/versions/3.6.0/bin/flake8"
;;   python-check-command "~/.pyenv/versions/3.6.0/bin/pyflakes"
;;   python-shell-interpreter "~/.pyenv/versions/3.6.0/bin/ipython"))
;;
;;(defun use-system-python3 ()
;;  "Use the system python3 for `elpy-mode', `flycheck-mode', and `python-mode'."
;;  (interactive)
;;  (setq
;;   elpy-rpc-python-command "/usr/bin/python3.4m"
;;   elpy-rpc-pythonpath "/usr/local/lib/python3.4/dist-packages"
;;   flycheck-python-flake8-executable "/usr/bin/flake8"
;;   python-check-command "/usr/bin/pyflakes3"
;;   python-shell-interpreter "/usr/bin/ipython3"))
;;
;;(defun use-system-python2 ()
;;  "Use the system python2 for `elpy-mode', `flycheck-mode', and `python-mode'."
;;  (interactive)
;;  (setq
;;   elpy-rpc-python-command "/usr/bin/python2.7"
;;   elpy-rpc-pythonpath "/usr/local/lib/python2.7/dist-packages"
;;   flycheck-python-flake8-executable "/usr/bin/flake8"
;;   python-check-command "/usr/bin/pyflakes"
;;   python-shell-interpreter "/usr/bin/ipython"))
;;
;;(defun toggle-comment ()
;;  "Toggle comments on the current line or highlighted region."
;;  (interactive)
;;  (if mark-active
;;      (let ((mark (mark))
;;            (point (point)))
;;        (if (> (mark) (point))
;;            (comment-or-uncomment-region
;;             point
;;             mark)
;;          (comment-or-uncomment-region
;;           mark
;;           point)))
;;    (comment-or-uncomment-region
;;     (line-beginning-position)
;;     (line-end-position))))
;;
;;(use-pyenv-python360)
(pyenv-mode)
(require 'pyenv-mode)
(defun ssbb-pyenv-hook ()
"Automatically activates pyenv version if .python-version file exists."
(f-traverse-upwards
(lambda (path)
  (let ((pyenv-version-path (f-expand ".python-version" path)))
		(if (f-exists? pyenv-version-path)
				(pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

(add-hook 'find-file-hook 'ssbb-pyenv-hook)

(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
(provide 'python-module)
;;; python-module ends here
