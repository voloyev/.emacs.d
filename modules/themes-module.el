;;; packaage --- Summary:
;;; Code:
;;; Commentary:
;;; cursor-active-mark-init doing this
;; themes
(setq custom-safe-themes t)
;;(load-theme 'zenburn t)
;;(load-theme 'phoenix-dark-mono t)
;;(load-theme 'ample t t)
;(enable-theme 'ample)
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes/")
(load-theme 'minimal)
;;(load-theme 'sexy-monochrome t)
;;(enable-theme 'sexy-monochrome)
;;(load-theme 'tomorrow-night-paradise t)
;;(load-theme 'base16-solarized-dark t)
;;(load-theme 'ujelly t)
;;(enable-theme 'ujelly)
(defun cursor-active-mark-init ()
    "Данная функция меняет цвет курсора на красный."
(set-cursor-color "#ff0000")
(setq cursor-type 'bar))
"Мы можем дописывать нашу функцию, например, нам нужно, чтобы эта функция также меняла сам курсор на hollow:."

(add-hook 'activate-mark-hook 'cursor-active-mark-init)
"Теперь напишем хук, при котором вызывается наша функция."
(defun th-deactivate-mark-init ()
  "Когда мы выделяем текст, наш курсор становится красным и имеет стиль bar.
Но есть проблема, даже если мы закончим выделять текст,
стиль на дефолтный не вернётся.
Поэтому пишем функцию и хук, которые делают это."
(set-cursor-color "light gray")
(setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)
"Если выделение текста не активно."
(provide 'themes-module)
;;; themes-module ends here
