;;Init MELPA represitory
(require 'package)
;;; Code:
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))
(provide 'melpa-module)
;;; melpa-module.el ends here
