(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 'dash)
(package-install 'ghub)
(package-install 'glab)
(package-install 'magit-popup)
(package-install 'with-editor)

;; (package-install 'emacsql)
;; (package-install 'emacsql-sqlite)
;; (package-install 'closql)
