;;;; Windows only customization

;; Omnisharp (for working with C# on Windows)
(require 'init-omnisharp)

;; Windows fix for pushing via https.
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; Use Powershell commands for grep.
(require 'grep)
(grep-apply-setting 'grep-command "powershell -Command Select-String ")
(grep-apply-setting 'grep-use-null-device nil)

(provide 'init-windows)
