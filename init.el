(let ((min-ver "24"))
  (when (version< emacs-version min-ver)
    (error "Your Emacs version is too old, v%s or higher is required" min-ver)))

;; ----------------------------------------------------------------------------
;; Setup code
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
;; End setup code
;; ----------------------------------------------------------------------------

;; MELPA repository for additional packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; ----------------------------------------------------------------------------
;; Load configurations for various modes and packages.
;; This also automatically installs missing packages, so if you don't
;; plan on using any of the modes you should probably comment it out here.

;; Rainbow delimiters
(require 'init-rainbow-delimiters)
;; SLIME (Superior Lisp Interaction Mode for Emacs)
(require 'init-slime)
;; Omnisharp (for working with C# on Windows)
(require 'init-omnisharp)
;; Haskell
(require 'init-haskell)
;; CC Mode customizations
(require 'init-cc)
;; Magit setup
(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
;; ----------------------------------------------------------------------------

;; Windows fix for pushing via https.
(when (equal system-type 'windows-nt)
  (setenv "GIT_ASKPASS" "git gui--askpass"))

;; Ido (Interactive Do)
(require 'ido)
(ido-mode 'buffers)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Set theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)
(setq-default custom-enabled-themes '(zenburn))

(setq-default
 fill-column 78
 column-number-mode t
 display-time-24hr-format t
 display-time-default-load-average nil
 indent-tabs-mode nil
 indicate-buffer-boundaries 'left
 read-buffer-completion-ignore-case t)

;; Use eww as default browser
(setq-default browse-url-browser-function 'eww-browse-url)

(display-time-mode)

(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(set-face-attribute 'default nil :font "Source Code Pro Semibold 10")

(prefer-coding-system 'utf-8)

;; ----------------------------------------------------------------------------
;; Store 'Customization' stuff here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ----------------------------------------------------------------------------
;; Load local stuff.
(require 'init-local nil t)
