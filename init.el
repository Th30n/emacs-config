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
;; Magit setup
(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(when (equal system-type 'windows-nt)
  (require 'init-windows))
;; ----------------------------------------------------------------------------

;; Ido (Interactive Do)
(require 'ido)
(ido-mode 'buffers)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)

;; Org Mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-agenda-start-on-weekday nil) ;; Start week from current day.

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Set theme
(require-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-soft t t)
(load-theme 'gruvbox-light-hard t t)

(defun light-theme ()
  (interactive)
  (unless (member 'gruvbox-light-hard custom-enabled-themes)
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme 'gruvbox-light-hard)))

(defun dark-theme ()
  (interactive)
  (unless (member 'gruvbox-dark-soft custom-enabled-themes)
    (mapc #'disable-theme custom-enabled-themes)
    (enable-theme #'gruvbox-dark-soft)))

(defun theme-for-time-of-day ()
  (interactive)
  (let ((curr-hour (decoded-time-hour (decode-time))))
    (if (< 7 curr-hour 17)
        (light-theme)
      (dark-theme))))

(cancel-function-timers #'theme-for-time-of-day)
(run-with-timer 0 60 #'theme-for-time-of-day)

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
