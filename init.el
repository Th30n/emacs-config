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

;; SLIME (Superior Lisp Interaction Mode for Emacs)
(require 'init-slime)
;; Omnisharp (for working with C# on Windows)
(require 'init-omnisharp)
;; Haskell
(require 'init-haskell)
;; ----------------------------------------------------------------------------

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(global-linum-mode nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(if (equal system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas 10")
  (set-face-attribute 'default nil :font "Source Code Pro Medium 10"))

;; Rainbow delimiters
(require 'rainbow-delimiters)

(setq rainbow-delimiters-max-face-count 6)
(set-face-foreground 'rainbow-delimiters-depth-1-face "gray")
(set-face-foreground 'rainbow-delimiters-depth-2-face "aquamarine")
(set-face-foreground 'rainbow-delimiters-depth-3-face "gold")
(set-face-foreground 'rainbow-delimiters-depth-4-face "orchid")
(set-face-foreground 'rainbow-delimiters-depth-5-face "OliveDrab3")
(set-face-foreground 'rainbow-delimiters-depth-6-face "peru")
(set-face-foreground 'rainbow-delimiters-unmatched-face "red")
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(prefer-coding-system 'utf-8)
