(let ((min-ver "29"))
  (when (version< emacs-version min-ver)
    (error "Your Emacs version is too old, v%s or higher is required" min-ver)))

;; ----------------------------------------------------------------------------
;; Setup code
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; MELPA repository for additional packages
(use-package package
  :config
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t))
;; End setup code
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Load configurations for various modes and packages.

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

;; SLIME (Superior Lisp Interaction Mode for Emacs)
(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-indentation slime-fancy))
  (setq slime-auto-start 'ask)
  (setq slime-repl-history-remove-duplicates t)
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix))))

;; Magit setup
(use-package magit :ensure t)

;; Load Windows setup
(when (equal system-type 'windows-nt)
  (require 'init-windows))

;; Ido (Interactive Do)
(use-package ido
  :config
  (ido-mode 'buffers)
  (setq ido-case-fold t)
  (setq ido-use-virtual-buffers t))

;; Org Mode
(use-package org
  :config
  (keymap-global-set "C-c l" 'org-store-link)
  (keymap-global-set "C-c c" 'org-capture)
  (keymap-global-set "C-c a" 'org-agenda))

;; Spellchecking (requires `aspell` or similar to be installed on system)
(use-package flyspell
  :hook (((org-mode text-mode) . (lambda ()
                                   (flyspell-mode)
                                   (flyspell-buffer)))
         (prog-mode . (lambda ()
                        (flyspell-prog-mode)
                        (flyspell-buffer)))))

;; Emacs IRC Client (ERC)
(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setopt erc-modules
          '(autojoin button completion fill irccontrols keep-place list
            match menu move-to-prompt netsplit networks noncommands
            notifications readonly ring stamp spelling track))
  :hook (erc-mode . hl-line-mode))
;; ERC nickname coloring/highlighting
(use-package erc-hl-nicks :ensure t :hook erc-mode)

;; ----------------------------------------------------------------------------

;; Set theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t t)
  (load-theme 'gruvbox-light-hard t t))

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

;; Enable disabled functions (they are disabled for newcomers).
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
