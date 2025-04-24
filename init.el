;;; Version Check

(let ((min-ver "30"))
  (when (version< emacs-version min-ver)
    (error "Your Emacs version is too old, v%s or higher is required" min-ver)))

;;; Package Setup Code

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; MELPA repository for additional packages
(use-package package
  :config
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t))

;;; Modes & Packages

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
(use-package magit :ensure t
  :config
  (setopt git-commit-summary-max-length 51)
  :init
  ;; For some reason this doesn't work with :hook, so do it explicitly.
  (add-hook 'git-commit-setup-hook (lambda () (setq fill-column 72))))

;; Load Windows setup
(when (equal system-type 'windows-nt)
  (require 'init-windows))

;; Use ripgrep as default grep.
(when (executable-find "rg")
  (use-package grep
    :config
    (grep-apply-setting 'grep-command "rg -nHS --no-heading ")
    (grep-apply-setting 'grep-use-null-device nil)
    (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec rg <C> -nHS --no-heading --null -e <R> \\{\\} +"))
  (setopt grep-use-headings t)
  (setopt xref-search-program 'ripgrep))

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
  (keymap-global-set "C-c a" 'org-agenda)
  ;; Make `org-refile' look for up to :maxlevel headers in the current file.
  (setopt org-refile-targets '((nil . (:maxlevel . 2))))
  ;; Make `org-refile' use path-like selection of headers.
  (setopt org-refile-use-outline-path t)
  (setopt org-agenda-skip-scheduled-if-done t)
  (setopt org-agenda-skip-deadline-if-done t)
  (setopt org-agenda-custom-commands
          '(("n" "Week agenda and all TODOs scheduled for today (or without SCHEDULE)"
             ((agenda "") (tags-todo "SCHEDULED<=\"<today>\"|SCHEDULED=\"\"")))))
  ;; Enable `auto-revert-mode' because I typically access .org files from
  ;; multiple computers.
  :hook ((org-mode . auto-revert-mode)
         (org-agenda-mode . hl-line-mode)))

;; Spellchecking (requires `aspell` or similar to be installed on system)
(use-package flyspell
  :hook ((org-mode text-mode)
         (prog-mode . flyspell-prog-mode)))

;; Emacs IRC Client (ERC)
(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setopt erc-modules
          '(autojoin button completion fill irccontrols keep-place list
            match menu move-to-prompt netsplit networks nicks noncommands
            notifications readonly ring spelling stamp track))
  :hook (erc-mode . hl-line-mode))

;; Lightweight Rust mode
(use-package rust-mode
  :ensure t
  :custom
  (rust-rustfmt-switches '("+nightly"))
  :hook
  (rust-mode . (lambda ()
                 (setq fill-column 99)
                 (electric-pair-local-mode)
                 (subword-mode)))
  :bind (:map rust-mode-map
              ("C-M-\\" . rustfmt-region)))

(setf (alist-get 'my-cargo-test compilation-error-regexp-alist-alist)
      (list "\\(^thread .+ panicked at \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\):$\\)"
            2 3 4 nil nil (list 1 compilation-error-face)))

(add-to-list 'compilation-error-regexp-alist 'my-cargo-test)

(defun rustfmt-region (start end)
  (interactive "r")
  (shell-command-on-region start end "rustfmt +nightly" nil t))

(defun find-cargo-toml ()
  (interactive)
  (when-let ((dir (locate-dominating-file buffer-file-truename "Cargo.toml")))
    (message "Found `Cargo.toml' in `%s'" dir)
    (find-file (concat dir "Cargo.toml"))))

;;;; Aider

(when (executable-find "aider")
  (defun aider ()
    "Start Aider (AI pair programming) in `default-directory'"
    (interactive)
    (pop-to-buffer
     ;; NOTE: `--watch-files` doesn't work with `comint-mode'
     (make-comint "aider" "aider" nil
                  "--chat-mode" "ask" "--watch-files" "--subtree-only"))))

;;;; Dired

(setopt dired-listing-switches "--group-directories-first -lha")
(setopt dired-isearch-filenames 'dwim)
(setopt dired-dwim-target t)

;;; Theme

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
  (let* ((curr-time (decode-time))
         (curr-hour (decoded-time-hour curr-time))
         (curr-minute (decoded-time-minute curr-time))
         (curr-month (decoded-time-month curr-time)))
    (if (and (require 'solar nil t) calendar-latitude calendar-longitude)
        ;; We have setup latitude and longitude so we can use the sunrise and
        ;; sunset times to determine when to switch themes.
        (let* ((sunrise-sunset (solar-sunrise-sunset (calendar-current-date)))
               ;; The result of `solar-sunrise-sunset' outputs times as a
               ;; fraction of hours in a day.  We want to offset the
               ;; sunrise/sunset by half an hour toward noon.
               (sunrise (+ (caar sunrise-sunset) 0.5))
               (sunset (- (caadr sunrise-sunset) 0.5))
               ;; Convert current time to a floating point comparable to
               ;; sunrise/sunset.
               (curr-hour-fraction (+ curr-hour (/ curr-minute 60.0))))
          (if (<= sunrise curr-hour-fraction sunset)
              (light-theme)
            (dark-theme)))
      ;; Without latitude/longitude fallback to hard-coded values.
      ;; We have different hours for March---Aug
      (let* ((morning-hour (if (<= 3 curr-month 8) 7 8))
             (evening-hour (if (<= 3 curr-month 8) 17 16)))
        (if (< morning-hour curr-hour evening-hour)
            (light-theme)
          (dark-theme))))))

(cancel-function-timers #'theme-for-time-of-day)
(run-with-timer 0 60 #'theme-for-time-of-day)

;;; Defaults

(defun find-user-init-file (arg)
  "Edit init.el.  If called with a prefix argument, edit init-local.el"
  (interactive "P")
  (if arg
      (find-library "init-local")
    (find-file user-init-file)))

(keymap-global-set "C-c i" 'find-user-init-file)

;; Enable disabled functions (they are disabled for newcomers).
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setopt kill-do-not-save-duplicates t)

;; Make Dabbrev completion copy expansion case.
(setopt dabbrev-case-replace nil)

(setq-default
 fill-column 79
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

(setopt comint-input-ignoredups t)
(setopt comint-prompt-read-only t)

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

;;; File Local Variables

;; Local Variables:
;; outline-minor-mode: t
;; End:
