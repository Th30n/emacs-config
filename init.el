(let ((min-ver "24"))
  (when (version< emacs-version min-ver)
    (error "Your Emacs version is too old, v%s or higher is required" min-ver)))

;; MELPA repository for additional packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; SLIME (Superior Lisp Interaction Mode for Emacs)
(require 'slime)
(with-eval-after-load 'slime
  (slime-setup '(slime-fancy))
  (setq slime-auto-start 'ask)
  (setq slime-repl-history-remove-duplicates t)
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix))))

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

;; Omnisharp
(require 'omnisharp)

(defun my-csharp-mode-fn ()
  (turn-on-font-lock)
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil))

(with-eval-after-load 'omnisharp
  (when (equal system-type 'windows-nt)
    (setq omnisharp-server-executable-path
          "F:/src/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))
  (add-hook 'chsarp-mode-hook 'omnisharp-mode))

(add-hook 'csharp-mode-hook 'my-csharp-mode-fn t)

;; Haskell
(require 'haskell-mode)

(setq haskell-compile-command
   "ghc -Wall -ferror-spans -fforce-recomp -fwarn-tabs -c %s")
(setq haskell-font-lock-symbols nil)
(setq haskell-mode-hook (quote (turn-on-haskell-doc turn-on-haskell-indentation)))
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines t)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map
       (kbd "C-c c") 'haskell-process-cabal)))

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
