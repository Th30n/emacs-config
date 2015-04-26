;; Haskell
(require-package 'haskell-mode)

(setq haskell-compile-command
   "ghc -Wall -ferror-spans -fforce-recomp -fwarn-tabs -c %s")
(setq haskell-font-lock-symbols nil)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

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

(provide 'init-haskell)
