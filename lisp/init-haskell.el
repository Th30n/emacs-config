;; Haskell
(require-package 'haskell-mode)

(setq-default haskell-compile-command
   "ghc -Wall -ferror-spans -fforce-recomp -fwarn-tabs -c %s"
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t
   haskell-process-suggest-remove-import-lines t
   haskell-cabal-list-comma-position 'after
   haskell-stylish-on-save t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "C-c C-c") 'comment-region))

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map
    (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map
    (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map
    (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map
    (kbd "C-c c") 'haskell-process-cabal))

(provide 'init-haskell)
