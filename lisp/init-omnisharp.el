;; Omnisharp
(use-package omnisharp
  :ensure t
  :config
  (defun my-csharp-mode-fn ()
    (turn-on-font-lock)
    (turn-on-auto-revert-mode)
    (setq indent-tabs-mode nil)
    (omnisharp-mode)
    (define-key omnisharp-mode-map (kbd "<C-tab>") 'omnisharp-auto-complete))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-fn t))

(provide 'init-omnisharp)
