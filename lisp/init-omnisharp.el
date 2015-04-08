;; Omnisharp
(require-package 'omnisharp)

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

(provide 'init-omnisharp)
