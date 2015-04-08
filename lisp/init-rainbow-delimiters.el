;; Rainbow delimiters
(require-package 'rainbow-delimiters)

(with-eval-after-load 'rainbow-delimiters
(setq rainbow-delimiters-max-face-count 6)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "gray")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "aquamarine")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "gold")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "orchid")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "OliveDrab3")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "peru")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "red"))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide 'init-rainbow-delimiters)
