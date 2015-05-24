;; TeX setup
(require 'tex)
(require 'reftex)
(require-package 'auctex)
(require-package 'auctex-latexmk)

(auctex-latexmk-setup)

(setq TeX-auto-save t
      TeX-parse-self t
      reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'latex-math-mode)

(provide 'init-tex)
