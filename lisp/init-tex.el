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

(defun my-latex-hook ()
  (setq TeX-command-default "LatexMk"))

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'latex-math-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'my-latex-hook)

(provide 'init-tex)
