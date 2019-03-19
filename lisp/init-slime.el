;; SLIME (Superior Lisp Interaction Mode for Emacs)
(require-package 'slime)

(with-eval-after-load 'slime
  (slime-setup '(slime-indentation slime-fancy))
  (setq slime-auto-start 'ask)
  (setq slime-repl-history-remove-duplicates t)
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix))))

(provide 'init-slime)
