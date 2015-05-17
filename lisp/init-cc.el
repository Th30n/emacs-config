;; CC Mode customizations

;; C++ style
(c-add-style "my-c++-style"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist
                (inclass . ++)
                (access-label . -)
                (arglist-intro . ++)
                (member-init-intro . ++))))

(setq-default c-default-style '((c++-mode . "my-c++-style")
                                (java-mode . "java")
                                (awk-mode . "awk")
                                (other . "gnu")))

;; C/C++ bindings
(defun my-c-init-hook ()
  (define-key c-mode-base-map (kbd "C-c C-k") 'compile))

(add-hook 'c-initialization-hook 'my-c-init-hook)

(defun my-c++-mode-hook ()
  (when (equal system-type 'windows-nt)
    (set (make-local-variable 'compile-command) "msbuild")))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(provide 'init-cc)
