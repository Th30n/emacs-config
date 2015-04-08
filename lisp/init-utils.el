(defun require-package (package &optional min-version no-refresh)
  "Install PACKAGE with optional MIN-VERSION.
  If NO-REFRESH is non NIL, the available package list will not be refreshed."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (package-refresh-contents)
      (package-install package))))

(provide 'init-utils)
