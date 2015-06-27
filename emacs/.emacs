(add-to-list 'load-path "~/elisp")

(setq init-files-list '(packages code helm lisp c haskell vcs pretty text helm fun))
(dolist (init-file init-files-list)
  (library-load (symbol-name init-file)))

;; Save backups in a directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Vim-like scrolling
(setq scroll-step 1
      scroll-conservatively 10000)
