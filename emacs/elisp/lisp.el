;; Sets up SLIME, geiser etc

;; Sets lisp program names, for SLIME, run-scheme etc
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq scheme-program-name "racket")
(setq geiser-active-implementations '(racket))
(add-hook 'scheme-mode-hook 'geiser-mode)

;; For that sweet AST manipulation
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; Scribble - for scheme documentation
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . scheme-mode))


