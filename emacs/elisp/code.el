;; Sets up all miscellaneous programming stuff

;; Since when is it _ever_ useful _not_ to indent?
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Adds line numbers
(require 'linum-relative)
(global-linum-mode 1)

;; electric-pair-mode sucks, autopair is better
(require 'autopair)
(autopair-global-mode)
(electric-indent-mode)

;; perl-mode is actually cancer
(defalias 'perl-mode 'cperl-mode)

;; Literally impossible to write code without this
(require 'evil)
(evil-mode 1)

;; Company, for completion (sometimes useful)
(require 'company)
(global-company-mode)

;; This is more convenient than M-x company-complete
(define-key evil-normal-state-map (kbd ";") 'company-complete)
(define-key evil-insert-state-map (kbd "C-;") 'company-complete)

;; I never use digraphs, but I sometimes hit C-k in insert mode due to muscle memory
(define-key evil-insert-state-map (kbd "C-k") 'nil)

;; I really don't have enough LaTeX stuff to put it in its own file...
(require 'tex)
(require 'latex-preview-pane)

;; Enable preview pane for Latex files only
(latex-preview-pane-enable)

;; Line numbers in a terminal is just weird
(add-hook 'eshell-mode-hook
	  #'(lambda () (linum-mode 0)))
