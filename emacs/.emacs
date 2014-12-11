(setq package-list '(linum-relative magit evil haskell-mode auctex
									latex-preview-pane yasnippet helm
									geiser paredit))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Load all custom things
(add-to-list 'load-path "~/.emacs.d/")

;; Ensure packages are installed
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Save backups in a directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Vim-like scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Adds line numbers
(require 'linum-relative)
(global-linum-mode 1)

;; Shell for ansi-term et al
(setq explicit-shell-file-name "/usr/local/bin/zsh")

;; Sets lisp program names, for SLIME, run-scheme etc
(setq inferior-lisp-program "clisp")
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

;; Disables menu bar, scroll bar, toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; electric-pair-mode sucks, autopair is better
(require 'autopair)
(autopair-global-mode)
(electric-indent-mode)

;; perl-mode is actually cancer
(defalias 'perl-mode 'cperl-mode)

;; Don't just indent, complete too!
(require 'scheme-complete)
(eval-after-load 'scheme
   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

;; I can see the rainbow
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Colours!
(require 'color-theme-molokai)
(color-theme-molokai)

;; Emacs is an operating system with a great editor
(require 'evil)
(evil-mode 1)

;; Markdown!
(autoload 'markdown-mode "markdown-mode"
     "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; For blogging
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))

;; Fill text only, filling code gets messy
(set-variable 'fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Cyclic indent, haddock, types in minibuffer
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq inferior-haskell-find-project-root nil)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(setq haskell-process-type 'ghci)

;; Only really use this for C, C#
(require 'yasnippet)
(yas-global-mode 1)

;; Are there even any shell snippets?
(add-hook 'shell-mode-hook (lambda()
            (yas-minor-mode -1)))

;; Editing symlinked dotfiles gets annoying without this
(setq vc-follow-symlinks t)

;; For git
(require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)

;; Latex stuff
(require 'tex)
(require 'latex-preview-pane)

;; Auto-regenerating PDF? Yes please!
(add-hook 'latex-mode-hook 'latex-preview-pane-mode)

;; Line numbers in a terminal is just weird
(add-hook 'eshell-mode-hook
	  #'(lambda () (linum-mode 0)))

;; Make sure shitty GNU style indentation is gone
(setq c-default-style
	  '((java-mode . "java")
		(awk-mode . "awk")
		(csharp-mode . "c#")
		(other . "bsd")))

;; The actual default is 8 or something - crazy
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)

(add-hook 'csharp-mode-hook
	  #'(lambda ()
	      (push ?{
		    (getf autopair-dont-pair :code))
	      (push ?{
		    (getf autopair-dont-pair :string))
	      (push ?{
		    (getf autopair-dont-pair :comment))
	      ))

(require 'helm-config)
(require 'helm-grep)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t
      helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(haskell-process-type (quote ghci))
 '(inhibit-startup-screen t)
 '(send-mail-function (quote sendmail-send-it))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

;; Make super sure font is Terminus
(set-frame-font "Terminus 7" nil t)
(add-to-list 'default-frame-alist '(font . "Terminus 7"))

(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
	(end (copy-marker (or (search-forward "\n\n" nil t)
			      (point-max))))
	(fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
