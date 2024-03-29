(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; Ensure packages are installed
(package-initialize)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Download package when use-package is used
(setq use-package-always-ensure t)

(use-package rust-mode)
(use-package toml-mode)
(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
(use-package nlinum-relative
  :config
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (global-nlinum-relative-mode))
(use-package magit
  :bind ("C-c C-g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))
(use-package evil
  :config
  (evil-mode 1))
(use-package haskell-mode)
;(use-package tex
;  :ensure auctex)
;(use-package latex-preview-pane)
(use-package yasnippet)
(use-package helm)
(use-package geiser
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode))
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode))
(use-package clojure-mode)
(use-package slime-company)
(use-package slime
  :config
  (slime-setup '(slime-company)))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
(use-package ghc)
(use-package company-ghc)
(use-package oberon
  :mode ("\\.m\\'" . oberon-mode))
(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode))
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package gap-mode
  :mode ("\\.g\\'" . gap-mode)
  :config
  (define-key gap-mode-map (kbd "M-q") #'fill-paragraph)
  (setq gap-electric-semicolon nil))

(setq flycheck-clang-include-path (list "/usr/local/include"))

;; Load all custom things
(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq frame-background-mode 'dark)
(load-theme 'monokai t)

;; Save backups in a directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Vim-like scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

(define-key global-map (kbd "RET") 'newline-and-indent)


;; Sets lisp program names, for SLIME, run-scheme etc
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")
(setq geiser-active-implementations '(racket))

;; Built-in VC is annoying
(setq vc-handled-backends nil)

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

(add-hook 'cperl-mode-hook
          (lambda ()
            (clear-abbrev-table cperl-mode-abbrev-table)))

(setq save-abbrevs 'silently)

;; I can see the rainbow
(require 'rainbow-delimiters)

;; Less gray colours
(require 'cl-lib)
(require 'color)

(add-hook 'rust-mode-hook #'(lambda ()
  (setq flycheck-mode-hook #'flycheck-rust-setup)))

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))
(global-rainbow-delimiters-mode)

;; Markdown!
(autoload 'markdown-mode "markdown-mode"
     "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; For blogging
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))

;; Scribble
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . scheme-mode))

;; StumpWM RC file
(add-to-list 'auto-mode-alist '(".stumpwmrc" . common-lisp-mode))

;; C-x C-e in zsh
(add-to-list 'auto-mode-alist '("zsh.*\\'" . sh-mode))

;; Sage is basically Python
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))

;; Fill text only, filling code gets messy
(set-variable 'fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Cyclic indent, haddock, types in minibuffer
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq inferior-haskell-find-project-root nil)

;; Not in literate mode
(add-hook 'literate-haskell-mode-hook 'turn-off-haskell-indent)

(setq haskell-process-type 'cabal-repl)

;; Evil in an interactive buffer ... no
(add-hook 'haskell-interactive-mode-hook 'turn-off-evil-mode)

;; Set up ghc-mod stuff
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Set right tab-width in makefile
(add-hook 'makefile-bsdmake-mode-hook (lambda ()
										(setq tab-width 8)))

;; Editing symlinked dotfiles gets annoying without this
(setq vc-follow-symlinks t)

;; Please don't revert changes in my buffers
(setq magit-auto-revert-mode nil)

;; Latex stuff
;(require 'tex)
;(require 'latex-preview-pane)

;; Enable preview pane for Latex files only
;(latex-preview-pane-enable)

;; Line numbers in a terminal is just weird
(add-hook 'eshell-mode-hook
      #'(lambda () (linum-mode 0)))

;; The actual default is 8 or something - crazy
(setq-default c-basic-offset 4
              tab-width 4)

;; For some reason, it double-pairs them without this
(add-hook 'csharp-mode-hook
      #'(lambda ()
          (push ?{
            (getf autopair-dont-pair :code))
          (push ?{
            (getf autopair-dont-pair :string))
          (push ?{
            (getf autopair-dont-pair :comment))))


(require 'helm-config)
(require 'helm-grep)

;; I almost never want to go through _only_ commands in my history
(setq helm-mode-reverse-history t)
(setq helm-move-to-line-cycle-in-source nil)

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


;; This is more convenient than M-x company-complete
(define-key evil-normal-state-map (kbd ";") 'company-complete)
(define-key evil-insert-state-map (kbd "C-;") 'company-complete)

;; More completion backends
(add-to-list 'company-backends 'company-ghc)

;; Make super sure font is Terminus
(set-frame-font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*" nil t)
(add-to-list 'default-frame-alist '(font . "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"))
(set-face-attribute 'default nil :font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*")

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

(setq message-directory "~/.emacs.d/mail/")
(setq gnus-directory "~/.emacs.d/news/")
(setq nnfolder-directory "~/.emacs.d/mail/archive")

(defun fence-code-block (language)
  "Turns an indented code block into a Pandoc-style fenced one"
  (interactive "sLanguage: ")
  (forward-paragraph)
  (insert "```")
  (newline)
  (backward-paragraph)
  (newline)
  (insert (concat "```" language))
  (mark-paragraph)
  (narrow-to-region (region-beginning) (region-end))
  (replace-regexp "^\t" "")
  (replace-regexp "^    " "")
  (widen))

(global-set-key (kbd "C-c c") 'fence-code-block)

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))


(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

;; I never use digraphs, but I sometimes hit C-k in insert mode due to muscle memory
(define-key evil-insert-state-map (kbd "C-k") 'nil)

(define-key evil-normal-state-map (kbd "<left>") 'nil)
(define-key evil-normal-state-map (kbd "<right>") nil)
(define-key evil-normal-state-map (kbd "<up>") nil)
(define-key evil-normal-state-map (kbd "<down>") nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-commit-arguments nil)
 '(package-selected-packages
   (quote
    (yaml-mode gap-mode nhexl-mode nlinum-relative flycheck xcscope gherkin-mode racer flycheck-rust toml-mode rust-mode yasnippet use-package slime-company scala-mode paredit oberon magit linum-relative latex-preview-pane helm geiser evil cython-mode company-ghc clojure-mode auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/src/org/todo.org"))

;; No tabs, only spaces
(setq indent-tabs-mode nil)

(setq tramp-default-method "ssh")

(add-hook 'c-mode-hook (lambda ()
                         (setq indent-tabs-mode nil)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
		;; This would override `fill-column' if it's an integer.
		(emacs-lisp-docstring-fill-column t))
	(fill-paragraph nil region)))

(add-to-list 'revert-without-query ".+\.pdf")

(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

(kill-all-abbrevs)

(setq show-trailing-whitespace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq tex-fontify-script nil)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
(setq python-shell-interpreter "python3")
(setq flycheck-python-pycompile-executable "python3")
(setq mouse-yank-at-point t)

(setq enable-local-variables nil
      enable-local-eval nil)
