;; Installs all packages that can be installed from ELPA etc

(setq package-list '(linum-relative magit evil haskell-mode auctex
              	     latex-preview-pane yasnippet helm
                     geiser paredit clojure-mode slime company))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;; Ensure packages are installed
(package-initialize)

;; fetch the list of packages available 
(package-refresh-contents)

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; I have a lot of non-packaged stuff in there
(add-to-list 'load-path "~/.emacs.d/")
