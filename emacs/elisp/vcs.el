;; Sets up magit, mercurial etc

;; Built-in VC is annoying, let magit handle it
(setq vc-handled-backends nil)

;; Editing symlinked dotfiles gets annoying without this
(setq vc-follow-symlinks t)

;; For git
(require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)

