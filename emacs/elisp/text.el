;; Sets up stuff for writing (mostly) plaintext (e.g. markdown, rst, completely plain text)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; The obvious file associations
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; For blogging
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))

;; Fill text only, filling code gets messy
(set-variable 'fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
