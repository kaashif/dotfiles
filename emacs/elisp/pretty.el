;; Fonts, colours that sort of thing

;; This is _most_ useful for Lisp, but still sort of useful for other languages
(require 'rainbow-delimiters)

;; Makes rainbow colours actually rainbow
(require 'cl-lib)
(require 'color)

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))
(global-rainbow-delimiters-mode)

;; Disables menu bar, scroll bar, toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; These colours look nice
(require 'color-theme-molokai)
(color-theme-molokai)

;; Make super sure font is Terminus
(set-frame-font "Terminus 7" nil t)
(add-to-list 'default-frame-alist '(font . "Terminus 7"))

