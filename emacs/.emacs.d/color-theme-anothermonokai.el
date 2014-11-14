;; Author: Simon Manning <simon@ecksd.com>
;; Emacs Molokai Author: Adam Lloyd (https://github.com/alloy-d)
;;
;; Note: Based on the molokai theme for vim by Tomas Restrepo, which
;; is in turn based on the monokai theme for textmate by Wimer
;; Hazenberg and a darker variant by Hamish Stuart Macpherson.
;;
;; This version is a modification of the color-theme-molokai by
;; Adam Lloyd: https://github.com/alloy-d/color-theme-molokai

(eval-when-compile
  (require 'color-theme))

(defun color-theme-anothermonokai ()
  "Color theme based on the Monokai color scheme for various editors."
  (interactive)
  (color-theme-install
   '(color-theme-anothermonokai
     ((foreground-color . "#F8F8F2")
      (background-color . "#272822")
      (cursor-color . "#F8F8F0"))

     (default ((t (:foreground "#F8F8F2" :background "#272822"))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))
     (italic ((t (:slant italic))))
     (region ((t (:background "#383830"))))
     (underline ((t (:underline t))))
     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#66D9EF"))))
     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))
     (escape-glyph ((t (:foreground "#E6DB74"))))
     (minibuffer-prompt ((t (:foreground "#66D9EF"))))

     (mode-line ((t (:foreground "#F8F8F2" :background "#474747"
                                 :box (:line-width 1 :color "#474747" :style released-button)))))
     (mode-line-buffer-id ((t (:foreground nil :background "#474747" :weight semi-bold))))
     (mode-line-inactive ((t (:foreground "#BCBCBC" :background "#474747"
                                          :box (:line-width 1 :color "#6A6A6A")))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#474747"))))
     (mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#474747"))))
     (font-lock-builtin-face ((t (:foreground "#A6E22E"))))
     (font-lock-comment-face ((t (:foreground "#75715E" :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground "#75715E" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-face ((t (:foreground "#E6DB74" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "#A6E22E" :slant italic))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#F92672"))))
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-type-face ((t (:foreground "#66D9EF"))))
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF"
                                              :background "#333333"))))
     (fringe ((t (:background "#272822"))))
     (linum ((t (:foreground "F8F8F2" :background "#272822"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     (hl-line ((t (:background "#293739"))))
     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     (isearch ((t (:foreground "#C4BE89" :background "#000000"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:foreground "#75715D" :background "#000000"))))
     (markdown-italic-face ((t (:slant italic))))
     (markdown-bold-face ((t (:weight bold))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t (:foreground "#66D9EF"))))
     (markdown-header-face-2 ((t (:foreground "#F92672"))))
     (markdown-header-face-3 ((t (:foreground "#A6E22E"))))
     (markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     (markdown-header-face-5 ((t (:foreground "#E6DB74"))))
     (markdown-header-face-6 ((t (:foreground "#FD971F"))))
     (markdown-inline-code-face ((t (:foreground "#66D9EF"))))
     (markdown-list-face ((t (:foreground "#A6E22E"))))
     (markdown-blockquote-face ((t (:slant italic))))
     (markdown-pre-face ((t (:foreground "#AE81FF"))))
     (markdown-link-face ((t (:foreground "#66D9EF"))))
     (markdown-reference-face ((t (:foreground "#66D9EF"))))
     (markdown-url-face ((t (:foreground "#E6DB74"))))
     (markdown-link-title-face ((t (:foreground "#F92672"))))
     (markdown-comment-face ((t (:foreground "#75715E"))))
     (markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
     (mumamo-background-chunk-major ((t (:background "#272822"))))
     (mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
     (outline-1 ((t (:foreground "#66D9EF"))))
     (outline-2 ((t (:foreground "#F92672"))))
     (outline-3 ((t (:foreground "#A6E22E"))))
     (outline-4 ((t (:foreground "#AE81FF"))))
     (outline-5 ((t (:foreground "#E6DB74"))))
     (outline-6 ((t (:foreground "#FD971F"))))
     (outline-7 ((t (:foreground "#66D9EF"))))
     (outline-8 ((t (:foreground "#F92672"))))
     (secondary-selection ((t (:background "#272822"))))
     (show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
     (show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
     (widget-inactive-face ((t (:background "#ff0000"))))
     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#66D9EF"))))

     (ido-subdir ((t (:foreground "#66D9EF"))))
     (ido-first-match ((t (:foreground "#F92672"))))
     (ido-only-match ((t (:foreground "#AE81FF"))))

     (ac-candidate-face ((t (:foreground "#66D9EF" :background "#31322A"))))
     (ac-selection-face ((t (:foreground "#F92672" :background "#49483E"))))
     )))


(provide 'color-theme-anothermonokai)
