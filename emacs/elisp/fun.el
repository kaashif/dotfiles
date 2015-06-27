;; Functions (although they are fun functions)

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
