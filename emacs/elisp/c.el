;; Sets up stuff for C-style languages (C#, Java, C, C++ etc)

;; Flymake is pretty convenient for C
(add-hook 'c-mode-hook 'flymake-mode)

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

;; For some reason, it double-pairs them without this
(add-hook 'csharp-mode-hook
	  #'(lambda ()
	      (push ?{
		    (getf autopair-dont-pair :code))
	      (push ?{
		    (getf autopair-dont-pair :string))
	      (push ?{
		    (getf autopair-dont-pair :comment))
	      ))

