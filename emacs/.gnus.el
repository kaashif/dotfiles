(setq user-mail-address "kaashif@kaashif.co.uk"
      user-full-name "Kaashif Hymabaccus"
      gnus-select-method '(nnmaildir "mail"
									 (directory "~/mail")
									 (directory-files nnheader-directory-files-safe)
									 (get-new-mail nil))
	  send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-extra-arguments nil      
      smtpmail-gnutls-credentials
      '(("smtp.zoho.com" 587 nil nil))
      smtpmail-starttls-credentials 
      '(("smtp.zoho.com" 587 "kaashif@kaashif.co.uk" nil))
      smtpmail-default-smtp-server "smtp.zoho.com"
      smtpmail-smtp-server "smtp.zoho.com"
      smtpmail-smtp-service 587
	  smtpmail-debug-info t
	  smtpmail-local-domain "kaashif.co.uk"
	  gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-number)
	  gnus-summary-line-format "%U%R%z %(%&user-date class='comment'>;  %-15,15f  %B%s%)\n"
	  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
	  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
	  gnus-sum-thread-tree-false-root ""
	  gnus-sum-thread-tree-indent " "
	  gnus-sum-thread-tree-leaf-with-other "├► "
	  gnus-sum-thread-tree-root ""
	  gnus-sum-thread-tree-single-leaf "╰► "
	  gnus-sum-thread-tree-vertical "│"
	  gnus-button-url 'browse-url-generic
	  browse-url-generic-program "opera"
	  browse-url-browser-function gnus-button-url
      gnus-always-read-dribble t
	  gnus-large-newsgroup nil)

(setq gnus-parameters
	  '((".*"
		 (gnus-use-scoring nil)
		 (display . all))))
