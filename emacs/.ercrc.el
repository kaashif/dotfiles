(require 'erc)
(setq erc-server "irc.nixers.net"
	  erc-port 6667
	  erc-prompt-for-password nil
	  erc-autojoin-channels-alist '(("freenode.net" "#ayylmao1234")
									("nixers.net" "#unix")
									("unix.chat" "#unix"))
	  erc-nick "kaashif1"
	  erc-user-full-name "Kaashif Hymabaccus"
	  erc-insert-timestamp-function 'erc-insert-timestamp-left
	  erc-nickserv-passwords '(("irc.nixers.net" (("kaashif1" . "megadeth")))
							   ("unix.chat" (("kaashif1" . "megadeth"))))
	  erc-keywords '("kaashif" "pizzaroll"))

(defun erc-cmd-SLAP (victim)
  "Slap someone silly with a trout."
  (interactive)
  (let ((channel (erc-default-target)))
	(erc-send-action channel (format "slaps %s around a bit with a large trout"
									 victim))))
(defalias 'erc-cmd-TROUT 'erc-cmd-SLAP)

(defun erc-cmd-COMBOSLAP (&rest victims)
  "SLAP EVERYONE SILLY!"
  (dolist (victim victims)
	    (erc-cmd-SLAP victim)))
