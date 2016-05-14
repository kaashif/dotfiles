(require 'erc)
(setq erc-server "irc.freenode.net"
	  erc-port 6667
	  erc-prompt-for-password nil
	  erc-autojoin-channels-alist '(("freenode.net" "#ayylmao1234")
									("nixers.net" "#unix"))
	  erc-nick "kaashif1"
	  erc-user-full-name "Kaashif Hymabaccus"
	  erc-insert-timestamp-function 'erc-insert-timestamp-left
	  erc-nickserv-passwords '(("irc.nixers.net" (("kaashif1" . "megadeth"))))
	  erc-keywords '("kaashif" "pizzaroll"))

(defun erc-cmd-AYY (lmao)
  (interactive "Payy: ")
  (erc-send-message (format "ayy %s" lmao)))
