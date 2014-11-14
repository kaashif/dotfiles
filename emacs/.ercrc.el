(require 'erc)
(setq erc-server "irc.nixers.net"
	  erc-port 6667
	  erc-prompt-for-password nil
	  erc-autojoin-channels-alist '(("irc.nixers.net" "#nixers"))
	  erc-nick "pizza"
	  erc-user-full-name "pizza"
	  erc-insert-timestamp-function 'erc-insert-timestamp-left
	  erc-nickserv-passwords '(("irc.nixers.net" (("pizza" . "megadeth")))))
