use strict;
use warnings;
use utf8;

weechat::register ("greentext", "shmibs", "0.1", "GPL", "auto-greenify greentext", "", "");
weechat::hook_modifier("input_text_for_buffer", "catch_send", "");

sub catch_send {
	my ($data, $modifier_name, $buffer, $rval) = @_;

	if(substr($rval, 0, 1) eq ">") {
		# greenify
		$rval="\x{03}09".$rval;
	}

	return $rval;
}
