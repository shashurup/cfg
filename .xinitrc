#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

xrandr --output VGA-0 --left-of DVI-0

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

#setxkbmap -option grp:alt_shift_toggle -option compose:prsc -layout us,ru
# swap CTRL and ALT
xkbcomp - $DISPLAY <<EOF
xkb_keymap {
	xkb_keycodes  {
	    include "evdev+aliases(qwerty)"

	    <LALT> = 37;
	    <LCTL> = 64;

	    <RCTL> = 108;
	    <RALT> = 105;

	    alias <ALGR> = <RALT>; };
	xkb_types     { include "complete" };
	xkb_compat    { include "complete" };
	xkb_symbols   { include "pc+us+ru:2+inet(evdev)+capslock(grouplock)+compose(prsc)" };
	xkb_geometry  { include "pc(pc105)" };
};
EOF

#xscreensaver -no-splash &
#kbdd &
#exec awesome
exec startxfce4
