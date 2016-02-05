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

setxkbmap -option grp:alt_shift_toggle,compose:prsc us,ru
xmodmap -e "clear control"
xmodmap -e "clear mod1"
xmodmap -e "keycode 37 = Alt_L Meta_L"
xmodmap -e "keycode 105 = Alt_R"
xmodmap -e "keycode 64 = Control_L"
xmodmap -e "keycode 108 = Control_R"
xmodmap -e "add mod1 = Alt_L Meta_L Alt_R"
xmodmap -e "add control = Control_L Control_R"

#xscreensaver -no-splash &
#kbdd &
#exec awesome
exec startxfce4
