#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

. ~/.xprofile

xrdb -merge ~/.Xresources

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

. ~/.xsession 


exec bspwm
#exec startxfce4
