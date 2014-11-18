#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

eval $(cat ~/.fehbg)

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources &
fi

xsetroot -cursor_name left_ptr &
compton -cCfF &
fcitx-autostart > ~/.fcitx.log 2>&1 &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
session=${1:-xmonad}

case $session in
	xmonad ) exec xmonad;;
	awesome ) exec awesome;;
	* ) exec $1;;
esac
