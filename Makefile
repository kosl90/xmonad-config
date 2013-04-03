.PHONY: all help install
all: help

help:
	echo "make install"
	echo "make [help]"

install:
	echo "install start..."
	apt-get install xmonad xmobar scrot ttf-wqy-microhei xloadimage thunar terminator stalonetray
	echo "done"
