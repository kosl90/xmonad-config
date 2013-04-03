.PHONY: all help install clean
all: help

help:
	@echo "make install"
	@echo "make clean"
	@echo "make [help]"

install:
	@echo "install start..."
	apt-get install xmonad xmobar scrot ttf-wqy-microhei xloadimage thunar terminator stalonetray
	@echo "done"

clean:
	rm -rf xmonad.hi xmonad.o xmonad.errors
