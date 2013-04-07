.PHONY: all help install clean
all: help

help:
	@echo "Usage:"
	@echo "\tmake install"
	@echo "\tmake clean"
	@echo "\tmake [help]"

install:
	@echo "install start..."
	sudo apt-get install xmonad xmobar scrot ttf-wqy-microhei xloadimage thunar terminator stalonetray conky-all mpd mpc ncmpcpp
	@echo "done"

clean:
	rm -rf xmonad.hi xmonad.o xmonad.errors
