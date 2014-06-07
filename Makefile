.PHONY: all help install clean
all: help

help:
	@echo "Usage:"
	@echo "\tmake install"
	@echo "\tmake clean"
	@echo "\tmake [help]"

install:
	@echo "install start..."
	sudo apt-get install cabal-install libiw-dev scrot ttf-wqy-microhei feh stalonetray conky-all mpd mpc ncmpcpp
	cabal install xmonad xmonad-contrib xmonad-extras xmobar --flags='all_extensions'
	@echo "done"

clean:
	rm -rf xmonad.hi xmonad.o xmonad.errors
