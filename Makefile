.PHONY: all install clean

all: install


install:
	@#sudo apt-get install cabal-install libiw-dev scrot ttf-wqy-microhei feh stalonetray conky-all mpd mpc ncmpcpp xfce4-notifyd
	@#cabal install xmonad xmonad-contrib xmonad-extras xmobar --flags='all_extensions'
	ln -sf $$(pwd)/xinitrc ~/.xinitrc


clean:
	rm -rf xmonad.hi xmonad.o xmonad.errors
