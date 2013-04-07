play:
	@mpd &> /dev/null

start:
	@mpd && ncmpcpp

all: help gui stop play

gui:
	@ncmpcpp

stop:
	@mpd --kill

help:
	@echo "Usage:"
	@echo "\tmusic\t\tstart music player"
	@echo "\tmusic gui\tstart gui"
	@echo "\tmusic stop\tkill music player"
	@echo "\tmusic help\tdisplay this help message"
