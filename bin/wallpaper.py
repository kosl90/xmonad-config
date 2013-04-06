#!/usr/bin/env python
# encoding: utf-8


wallpaper_dir = '$HOME/Pictures/wallpaper'


def main():
    global wallpaper_dir
    import os
    wallpaper_dir = os.path.expanduser(wallpaper_dir)


if __name__ == '__main__':
    import optparse
    opt = optparse.OptionParser()

    main()
