## Prerquisite

`sudo apt-get install xmonad suckless-tools gmrun xmobar cabal-install`
`sudo apt-get install lxappearance ttf-liberation ttf-mscorefonts-installer scrot`
`sudo apt-get install libghc-xmonad-contrib-dev`

## Reference

- <http://paddymullen.com/2010/01/17/xmonad-on-ubuntu/>
- <http://www.howtogeek.com/114728/how-to-use-xmonad-a-tiling-window-manager-for-linux/>
- <http://askubuntu.com/questions/129697/correct-way-to-get-a-nice-gtk-theme>
- <http://karuppuswamy.com/wordpress/2011/09/03/how-to-get-a-productive-desktop-based-on-xmonad-and-xmobar/>

## Main Config

- `/usr/share/xsessions/xmonad.desktop`
  Exec=xmonad => Exec=xmonad.start
- `/usr/share/gnome-session/sessions/xmonad.session`
- `/usr/local/bin/xmonad.start`
- `~/.xmonad/xmonad.hs`

## OSX 10.9

- make a final version after install `xmonad` lib and `osxmonad` lib

`LIBRARY_PATH=/usr/X11/lib:$LIBRARY_PATH ghc -v --make -framework=Cocoa -package-db=/Users/haiswu/Downloads/osxmonad/.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d Main.hs  -o xmonad`
