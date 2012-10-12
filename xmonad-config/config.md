`sudo apt-get install xmonad suckless-tools gmrun xmobar`
`sudo apt-get install lxappearance ttf-liberation ttf-mscorefonts-installer`

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