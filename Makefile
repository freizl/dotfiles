####################
### Misc Makefile
####################

GIT=git
GIT_SSH=git@github.com:freizl
HCN_SSH=git@github.com:HaskellCNOrg

XMONAD=xmonad-config
TOP := $(shell pwd)

### NOTES: some scripts not be tested!!!

default: echo
echo:
	if [ -d /media/cdrom ] ; then \
		echo "cdrom mount"; \
	if

####################
## sys-init
####################

init:
	apt-get install git haskell-platform emacs make
	apt-get install lightdm zsh w3m
	#apt-get install tmux proxychains

vbox:
	if [ ! -d "/media/cdrom"] then
		mkdir /media/cdrom
	fi
	mount /dev/cdrom /media/cdrom
	cd /media/cdrom && sudo VBOXxxx.sh

###
### for centos,
### `cabal install xmonad xmonad-contrib xmobar`
### ln the executor to /usr/bin
### install `gmrun` via source code
###

xmonad:
	sudo apt-get install xmonad suckless-tools gmrun xmobar
	sudo apt-get install lxappearance thunar
	#apt-get install ttf-liberation ttf-mscorefonts-installer
	sudo cp /usr/share/xsessions/xmonad.desktop  /usr/share/xsessions/xmonad.desktop.org
	sudo cp $(XMONAD)/xmonad.desktop /usr/share/xsessions/
	sudo cp /usr/share/gnome-session/sessions/xmonad.session /usr/share/gnome-session/sessions/xmonad.session.org
	sudo cp $(XMONAD)/xmonad.session /usr/share/gnome-session/sessions/
	sudo cp $(XMONAD)/xmonad.start /usr/local/bin
	sudo chmod +x /usr/local/bin/xmonad.start
	mkdir -p ~/.xmonad
	ln -s -f $(TOP)/$(XMONAD)/xmonad.hs ~/.xmonad/
	ln -s -f $(TOP)/$(XMONAD)/xmobarrc ~/.xmonad/
	cabal install xmonad-contrib

####################
## projects
####################

freizl=haisheng-sites freizl.github.com dive-into-haskell
haskellcn=snaplet-oauth snaplet-i18n snap-web haskellcn

push1:
	for x in $(haskellcn) ; do \
		cd ../$$x && $(GIT) push ; \
	done

pull1:
	for x in $(haskellcn) ; do \
		cd ../$$x && $(GIT) pull ; \
	done

gitconfig:
	$(GIT) config --global user.name "Haisheng.W.WU"


githubclone:
	cd ../ && $(GIT) clone $(HCN_SSH)/snaplet-oauth.git
	cd ../ && $(GIT) clone $(HCN_SSH)/snaplet-i18n.git
	cd ../ && $(GIT) clone $(HCN_SSH)/snap-web.git
	cd ../ && $(GIT) clone $(HCN_SSH)/haskellcn.git
