#################### 
### Misc Makefile
####################

GIT=git
GIT_SSH=git@github.com:freizl
GIT_HTTP=https://freizl@github.com/freizl

default: echo

echo:
	echo "hello, makefile"

## | checkout projects from github
githubclone:
	cat gitconfig >> ~/.gitconfig
	$(GIT) config --global user.name "Haisheng.W.WU"
	cd ../ && $(GIT) clone $(GIT_SSH)/haisheng-homepage.git 
	cd ../ && $(GIT) clone $(GIT_SSH)/freizl.github.com ../
	cd ../ && $(GIT) clone $(GIT_SSH)/dive-into-haskell
	cd ../ && $(GIT) clone $(GIT_SSH)/my-dot-emacs
	cd ../ && $(GIT) clone $(GIT_SSH)/snap-poc

gitpull:
	cd ../snap-poc && $(GIT) pull
	cd ../my-dot-emacs && $(GIT) pull
	cd ../freizl.github.com && $(GIT) pull
	cd ../haisheng-homepage && $(GIT) pull
	cd ../dive-into-haskell && $(GIT) pull

gitpush:
	cd ../snap-poc && $(GIT) push
	cd ../my-dot-emacs && $(GIT) push
	cd ../freizl.github.com && $(GIT) push
	cd ../haisheng-homepage && $(GIT) push
	cd ../dive-into-haskell && $(GIT) push
