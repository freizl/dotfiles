#################### 
### Misc Makefile
####################

GIT=git
GIT_SSH=git@github.com:freizl
GIT_HTTP=https://freizl@github.com/freizl

default: echo
echo:
	echo "hello, makefile"

####################
## projects
####################

projects=haisheng-sites freizl.github.com dive-into-haskell my-dot-emacs hoauth2
snaps=snap-poc snaplet-oauth snaplet-i18n a.haskellcn.org haskellcn.org

pull1:
	for x in $(projects) ; do \
		cd ../$$x && $(GIT) pull ; \
	done

pull2:
	for x in $(snaps) ; do \
		cd ../$$x && $(GIT) pull ; \
	done

push1:
	for x in $(projects) ; do \
		cd ../$$x && $(GIT) push ; \
	done

push2:
	for x in $(snaps) ; do \
		cd ../$$x && $(GIT) push ; \
	done

pullall: pull1 pull2
pushall: push1 push2

githubclone:
	cat gitconfig >> ~/.gitconfig
	$(GIT) config --global user.name "Haisheng.W.WU"
	for x in $(snaps) ; do \
		cd ../$$x && $(GIT) clone $(GIT_SSH)/$$(x) ; \
	done
