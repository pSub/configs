.PHONY: compile deploy update

compile:
	cd .elisp/haskell-mode; make compile
	cd .elisp/evil; make compile
	cd .elisp/helm; make batch-compile
	cd .elisp/magit; make

update:
	git submodule foreach git pull origin master

deploy:
	mkdir -p ~/.vim
	mkdir -p ~/.vim/tmp
	mkdir -p ~/.vim/backup
	mkdir -p ~/.config/zathura
	mkdir -p ~/.config/jumanji

	ln -sf ~/configs/bin ~/bin
	ln -sf ~/configs/.zshrc ~/.zshrc
	ln -sf ~/configs/.zsh ~/.zsh
	ln -sf ~/configs/.zprofile ~/.zprofile
	ln -sf ~/configs/.xinitrc ~/.xinitrc
	ln -sf ~/configs/.xinitrc ~/.xsession
	ln -sf ~/configs/.vimrc ~/.vimrc
	ln -sf ~/configs/.udisks-glue.conf ~/.udisks-glue.conf
	ln -sf ~/configs/.iptv-channels ~/.iptv-channels
	ln -sf ~/configs/.gitconfig ~/.gitconfig
	ln -sf ~/configs/.emacs.d ~/.emacs.d
	ln -sf ~/configs/.elisp ~/.elisp
	ln -sf ~/configs/.conkerorrc ~/.conkerorrc
	ln -sf ~/configs/.aurgetrc ~/.aurgetrc
	ln -sf ~/configs/.Xmodmap ~/.Xmodmap
	ln -sf ~/configs/.Xdefaults ~/.Xdefaults
	ln -sf ~/configs/.XCompose ~/.XCompose
	ln -sf ~/configs/.xbindkeysrc ~/.xbindkeysrc
	ln -sf ~/configs/jumanjirc ~/.config/jumanji/jumanjirc
	ln -sf ~/configs/.pentadactylrc ~/.pentadactylrc
	ln -sf ~/configs/.htoprc ~/.config/htop/htoprc

	chmod +x ~/.xsession
