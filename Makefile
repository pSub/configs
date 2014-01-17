.PHONY: deploy

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
	ln -sf ~/configs/.xprofile ~/.xprofile
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
	ln -sf ~/configs/cabal-config ~/.cabal/config
	ln -sf ~/configs/.stylish-haskell.yaml ~/.stylish-haskell.yaml
	ln -sf ~/configs/dwb/settings ~/.config/dwb/settings
	ln -sf ~/configs/dwb/keys ~/.config/dwb/keys
	ln -sf ~/configs/dwb/searchengines ~/.config/dwb/searchengines
