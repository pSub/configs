.PHONY: compile

compile:
	cd .elisp/haskell-mode; make compile
	cd .elisp/evil; make compile
	cd .elisp/helm; make batch-compile
	cd .elisp/magit; make
