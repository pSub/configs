# -*- mode: shell-script -*-

emulate -L zsh

# Peristent directory stack by
# Christian Neukirchen <http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html>
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}
