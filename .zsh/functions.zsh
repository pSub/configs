# -*- mode: shell-script -*-

emulate -L zsh

# Handle the start/stop/restart and reload of daemons uniformly.
# It's able to handle multiple daemons.
start restart stop reload(){
  local daemonPath distro cmd
  if [[ $# -le 0 ]]; then
    echo "Usage: start [restart, stop, reload] DAEMON [DAEMON ...]"
    return 1
  fi
  distro=$(cat /etc/issue)
  case $distro in
          *"Arch Linux"*)
                daemonPath="/etc/rc.d/" ;;
          *"Debian GNU/Linux"*)
                daemonPath="/etc/init.d/" ;;
          *)
                echo "Distro not supported"
                exit 1
  esac
  if [[ $UID = 0 ]]; then
      foreach daemon ($*); do
        $daemonPath$daemon $0
      done
  else
      foreach daemon ($*); do
         cmd="$cmd $daemonPath$daemon $0;"
      done
      su --command="$cmd"
  fi
  unset daemonPath distro
}

# Peristent directory stack by
# Christian Neukirchen <http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html>
DIRSTACKSIZE=9
DIRSTACKFILE=~/.zdirs
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi
