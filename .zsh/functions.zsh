emulate -L zsh

# Create a directory and change to it
mcd() { mkdir -p "$@" && cd "$@" }

dcp() {
    if [[ -d $2 ]] {
        mkdir -p $2 && cp $1 $2
    } else {
        mkdir -p dirname $2 && cp $1 $2
    }
}

# Change to directory and list the content
lcd() {
    cd "$@" && ls
}

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

# Very simple hexeditor that uses the editor
# specified in $EDITOR. Needs the package moreutils:
# http://joey.kitenet.net/code/moreutils/
hexeditor(){
    local input output
    if [[ ! -f $1 ]] {
        touch $1
    }
    input=$1
    output=$2
    if [[ $# -eq 1 ]] {
        output=$1
    }
    xxd $input | vipe | xxd -r | sponge $output
    unset input output
}

# Ugly function to grep ToDo-Entries.
greptodos(){
    find . -name $1 | xargs grep --no-filename -oE 'TODO:[^$]*' | sed s/TODO:/$2/ | combine - or ~/.todo.txt | sponge ~/.todo.txt
}
