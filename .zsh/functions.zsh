emulate -L zsh

mcd() { mkdir -p "$@" && cd "$@" }

dcp() {
    if [[ -d $2 ]] {
        mkdir -p $2 && cp $1 $2
    } else {
        mkdir -p dirname $2 && cp $1 $2
    }
}

lcd() {
    cd "$@" && ls
}

start restart stop reload(){
  local daemonPath distro
  if [[ $# -ne 1 ]]; then
    echo "Usage: start [restart, stop, reload] DAEMON"
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
      $daemonPath$1 $0
  else
      su --command="$daemonPath$1 $0"
  fi
  unset daemonPath distro
}

sync-unison(){ unison unison && unison all; }

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

greptodos(){
    find . -name $1 | xargs grep --no-filename -oE 'TODO:[^$]*' | sed s/TODO:/$2/ | combine - or ~/.todo.txt | sponge ~/.todo.txt
}
