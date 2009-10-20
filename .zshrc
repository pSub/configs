# Created by pSub for 4.3.10
PROMPT="[%n@%m %c]%# "
alias when="when --futur=0 --past=0"
alias ls="ls --color=auto"
if [ "$(tty)" = "/dev/tty1" ]; then
   (startx -- -nolisten tcp  &) && exit
fi
