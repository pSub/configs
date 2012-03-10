function precmd() {
  # vcs infos in the prompt
  vcs_info 'prompt'
}

# Choose the vcs logo as prompt
function prompt_char() {
 case "$vcs_info_msg_1_" in
  "git")
      echo '±'
      ;;
  "hg")
      echo '☿'
      ;;
  "darcs")
      echo '✼'
      ;;
  *)
      echo '%#'
      ;;
 esac
}

# Mark username red if you are root
if [ $UID -eq 0 ]; then usercolor="red"; else usercolor="cyan"; fi

# Mark hostname magenta if you are on a remote host
if [[ $(who am i) =~ (.+)$ ]]; then hostcolor="magenta"; else hostcolor="green"; fi

# Prompt inspired by Steven Loshs http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
export PROMPT='%{$fg[$usercolor]%}%n%{$reset_color%} at %{$fg[$hostcolor]%}%m%{$reset_color%} in %{$fg_bold[green]%}%c%{$reset_color%} $(prompt_char) '
export RPROMPT='${vcs_info_msg_0_} ${vcs_info_msg_2_} ${vcs_info_msg_3_}'
