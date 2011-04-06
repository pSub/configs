function precmd() {
  vcs_info 'prompt'
}

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

if [ $UID -eq 0 ]; then usercolor="red"; else usercolor="cyan"; fi

export PROMPT='%{$fg[$usercolor]%}%n%{$reset_color%} at %{$fg[green]%}%m%{$reset_color%} in %{$fg_bold[green]%}%c%{$reset_color%} $(prompt_char) '
export RPROMPT='${vcs_info_msg_0_} ${vcs_info_msg_2_} ${vcs_info_msg_3_}'
