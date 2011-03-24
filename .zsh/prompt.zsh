precmd() {
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
  *)
      echo '%#'
      ;;
 esac
}

export PROMPT='%{$fg[cyan]%}%n%{$reset_color%} at %{$fg[green]%}%m%{$reset_color%} in %{$fg_bold[green]%}%c%{$reset_color%} $(prompt_char) '
export RPROMPT='${vcs_info_msg_0_} ${vcs_info_msg_2_} ${vcs_info_msg_3_}'
