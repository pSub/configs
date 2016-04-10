gpg-connect-agent /bye
GPG_TTY=$(tty)
export GPG_TTY

unset SSH_AGENT_PID
export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
