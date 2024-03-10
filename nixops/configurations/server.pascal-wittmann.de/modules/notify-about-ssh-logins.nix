{ config, lib, pkgs, ... }:
{
  environment.etc."ssh/sshrc" = {
    mode = "750";
    text = ''
      export LANG=C
      YOUR_CITY="Hamburg"
      TELEGRAM_CHAT_ID="-4111240472"
      TELEGRAM_BOT_TOKEN="***REMOVED***"
      LOGGED_USER="$(/usr/bin/whoami)"
      LOGGED_HOST="$(/usr/bin/hostname -f)"
      SERVER_IP="$(/usr/bin/hostname -I | /usr/bin/cut -d ' ' -f 1)"

      LOGGED_IP=''${SSH_CONNECTION%% *}
      LOGGED_TTY="$(/usr/bin/ps -p "$$" -o tname h)"

      NOW="$(/usr/bin/date)"
      MESSAGE="$(/usr/bin/echo -e "<strong>⚠️ SSH Login Notification ⚠️</strong>\n\n<u>Host</u>: $LOGGED_HOST ($SERVER_IP)\n<u>User</u>: $LOGGED_USER\n<u>User IP</u>: $LOGGED_IP\n<u>Time</u>: $NOW")"

      /usr/bin/curl --silent --output /dev/null \
                    --data-urlencode "chat_id=$TELEGRAM_CHAT_ID" \
                    --data-urlencode "text=$MESSAGE" \
                    --data-urlencode "parse_mode=HTML" \
                    --data-urlencode "disable_web_page_preview=true" \
                    "https://api.telegram.org/bot$TELEGRAM_BOT_TOKEN/sendMessage"
    '';
    
  };
}
