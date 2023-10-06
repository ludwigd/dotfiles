# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if [ "$(tty)" = "/dev/tty1" ]; then
    eval $(gnome-keyring-daemon --start 2>/dev/null)
    export SSH_AUTH_SOCK
    exec sway > $XDG_RUNTIME_DIR/sway.log 2>&1
fi
