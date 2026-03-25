# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if [ "$(tty)" = "/dev/tty1" ]; then
    exec sway > $XDG_RUNTIME_DIR/sway.log 2>&1
fi
