# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if [ "$(tty)" = "/dev/tty1" ]; then
    # see: https://github.com/swaywm/sway/wiki#xdg_current_desktop-environment-variable-is-not-being-set
    export XDG_CURRENT_DESKTOP=sway
    exec sway > $XDG_RUNTIME_DIR/sway.log 2>&1
fi
