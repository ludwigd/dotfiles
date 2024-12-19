# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

export EDITOR=vim
export _JAVA_AWT_WM_NONREPARENTING=1

# set current command as window title on xterm compatible terminals
trap 'printf "\033]0;%s\007" ${BASH_COMMAND}' DEBUG

# classic fedora prompt, but with highlighted workdir
export PS1="[\u@\h \[\033[97m\]\w\[\033[0m\]]\\$ "

# user-defined aliases
if [[ -f $HOME/.aliases ]]; then
    source $HOME/.aliases
fi
