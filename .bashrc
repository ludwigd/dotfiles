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

# make java apps work on sway
export _JAVA_AWT_WM_NONREPARENTING=1

# set default window title to 'user:pwd'
export PROMPT_COMMAND='printf "\033]0;%s:%s\007" "${USER}" "${PWD/#$HOME/\~}"'

# customize prompt when running on fedora
if [[ $(awk -F= '/^ID/{print $2}' /etc/os-release) == "fedora" ]]; then
    export PROMPT_COLOR='1;33'
    export PROMPT_DIR_COLOR='1;34'
    #export PROMPT_START='['
    #export PROMPT_END=']'
    export PROMPT_USERHOST='\u'
    #export PROMPT_SEPARATOR=':'
    #export PROMPT_DIRECTORY='\w'
fi

# user-defined aliases
if [[ -f $HOME/.aliases ]]; then
    source $HOME/.aliases
fi
