export LESS_TERMCAP_mb=$'\E[01;31m' \
LESS_TERMCAP_md=$'\E[01;38;5;74m' \
LESS_TERMCAP_me=$'\E[0m' \
LESS_TERMCAP_se=$'\E[0m' \
LESS_TERMCAP_so=$'\E[38;5;246m' \
LESS_TERMCAP_ue=$'\E[0m' \
LESS_TERMCAP_us=$'\E[04;38;5;146m'

PS1='\[\e[1;32m\][\u\[\e[m\]@\[\e[1;35m\]\h \[\e[1;34m\]\W]\[\e[m\] \[\e[1;32m\]$\[\e[m\] '

distro=`lsb_release -si`
if [ $distro == "ManjaroLinux" ]; then
    alias us="yay -Syu"
    alias ms="yay -Scc"
elif [ $distro == "Arch" ]; then
    alias us="yay -Syu"
    alias ms="yay -Scc"
elif [ $distro == "LinuxMint" ]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [ $distro == "Ubuntu" ]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [ $distro == "SolydXK" ]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [ $distro == "Debian" ]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
fi

export EDITOR="vim"
export GOPATH=$HOME/Workspace/go
export PATH=$PATH:$GOPATH/bin
export HISTCONTROL=ignoreboth:erasedups
