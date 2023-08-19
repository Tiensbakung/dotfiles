autoload -Uz compinit && compinit
setopt HIST_IGNORE_ALL_DUPS
setopt INTERACTIVE_COMMENTS
autoload -U promptinit && promptinit
autoload -U colors && colors
zstyle ':completion:*' rehash true
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

PROMPT="%{$fg_bold[green]%}%n%{$reset_color%}@%{$fg_bold[blue]%}%m %{$fg_bold[cyan]%}%1~ $ %{$reset_color%}"

distro=`lsb_release -si`
if [[ $distro == "ManjaroLinux" ]]; then
    alias us="yay -Syu"
    alias ms="yay -Scc"
elif [[ $distro == "Arch" ]]; then
    alias us="yay -Syu"
    alias ms="yay -Scc"
elif [[ $distro == "LinuxMint" ]]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [[ $distro == "Ubuntu" ]]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [[  $distro == "SolydXK" ]]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
elif [[  $distro == "Debian" ]]; then
    alias us="sudo apt update && sudo apt -y full-upgrade"
    alias ms="sudo apt-get autoremove && sudo apt-get autoclean"
fi
