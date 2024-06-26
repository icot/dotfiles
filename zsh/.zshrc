# Clone OMZ if not present
if [[ ! -d ~/.ohmyzsh ]];then
  git clone https://github.com/ohmyzsh/ohmyzsh ~/.ohmyzsh
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.ohmyzsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="agnoster"
ZSH_THEME="daveverwer"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

if [[ ! -d $ZSH/custom/plugins/zsh-autosuggestions ]];then
  git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.ohmyzsh/custom}/plugins/zsh-autosuggestions
fi

if [[ ! -d $ZSH/custom/plugins/zsh-syntax-highlighting ]];then
  git clone https://github.com/zsh-users/zsh-syntax-highlighting ${ZSH_CUSTOM:-~/.ohmyzsh/custom}/plugins/zsh-syntax-highlighting
fi

plugins=(wd colorize z zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Install or initialize ASDF
if [[ ! -d ~/.asdf ]];then
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.13.1
else
    . "$HOME/.asdf/asdf.sh"
fi

# ASDF Plugins
# age
# babashka
# bat
# chezscheme
# chicken
# clojure
# colima/lima
# eza
# fd
# fx
# ghidra
# glab
# guile
# helm
# jbang
# k9s
# kcat
# kcctl
# kubectl
# leiningen
# racket
# ripgrep
# sbcl
# shellcheck
# sops
# sopstool
# starship

# User configuration
export PATH=~/bin:/usr/local/bin:$PATH
export PATH=~/apps/bin:$PATH
export PATH=~/apps/JohnTheRipper/run:$PATH
export PATH=~/.local/bin:$PATH

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
#if [[ -n $SSH_CONNECTION ]]; then
export EDITOR='emacsclient -nw'
#else
#  export EDITOR='vim'
#fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias ls="ls --color "
alias grep="grep --color "
alias rm="rm -v"
alias ll="ls --color -l "
alias mutt-gmail="mutt -F ~/.mutt/muttrc-gmail"
alias mutt-cern="mutt -F ~/.mutt/muttrc-cern"
alias mutt-cern-local="mutt -F ~/.mutt/muttrc-cern-local"
alias bat="bat -p"

alias l='eza -l --group-directories-first --git'
alias ll='eza -l --all --all --group-directories-first --git'
alias lt='eza -T --git-ignore --level=2 --group-directories-first'
alias llt='eza -lT --git-ignore --level=2 --group-directories-first'
alias lT='eza -T --git-ignore --level=4 --group-directories-first'
alias llfu='eza -bghHliS --git'
alias talacritty='tabbed -c alacritty --embed'

alias sbcl='rlwrap sbcl'
alias clisp='rlwrap clisp'

alias vi='emacsclient -c -n'

# Vi Mode
function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}

#bindkey -v
zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

# ENVIRONMENT
export PATH=$PATH:$HOME/Android/Sdk/platform-tools

# GOLANG
if [[ -d /usr/local/go ]];then
    export GOROOT=/usr/local/go
    export PATH=$PATH:$GOROOT/bin
fi

#export PATH=$PATH:/home/$USER/apps/graalvm-ce-java11-20.3.0/bin

bindkey '^R' history-incremental-search-backward
bindkey -r "^T"

export ORACLE_HOME=~/apps/instantclient_12_1
export LD_LIBRARY_PATH=~/apps/instantclient_12_1
#xmodmap ~/.speedswapper

#source ~/.ssh/rc_agent

GNURADIO_PATH=/opt/gnuradio-3.7.9.2
export PATH=$PATH:$GNURADIO_PATH/bin

# Add GNU Radio python libraries to python search path
if [ $PYTHONPATH ]; then
        export PYTHONPATH=$PYTHONPATH:$GNURADIO_PATH/lib/python2.7/dist-packages
else
        export PYTHONPATH=$GNURADIO_PATH/lib/python2.7/dist-packages
fi
export PATH=~/.rakudobrew/bin:$PATH
export PATH="$HOME/workspace/esp/xtensa-esp32-elf/bin:$PATH"
export IDF_PATH=~/workspace/esp/esp-idf

# Emacs vterm
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


# GUIX
if [[ -d /home/$USER/.guix-profile ]];then
 export GUIX_PROFILE="/home/$USER/.guix-profile"
 export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
 source $GUIX_PROFILE/etc/profile

 export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
 export PATH=$PATH:$GUIX_PROFILE/bin
 export PATH=$HOME/.config/guix/current/bin:$PATH

 export INFOPATH="${HOME}/.config/guix/current/share/info:${INFOPATH}"
 export MANPATH="${HOME}/.guix-profile/share/man:/usr/share/man:${MANPATH}"
 export XDG_CONFIG_DIRS="${HOME}/.desktop-profile/etc/xdg:${HOME}/.guix-profile/etc/xdg:$XDG_CONFIG_DIRS"
 export XDG_DATA_DIRS="${HOME}/.desktop-profile/share:${HOME}/.guix-profile/share:$XDG_DATA_DIRS"

 # Enable extra profiles
 [ "$(ls -A $GUIX_EXTRA_PROFILES)" ] &&
 for i in $GUIX_EXTRA_PROFILES/*; do
   profile=$i/$(basename "$i")
   if [ -f "$profile"/etc/profile ]; then
     GUIX_PROFILE="$profile"
     source "$GUIX_PROFILE"/etc/profile
   fi
   unset profile
 done
fi

# raco pkg
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# NIX
which nix > /dev/null
if [ $? -eq 0 ]; then
        source /home/$USER/.nix-profile/etc/profile.d/nix.sh
fi

export PATH=$PATH:$HOME/.roswell/bin

#Swift
export PATH=$HOME/apps/swift/usr/bin:"${PATH}"

which starship > /dev/null
if [ $? -eq 0 ]; then
        eval "$(starship init zsh)"
fi

# ASDF
if [[ -d /home/$USER/.asdf ]];then
source "$HOME/.asdf/asdf.sh"
fi

# Rust
if [[ -d /home/$USER/.cargo ]];then
source "$HOME/.cargo/env"
fi

# Emacs EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/$USER/.sdkman"
[[ -s "/home/$USER/.sdkman/bin/sdkman-init.sh" ]] && source "/home/$USER/.sdkman/bin/sdkman-init.sh"


