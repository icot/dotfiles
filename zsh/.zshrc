# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

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
plugins=(wd fzf colorize z zsh-syntax-highlighting zsh-autosuggestions)

export FZF_COMPLETION_TRIGGER='~~'

source $ZSH/oh-my-zsh.sh

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
export EDITOR='vim'
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
alias racket="rlwrap racket"
alias sqlplus="rlwrap sqlplus"
alias sqlsys="sqlplus sys as sysdba"
alias csi="rlwrap csi"
alias dfs="cadaver https://dfs.cern.ch/dfs"
alias httpu="http --verify=no"
alias mutt-gmail="mutt -F ~/.mutt/muttrc-gmail"
alias mutt-cern="mutt -F ~/.mutt/muttrc-cern"
alias mutt-cern-local="mutt -F ~/.mutt/muttrc-cern-local"
alias cp2="rsync -avz"
alias vi="unset PYTHONPATH;vi"
alias bat="bat -p"
alias ec="emacs -nw"

alias l='exa -l --group-directories-first --git'
alias ll='exa -l --all --all --group-directories-first --git'
alias lt='exa -T --git-ignore --level=2 --group-directories-first'
alias llt='exa -lT --git-ignore --level=2 --group-directories-first'
alias lT='exa -T --git-ignore --level=4 --group-directories-first'
alias llfu='exa -bghHliS --git'
alias talacritty='tabbed -c alacritty --embed'

alias sbcl='rlwrap sbcl'
alias clisp='rlwrap clisp'

alias vi='vim'

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
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export PATH=$PATH:$HOME/Android/Sdk/platform-tools
export PATH=$PATH:~/.rakudobrew/bin
export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:/home/$USER/.gem/ruby/2.1.0/bin
export GOPATH=~/workspace/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/home/$USER/.cargo/bin
export PATH=$PATH:/home/$USER/apps/graalvm-ce-java11-20.3.0/bin

# Set WMNAME for JAVA Apps compatibilty
which wmname > /dev/null
if [ $? -eq 0 ]; then
        wmname LG3D
fi

bindkey '^R' history-incremental-search-backward

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
export GUIX_PROFILE="/home/$USER/.guix-profile"
source $GUIX_PROFILE/etc/profile
export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale

export PATH=$PATH:$GUIX_PROFILE/bin
export PATH=$HOME/.config/guix/current/bin:$PATH

export INFOPATH="${HOME}/.config/guix/current/share/info:${INFOPATH}"
export MANPATH="${HOME}/.guix-profile/share/man:/usr/share/man:${MANPATH}"
export XDG_CONFIG_DIRS="${HOME}/.desktop-profile/etc/xdg:${HOME}/.guix-profile/etc/xdg:$XDG_CONFIG_DIRS"
export XDG_DATA_DIRS="${HOME}/.desktop-profile/share:${HOME}/.guix-profile/share:$XDG_DATA_DIRS"

# NIX
which nix > /dev/null
if [ $? -eq 0 ]; then
        source /home/$USER/.nix-profile/etc/profile.d/nix.sh
fi

export PATH=$PATH:$HOME/.roswell/bin
export PATH=$PATH:$HOME/.emacs.d/bin

#NPM
export NPM_PACKAGES=$HOME/.npm-pkgs
export PATH=$PATH:$NPM_PACKAGES/bin


which starship > /dev/null
if [ $? -eq 0 ]; then
        eval "$(starship init zsh)"
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/$USER/.sdkman"
[[ -s "/home/$USER/.sdkman/bin/sdkman-init.sh" ]] && source "/home/$USER/.sdkman/bin/sdkman-init.sh"


