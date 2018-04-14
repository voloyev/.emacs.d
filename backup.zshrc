# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
 export ZSH=/home/user/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="fishy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git rails ruby rbenv zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Example aliases
alias zshconfig="vim ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
eval "$(nodenv init -)"

# custom path
export PATH="$HOME/bin:$PATH"

# fzf
alias fzfp="fzf --preview '[[ $(file --mime {}) =~ binary ]] && echo {} is a binary file || (highlight -O ansi -l {} || coderay {} || rougify {} || cat {}) 2> /dev/null | head -500'"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# go
export GOROOT="$HOME/bin/go"
export PATH="$PATH:$GOROOT/bin"
GOPATH="$HOME/Documents/temp/go"

# editor
export EDITOR=vim
export VISUAL=vim
alias emct='emacsclient -t'

# rust src
export RUST_SRC_PATH="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
export PATH="$HOME/.cargo/bin:$PATH"

