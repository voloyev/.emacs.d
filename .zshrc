export ZSH=/home/voloyev/.oh-dmy-zsh

ZSH_THEME="mortalscumbag"

plugins=(
  git ruby chruby zsh-autosuggestions yaourt capistrano common-aliases knife gem docker python
)
source $ZSH/oh-my-zsh.sh

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i
alias zshconfig="vim ~/.zshrc"

# ruby versions
source /usr/share/chruby/chruby.sh
source /usr/share/chruby/auto.sh

export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
# custom bin path
export PATH="$HOME/bin:$PATH"

# fzf
alias fzfp="fzf --preview \
    [[ $(file --mime {}) =~ binary ]] \
    && echo {} is a binary file \
    || (highlight -O ansi -l {} \
    || coderay {} || rougify {} \
    || cat {}) 2> /dev/null | head -500"

# go
export GOROOT="$HOME/bin/go"
export PATH="$PATH:$GOROOT/bin"
export GOPATH="$HOME/workspace/go"
export PATH="$PATH:$GOPATH/bin"

# editor
export EDITOR=vim
export VISUAL=vim
alias emc='emacsclient -t'
alias enw="emacs -nw"
alias fd="/usr/bin/fd"
# rust src
export RUST_SRC_PATH="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# nim
export PATH=/home/voloyev/.nimble/bin:$PATH

# pipenv
eval "$(pipenv --completion)"

# fzf in history
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias be="bundle exec"
alias mongosrv="mongod --dbpath=$HOME/mongodb"

alias dockstop="(docker stop $(docker ps -a -q))"
alias dockrm="(docker rm $(docker ps -a -q))"

export CARP_DIR=~/.Carp/

# rowel
export PATH="$PATH:$HOME/.roswell/bin"
# flutter
export PATH="$PATH:$HOME/bin/flutter/bin"
# ansible
export ANSIBLE_INVENTORY="/home/voloyev/workspace/ansible/hosts"
# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"

alias betty="~/workspace/ruby/betty/main.rb"
