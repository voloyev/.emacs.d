export ZSH=/home/voloyev/.oh-dmy-zsh

ZSH_THEME="mortalscumbag"

plugins=(
  git ruby chruby zsh-autosuggestions capistrano common-aliases knife gem docker python
)

source $ZSH/oh-my-zsh.sh

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i

# ruby versions chruby
source /usr/share/chruby/chruby.sh
source /usr/share/chruby/auto.sh


### Aliases
# editor
alias emc='emacsclient -c'
alias emt='emacsclient -t'
alias enw="emacs -nw"
alias fd="/usr/bin/fd"

alias zshconfig="vim ~/.zshrc"
alias betty="~/workspace/ruby/betty/main.rb"

# fzf
alias fzfp="fzf --preview \
    [[ $(file --mime {}) =~ binary ]] \
    && echo {} is a binary file \
    || (highlight -O ansi -l {} \
    || coderay {} || rougify {} \
    || cat {}) 2> /dev/null | head -500"

alias be="bundle exec"

alias mongosrv="mongod --dbpath=$HOME/mongodb"

alias dockstop="(docker stop $(docker ps -a -q))"
alias dockrm="(docker rm $(docker ps -a -q))"

### fzf in history
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

### Exports
# rust src
export RUST_SRC_PATH="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"

# go
export GOROOT="$HOME/bin/go"
export GOPATH="$HOME/workspace/go"

# nim
export PATH=/home/voloyev/.nimble/bin:$PATH

export CARP_DIR=~/.Carp/

# ansible
export ANSIBLE_INVENTORY="/home/voloyev/workspace/ansible/hosts"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"

# pipenv
eval "$(pipenv --completion)"
