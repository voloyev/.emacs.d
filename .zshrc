export ZSH=/home/voloyev/.oh-my-zsh

ZSH_THEME="mortalscumbag"

plugins=(
  git rails ruby rbenv zsh-autosuggestions yaourt capistrano common-aliases knife gem docker django python zsh-autosuggestions
)
source $ZSH/oh-my-zsh.sh

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i
alias zshconfig="vim ~/.zshrc"

# ruby versions
source /usr/share/chruby/chruby.sh
source /usr/share/chruby/auto.sh

# nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
eval "$(nodenv init -)"

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

# rust src
export RUST_SRC_PATH="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# nim
export PATH=/home/voloyev/.nimble/bin:$PATH

# pipenv
eval "$(pipenv --completion)"
eval "$(luarocks path --bin)"

# fzf in history
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias be="bundle exec"
alias mongosrv="mongod --dbpath=$HOME/mongodb"

alias dockstop="(docker stop $(docker ps -a -q))"
alias dockrm="(docker rm $(docker ps -a -q))"
