export ZSH=/home/voloyev/.oh-my-zsh

ZSH_THEME="mortalscumbag"

plugins=(
  git rails ruby rbenv zsh-autosuggestions yaourt capistrano common-aliases knife gem docker django python zsh-autosuggestions
)
source $ZSH/oh-my-zsh.sh

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i
alias zshconfig="vim ~/.zshrc"

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

# go
export GOROOT="$HOME/bin/go"
export PATH="$PATH:$GOROOT/bin"
export GOPATH="$HOME/workspace/go"
export PATH="$PATH:$GOPATH/bin"

# editor
export EDITOR=vim
export VISUAL=vim
alias emc='emacsclient -t'

# rust src
export RUST_SRC_PATH="~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
# nim
export PATH=/home/voloyev/.nimble/bin:$PATH

# pipenv
eval "$(pipenv --completion)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# android
export ANDROID_HOME=~/bin/android/sdk

###-tns-completion-start-###
if [ -f /home/voloyev/.tnsrc ]; then 
    source /home/voloyev/.tnsrc 
fi
###-tns-completion-end-###
