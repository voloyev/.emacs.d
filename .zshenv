# Editor
export EDITOR=vim
export VISUAL=vim

# Ruby
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

# Custom bin path
export PATH="$HOME/bin:$PATH"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# GO
export GOROOT="$HOME/bin/go"
export GOPATH="$HOME/workspace/go"

export PATH="$PATH:$GOPATH/bin"
export PATH="$GOROOT/bin:$PATH"

# rowel
export PATH="$PATH:$HOME/.roswell/bin"

# flutter
export PATH="$PATH:$HOME/bin/flutter/bin"

export PATH="$PATH":"$HOME/.pub-cache/bin"
# nimble
export PATH="$HOME/.nimble/bin:$PATH"

export FZF_DEFAULT_COMMAND='fd --type f'
export WORKON_HOME="$HOME/.virtualenvs"

# rust src
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"

export CARP_DIR=~/.Carp/

# ansible
export ANSIBLE_INVENTORY="/home/voloyev/workspace/ansible/hosts"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"
