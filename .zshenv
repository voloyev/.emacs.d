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
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$GOROOT/bin"

# rowel
export PATH="$PATH:$HOME/.roswell/bin"

# flutter
export PATH="$PATH:$HOME/bin/flutter/bin"

export PATH="$PATH":"$HOME/.pub-cache/bin"
