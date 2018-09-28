#!/usr/bin/bash
yaourt -S chruby ruby-install ; \
cd ~
ln -s ~/.emacs.d/.vimrc ~/.vimrc ; \
ln -s ~/.emacs.d/.zshrc ~/.zshrc ; \
ln -s ~/.emacs.d/.bashrc ~/.bashrc ; \
ln -s ~/.emacs.d/.muttrc ~/.muttrc ; \
ln -s ~/.emacs.d/.tmux.conf ~/.tmux.conf ; \
ln -s ~/.emacs.d/.gitconfig ~/.gitconfig ; \
cd -
