set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
Plugin 'VundleVim/Vundle.vim'
" Keep Plugin commands between vundle#begin/end.
Plugin 'L9'
" Git plugin not hosted on Gpumvisible() ? \itHub" : "\itHub
"Plugin 'git://git.wincent.com/command-t.git'"
" git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
"YCM
Plugin 'Valloric/YouCompleteMe'
"zenburn
"Plugin 'jnurmine/Zenburn'
"vim-monochrome
Plugin 'fxn/vim-monochrome'
"NERDtree
Plugin 'scrooloose/nerdtree'
"supertab
Plugin 'ervandew/supertab'
"Tagbar
Plugin 'majutsushi/tagbar'
"Rust vim
Plugin 'rust-lang/rust.vim'
"multicursor
Plugin 'terryma/vim-multiple-cursors'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-markdown'
Plugin 'townk/vim-autoclose'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-ruby/vim-ruby'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'tpope/vim-bundler'
Plugin 'vimwiki/vimwiki'
Plugin 'stephpy/vim-yaml'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'python-mode/python-mode'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

syntax on
set mouse=a
set number
set guioptions-=m
set incsearch
"set background=dark
colorscheme monochrome
nmap <F3> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>
set tabstop=4 softtabstop=0 expandtab shiftwidth=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_user_command = 'find %s -type f'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
set shortmess+=c
set shell=/bin/bash
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar
