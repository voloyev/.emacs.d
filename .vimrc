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
Plugin 'fxn/vim-monochrome'
Plugin 'scrooloose/nerdtree'
Plugin 'ervandew/supertab'
Plugin 'majutsushi/tagbar'
Plugin 'rust-lang/rust.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-markdown'
Plugin 'townk/vim-autoclose'
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
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-endwise'
Plugin 'mileszs/ack.vim'
Plugin 'andreimaxim/vim-io'
Plugin 'rhysd/vim-crystal'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

syntax on
set mouse=a
set number
set guioptions-=m
set incsearch
set background=dark
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
"80 columns
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27
let &colorcolumn="80,".join(range(120,999),",")
"setup backups
"set backupdir=~/.vim/backup//
"set directory=~/.vim/swap//
"set undodir=~/.vim/undo//
set cursorline
set guioptions-=T "remove menu bar

hi cursorline cterm=none ctermbg=235 ctermfg=white guibg=darkred guifg=white
"set nocursorline

"change cursor shape
if has("autocmd")
  au VimEnter,InsertLeave * silent execute '!echo -ne "\e[1 q"' | redraw!
  au InsertEnter,InsertChange *
    \ if v:insertmode == 'i' | 
    \   silent execute '!echo -ne "\e[5 q"' | redraw! |
    \ elseif v:insertmode == 'r' |
    \   silent execute '!echo -ne "\e[3 q"' | redraw! |
    \ endif
  au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!
endif
"indent guide
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_guide_size=1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=235
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=235
hi MatchParen cterm=bold ctermbg=none ctermfg=green

let g:ackprg = 'ag --vimgrep'
"let g:ackprg = 'ag --nogroup --nocolor --column'
