set nocompatible
set nu
" set title " set terminal title
set nobackup
set paste
set smartcase " ignore case if search patter is all lowercase, case-sensitive otherwise
set ignorecase
set showmatch

set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set laststatus=2

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,gbk,big5,utf-16,latin1

" basic programing setup
filetype on
filetype plugin indent on
syntax enable
syntax on
set autoindent
set copyindent
set tabstop=4
set expandtab
set softtabstop=4
set shiftwidth=4

" set cursorcolumn
set cursorline
