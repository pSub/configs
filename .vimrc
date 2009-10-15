set nocompatible
filetype plugin indent on
set autochdir
set backup
set backupdir=~/.vim/backup//
set directory=~/.vim/tmp//
set ttymouse=xterm
if has('mouse')
    set mouse=a
endif
set noerrorbells
set novisualbell
set incsearch
set lazyredraw
set textwidth=72
set tabstop=4
set softtabstop=4
set expandtab
set list
set listchars=tab:>-
set number
set numberwidth=4
set ruler
set ttyfast
set background=dark
colorscheme molokai
hi Comment ctermfg=darkgray
hi Pmenu guifg=#f6f3e8 guibg=#444444 gui=NONE ctermfg=white ctermbg=darkgray cterm=NONE
hi PmenuSel guifg=#000000 guibg=#cae682 gui=NONE ctermfg=black ctermbg=lightgreen cterm=NONE
syntax enable
