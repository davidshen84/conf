" for program formatting
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set lcs=tab:>-,trail:.
set list    " turn on list
set number
set nowrap
set ai

" for search
set hlsearch
set incsearch
set noignorecase

" color darkblue
" color slate works better on Ubuntu 10.10
color slate

au filetypedetect BufEnter *.exe.config setf xml
au filetypedetect FileType javascript,xml,html,xhtml,php,python setlocal tabstop=2 softtabstop=2 shiftwidth=2
