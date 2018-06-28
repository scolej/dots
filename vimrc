syntax on
filetype on
map Down gj
map Up gk
map Y y$
map j gj
map k gk
nmap <silent> <S-Down> :wincmd j<CR>
nmap <silent> <S-Left> :wincmd h<CR>
nmap <silent> <S-Right> :wincmd l<CR>
nmap <silent> <S-Up> :wincmd k<CR>
nore , ;
nore ; :
nore <leader>h :set hls!<CR>
nore <leader>w :%s/\s\+$//ge<CR>
nore gf gF
set autoindent
set backspace=indent,eol,start
set clipboard=unnamed
set expandtab
set fillchars+=vert:\ 
set ignorecase
set isfname-=:
set linebreak
set mouse=a
set nocp
set nojoinspaces
set nowrap
set nowrapscan
set number
set ruler
set shiftwidth=2
set shortmess=aoOtI
set smartcase
set softtabstop=2
set tabstop=4
set tw=0
set virtualedit+=block
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*~,*.hi
set wildmode=longest:full,full
set incsearch
set fo-=o

autocmd BufNewFile,BufRead Jenkinsfile set syntax=groovy

set completeopt=menuone,menu,longest
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

:noremap Q <Nop>

map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'
