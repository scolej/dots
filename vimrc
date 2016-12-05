filetype on
let NERDTreeIgnore=['\.hi$', '\.o$']
let NERDTreeMinimalUI=1
let NERDTreeSortOrder=[]
let NERDTreeWinPos="right"
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
nore <leader>c :%s/\[[0-9]*m//g<CR>
nore <leader>d :e .<CR>
nore <leader>h :set hls!<CR>
nore <leader>j :Ex<CR>
nore <leader>w :%s/\s\+$//ge<CR>
nore [ O<ESC>
nore ] o<ESC>
nore gf gF
nore <space> 15l
nore <backspace> 15h
set autoindent
set backspace=indent,eol,start
set clipboard=unnamed
set expandtab
set fillchars+=vert:\
set isfname-=:
set linebreak
set mouse=a
set nocp
set nojoinspaces
set nowrap
set nowrapscan
set number
set ruler
set shiftwidth=4
set shortmess=aoOtI
set smartcase
set softtabstop=2
set tabstop=4
set tw=0
set virtualedit+=block
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*~,*.hi
set wildmode=longest:full,full
syntax on
