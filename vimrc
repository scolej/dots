set nocp
set clipboard=unnamed
set ruler
set nowrap
set autoindent
set ignorecase
set mouse=a
set tw=0
set linebreak
set number
set nowrapscan
syntax on
set virtualedit+=block
set tabstop=4
set shiftwidth=4
set softtabstop=2
set expandtab
set wildmode=list:full
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*~,*.hi
set shortmess=aoOtI
set fillchars+=vert:\
hi LineNr gui=NONE
hi StatusLine gui=NONE
hi StatusLineNC gui=NONE
hi VertSplit gui=NONE
nore ; :
nore , ;
map Y y$
map j gj
map k gk
map Down gj
map Up gk
noremap <leader>w :%s/\s\+$//ge<CR>
noremap <leader>h :set hls!<CR>
noremap <leader>d :e .<CR>
nnoremap <silent> <S-Right> <c-w>l
nnoremap <silent> <S-Left> <c-w>h
nnoremap <silent> <S-Up> <c-w>k
nnoremap <silent> <S-Down> <c-w>j
filetype on
au BufNewFile,BufRead *.flintconf set syntax=json
au BufNewFile,BufRead *.gradle set syntax=groovy
let NERDTreeIgnore=['\.hi$', '\.o$']
let NERDTreeMinimalUI=1
let NERDTreeSortOrder=[]
