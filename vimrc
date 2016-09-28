set nocp
set nowrap
set autoindent
set mouse=a
set tw=70
set linebreak
set number
set clipboard=unnamedplus
syntax on
set virtualedit+=block
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set wildmode=list:full
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*~,*.hi
set shortmess=a
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
