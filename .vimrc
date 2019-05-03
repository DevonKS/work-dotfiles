"Setting Space to be my leader
noremap <Space> <Nop>
let mapleader = "\<Space>"

"Configuring vim-plug
call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'
Plug 'easymotion/vim-easymotion'
Plug 'vim-syntastic/syntastic'
Plug 'scrooloose/nerdtree'

"Javascript tools
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'mattn/emmet-vim'
"Plug 'w0rp/ale'
Plug 'editorconfig/editorconfig-vim'

"Clojure Tools
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-surround'
"Plug 'venantius/vim-eastwood'
Plug 'venantius/vim-cljfmt'

call plug#end()

"plug setup
nnoremap <leader>pi :PlugInstall<CR>

set encoding=utf-8
set autochdir

inoremap jk <Esc>

nnoremap <leader>e :NERDTree<CR>
nnoremap <leader>o :edit<Space>

"Buffer Management
nnoremap <leader>bd :bd<CR>
nnoremap <leader>bl :ls<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader>bp :bp<CR>
nnoremap <leader>bs :w<CR>

nnoremap <leader>b :b
nnoremap <leader>vb :vsp<Space><bar><Space>b
nnoremap <leader>hb :sp<Space><bar><Space>b

"Window Management
nnoremap <leader>sv :vsp<Space>
nnoremap <leader>sh :sp<Space>
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l
nnoremap <leader>wc :close<CR>
nnoremap <leader>wm :only<CR>

"vimrc management
nnoremap <leader>ve :edit<Space>~/.vimrc<CR>
nnoremap <leader>vs :source<Space>~/.vimrc<CR>

"js tools setup
let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}
let g:jsx_ext_required = 0

let g:airline#extensions#ale#enabled = 1
let g:ale_javascript_eslint_use_global = 0
let g:ale_lint_on_text_changed='never'
let g:ale_lint_on_enter = 0 
let g:ale_fix_on_save = 1
let g:ale_lint_on_save = 1

"Syntastic setup
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"eastwood setup
let g:syntastic_clojure_checkers = ['eastwood']

"Omnicomplete setup
filetype plugin on
set omnifunc=syntaxcomplete#Complete

"netrw setup
"let g:netrw_banner = 0
"let g:netrw_liststyle = 3
"let g:netrw_browse_split = 4
"let g:netrw_altv = 1
"let g:netrw_winsize = 20
"augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
"augroup END

"Always show status bar
set laststatus=2
set number relativenumber

let g:airline_skip_empty_sections = 1

set wildmenu
set wildmode=list:longest,full

if !has("gui_running")
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
   let g:airline_symbols_ascii = 1
endif

set background=dark
colorscheme gruvbox
set guifont=Fira\ Code
set renderoptions=type:directx
set encoding=utf-8
