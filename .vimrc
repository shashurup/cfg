call pathogen#infect()

set nocompatible
syntax on
filetype plugin indent on
set hlsearch
set incsearch
set expandtab
set tabstop=2
set shiftwidth=2
set smartindent
set ignorecase
set smartcase
set smarttab
set wildmode=list:longest
set ruler
set backspace=indent,eol,start
set hidden
set fencs=ucs-bom,utf-8,cp1251,koi8-r,cp866
set switchbuf=useopen
set splitright
set splitbelow

set guifont=Monospace\ 9
set guioptions-=m
set guioptions-=T

if (!has('gui_running'))
  set t_Co=256
endif

colorscheme wombat

let g:netrw_preview = 1

let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_working_path_mode = '0'

let g:slimv_leader=';'
let g:slimv_repl_split=4
let g:paredit_leader = ';'
let g:paredit_electric_return = 0
let g:lisp_rainbow = 1
let g:slimv_disable_clojure = 1
nnoremap ;' :sbuffer REPL<CR>GA

" vim-gains-nrepl
" Evals current top level form with help of paredit's [[
nmap <silent> ;e )[[va(:call NreplEval(@*)<CR>
" Evals visual selection
vnoremap <silent> ;e :call NreplEval(@*)<CR>
" Misc helper mappings for a word under cursor
nmap <silent> ;d viw:call NreplEval('(clojure.repl/doc '.@*.')')<CR>
nmap <silent> ;s viw:call NreplEval('(clojure.repl/source '.@*.')')<CR>
" Macroexpands innermost form
nmap <silent> ;m va(:call NreplEval('(clojure.core/macroexpand '''.@*.')')<CR>
" and visual selection
vnoremap <silent> ;m :call NreplEval('(clojure.core/macroexpand '''.@*.')')<CR>

map й q
map ц w
map у e
map к r
map е t
map н y
map г u
map ш i
map щ o
map з p
map х [
map ъ ]
map ф a
map ы s
map в d
map а f
map п g
map р h
map о j
map л k
map д l
map ж ;
map э '
map я z
map ч x
map с c
map м v
map и b
map т n
map ь m
map б ,
map ю .
map Ё ~
map Й Q
map Ц W
map У E
map К R
map Е T
map Н Y
map Г U
map Ш I
map Щ O
map З P
map Х {
map Ъ }
map Ф A
map Ы S
map В D
map А F
map П G
map Р H
map О J
map Л K
map Д L
map Ж :
map Э "
map Я Z
map Ч X
map С C
map М V
map И B
map Т N
map Ь M
map Б <
map Ю >
