" Always use bash
set shell=/bin/bash

" Helpful defaults
set nocompatible " Disable complete vi compatibility
set backspace=indent,eol,start " Smarter backspacing
set history=50 " Keep command line history
set ignorecase " Case-insensitive searching
set matchtime=2 " Time between bracket jumping for showmatch
if has("mouse")
    set mouse=a " Enable the mouse
endif
set noautoindent " No default auto-indentation
set nobackup " Don't make backup files
set nohlsearch " No search highlighting
set noincsearch " No incremental searching
set noshowcmd " Don't show incomplete commands
set showmatch " Show matching brackets
set ruler " Show cursor information
set smartcase " Case-sensitive searching for searches with uppercase letters
set textwidth=0 " No hard line wrapping
set viminfo=\"50,'20 " Store session info in ~/.viminfo
set wildmode=list:longest " More useful command completion
" Hide annoying files from wildmenu/netrw/fuzzyfinder
set wildignore=*.orig,*.rej,*~,*.o,*.so,*.py[cdo],*.swp,*.prof
let g:netrw_list_hide='\.orig$,\.rej$,\.~$,\.s?o$,\.py[cdo]$,\.swp$,\.prof$'
fixdel " Try to fix backspace if it's broken
" Enable automatic filetype and ft plugins
filetype on
filetype plugin on

" Syntax highlighting settings
if has("syntax")
    let python_highlight_all=1 " More Python highlighting
    syntax enable " Automatic syntax highlighting
    syntax sync maxlines=100 " Sync highlighting with previous 100 lines
endif

" Auto-commands
if has("autocmd")
    " Tabbing settings
    autocmd FileType c,changelog,cheetah,cpp,cs,csh,css,django,dosini,haskell,java,javascript,mysql,objc,objcpp,perl,po,pyrex,python,rl,rst,ruby,sh,sql,tcsh,vim,zsh setlocal autoindent expandtab tabstop=8 shiftwidth=4 softtabstop=4
    autocmd FileType ant,dtml,genshi,html,htmlcheetah,htmldjango,kid,mako,php,sgml,smarty,xhtml,xml,xslt setlocal autoindent expandtab tabstop=8 shiftwidth=2 softtabstop=2
endif

" Convenience command to map something to every mode
command -nargs=+ AllMap noremap <args>|noremap! <args>|vnoremap <args>

" emacs-style bindings
inoremap <Esc><BS> <C-W>
cnoremap <Esc><BS> <C-W>
AllMap <C-A> <Home>
AllMap <C-E> <End>
AllMap <C-P> <Up>
AllMap <C-N> <Down>
AllMap <C-G> <Esc><Esc>
AllMap <Esc><Right> <C-Right>
AllMap <Esc><Left> <C-Left>

" Map FreeBSD/OS X keys properly
AllMap <Esc>[H <xHome>
AllMap <Esc>[F <xEnd>
AllMap <Esc>[5D <C-Left>
AllMap <Esc>[5C <C-Right>

" Fuzzy finder
AllMap <C-F> <Esc>:FufFileWithCurrentBufferDir<Return>
AllMap <C-B> <Esc>:FufBuffer<Return>

delcommand AllMap

" :Man for man pages
runtime ftplugin/man.vim

" Enable spell checking (vim7 only)
if has("spell")
    set spelllang=en_us " Global spell checking
    highlight clear SpellBad
    highlight SpellBad term=standout ctermfg=1 term=underline cterm=underline
    highlight clear SpellCap
    highlight SpellCap term=underline cterm=underline
    highlight clear SpellRare
    highlight SpellRare term=underline cterm=underline
    highlight clear SpellLocal
    highlight SpellLocal term=underline cterm=underline
    if has("autocmd")
        " Spell check where it works properly
        autocmd FileType c,changelog,cheetah,cpp,cs,csh,css,java,javascript,perl,po,python,rst,ruby,sh,tcsh,vim,dtml,genshi,html,htmlcheetah,htmldjango,kid,mako,php,smarty,xhtml,xml,xslt setlocal spell
    endif
endif

" Enable colors if available
if &term == "xterm-color"
    set t_Co=16
elseif &term == "xterm-256color"
    set t_Co=256
endif

" Set color scheme
if has("gui")
    set background=dark
    colorscheme billw
else
    set background=light
endif

" MacVim settings
if has("gui_macvim")
    set guifont=DejaVu\ Sans\ Mono:h12
    set guioptions-=T " Disable toolbar
    set guioptions-=r " Disable scrollbar
    set guioptions-=L " Disable left-hand scrollbar
    "set transparency=15 " Transparency's broken in 7.3e!
    set timeoutlen=100 " Lower ESC timeout
    set columns=80 lines=44 " Set default window size
    set fuoptions+=maxhorz
    autocmd FileType c,changelog,cheetah,cpp,cs,csh,css,django,dosini,haskell,java,javascript,mysql,objc,objcpp,perl,po,pyrex,python,rl,rst,ruby,sh,sql,tcsh,vim,zsh setlocal colorcolumn=80
    hi ColorColumn guibg=#121212
endif
