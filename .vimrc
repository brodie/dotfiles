" Always use bash
set shell=/bin/bash

" Enable colors if available
if &term == "xterm-color"
    set t_Co=16
elseif &term == "xterm-256color"
    set t_Co=256
endif

" Helpful defaults
set nocompatible " Disable complete vi compatibility
set background=light " Easier-to-read colors for dark backgrounds
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
fixdel " Try to fix backspace if it's broken

" Syntax highlighting settings
if has("syntax")
    let python_highlight_all=1 " More Python highlighting
    syntax enable " Automatic syntax highlighting
    syntax sync maxlines=100 " Sync highlighting with previous 100 lines
endif

" Auto-commands
if has("autocmd")
    " Tabbing settings
    autocmd FileType c,changelog,cheetah,cpp,cs,csh,css,django,dosini,haskell,java,javascript,mysql,objc,objcpp,perl,po,pyrex,python,rst,ruby,sh,sql,tcsh,vim,zsh setlocal autoindent tabstop=4 shiftwidth=4 expandtab softtabstop=4
    autocmd FileType ant,dtml,genshi,html,htmlcheetah,htmldjango,kid,mako,php,sgml,smarty,xhtml,xml,xslt setlocal autoindent tabstop=2 shiftwidth=2 expandtab softtabstop=2
endif

" Convenience command to map something to every mode
command -nargs=+ AllMap noremap <args>|noremap! <args>|vnoremap <args>

" Map FreeBSD/OS X home/end keys properly
AllMap <Esc>[H <xHome>
AllMap <Esc>[F <xEnd>

delcommand AllMap

" :Man for man pages
runtime ftplugin/man.vim

" A nicer-looking tabline (vim7 only)
if exists(":tabnew") == 2
    highlight TabLine term=underline cterm=bold,underline ctermfg=Grey gui=underline
    highlight TabLineFill term=underline cterm=bold,underline gui=underline guibg=DarkGrey
    highlight TabLineSel term=reverse cterm=reverse gui=reverse
endif

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
