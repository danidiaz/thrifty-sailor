set path=.,./delays,./do,./json,./main,./network,./network-sig,./prelude,./thrifty
set suffixesadd=.hs
set includeexpr=substitute(v:fname,'\\.','/','g')
set wildignore+=dist-newstyle/*

set include=^import\\\s*\\\(qualified\\\)\\\?

argadd package.cabal README.md

" https://www.gilesorr.com/blog/vim-variable-scope.html
let s:targets = { 'exe' : 'exe:thrifty', 'lib' : 'lib:thrifty', 'prelude' : 'lib:prelude', 'delays' : 'lib:delays', 'json' : 'lib:thrifty-json', 'network' : 'lib:t-network' , 'do' : 'lib:t-do' }

function CabalTargets()
    echo s:targets
endfunction

command! CabalTargets call CabalTargets()

function! Ghcid(which)
    execute "below terminal ++rows=10 ghcid --command=\"cabal v2-repl" s:targets[a:which] "\""
endfunction

command! -nargs=1 Ghcid call Ghcid("<args>") 

function! CabalRepl(which)
    execute "below terminal ++rows=10 cabal v2-repl" s:targets[a:which]
endfunction

command! -nargs=1 CabalRepl call CabalRepl("<args>") 
