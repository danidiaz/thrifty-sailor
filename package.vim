set path=$PWD,$PWD/library,$PWD/delays,$PWD/prelude
set suffixesadd=.hs
set includeexpr=substitute(v:fname,'\\.','/','g')
set wildignore+=dist-newstyle/*

set include=^import\\\s*\\\(qualified\\\)\\\?

argadd package.cabal README.md Main.hs

function! GHCId(which)
    if     a:which == "exe"
        let l:target = "exe:thrifty-sailor"
    elseif a:which == "lib"
        let l:target = "lib:thrifty-sailor"
    elseif a:which == "prelude"
        let l:target = "lib:prelude"
    elseif a:which == "delays"
        let l:target = "lib:delays"
    elseif a:which == "json"
        let l:target = "lib:thrifty-json"
    endif
    execute "below terminal ++rows=10 ghcid --command=\"cabal new-repl" l:target "\""
endfunction

command! -nargs=1 GHCId call GHCId("<args>") 
