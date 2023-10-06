set background=dark
syntax on
set rnu

augroup filetypedetect
	" Mail
	autocmd BufRead,BufNewFile *mutt-* setfiletype mail
augroup END
