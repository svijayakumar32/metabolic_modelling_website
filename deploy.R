bookdown::clean_book(clean=TRUE); bookdown::render_book(list.files(pattern='*.Rmd'))

system(' rsync -r ~/git/metabolic_modelling_website/_book/* ~/public_html/metabolic_modelling/')
