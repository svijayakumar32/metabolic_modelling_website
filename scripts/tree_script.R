treefun_character <- function(x){
  return(list(name = x))
}

treefun_onelist <- function(x){
  list(name = names(x),
       children = treefun(x[[1]]))
}

treefun <- function(x){
  c(x %>% keep(is_list) %>% map(treefun_onelist),
    x %>% keep(is_character) %>% map(treefun_character))
}


tax <- read_file('data/taxonomy.yaml') %>%
  yaml.load() %>%
  treefun_onelist()

diagonalNetwork(tax)
