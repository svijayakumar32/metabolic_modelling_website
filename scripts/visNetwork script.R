library(tidyverse)
library(yaml)
library(visNetwork)

treefun_character <- function(x){
  return(list(name = x, children = list()))
}

treefun_onelist <- function(x){
  list(name = names(x),
       children = treefun(x[[1]]))
}

treefun <- function(x){
  c(x %>% keep(is_list) %>% map(treefun_onelist),
    x %>% keep(is_character) %>% map(treefun_character))
}

getNodes <- function(tree){
  bind_rows(tree[names(tree)!='children'], 
            map_df(tree$children %>% keep(is_list), getNodes)
  )
}

getEdges <- function(tree){
  if(length(tree$children)==0){return(NULL)}
  
  bind_rows(data_frame(from = tree$name, to = tree$children %>% map_chr('name')),
            map_df(tree$children, getEdges))
}

tax <- read_file('data/taxonomy.yaml') %>%
  yaml.load() %>%
  treefun_onelist()

visNetwork(nodes = getNodes(tax) %>% 
             mutate(id=name, title=name, label=name),
           edges = getEdges(tax)) %>%
  visHierarchicalLayout(direction = 'LR', sortMethod = 'directed', levelSeparation=300) %>%
  visPhysics(solver='forceAtlas2Based') %>%
  visNodes(shape='box') %>%
  visEdges(smooth = list(enabled=TRUE, type='continuous'))