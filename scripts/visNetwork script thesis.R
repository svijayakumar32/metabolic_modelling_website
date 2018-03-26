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

nodes <- getNodes(tax)

edges <- getEdges(tax)

mynodes <- tibble(name = c('Structure detection',
                           'Biased Monte-Carlo',
                           'Visualization',
                           'Unsupervised clustering',
                           'Biased SNF',
                           'Metabolic HBMs',
                           'Metabex',
                           'FBAonline'),
                  color = list(border='red'))

myedges <- tribble(~from,                       ~to,
                   'Biased',                    'Structure detection',
                   'Structure detection',       'Biased Monte-Carlo',
                   'Structure detection',       'Visualization',
                   'Structure detection',       'Unsupervised clustering',
                   'Visualization',             'Metabex',
                   'Visualization',             'FBAonline',
                   'Unsupervised clustering',   'Metabolic HBMs',
                   'Unsupervised clustering',   'Biased SNF'
                   )

nodes <- bind_rows(nodes, mynodes)
edges <- bind_rows(edges, myedges)



visNetwork(nodes = nodes %>% 
             mutate(id=name, title=name, label=name),
           edges = edges) %>%
  visHierarchicalLayout(direction = 'LR', 
                        sortMethod = 'directed', 
                        levelSeparation=400,
                        nodeSpacing = 25) %>%
  visPhysics(hierarchicalRepulsion = list(
    nodeDistance=80,
    springLength=50
  )) %>%
  visNodes(shape='box', font=list(size=22)) %>%
  visEdges(smooth = list(enabled=TRUE, type='continuous'))