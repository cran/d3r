library(jqr)
library(purrr)

str <- '[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]'
str %>% dot
str %>% index %>% dotstr(name)


URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata//flare.json")

## Convert to list format
d3flare <- paste0(
  readLines(
    URL
  ),
  collapse = "\n"
)

d3flare_r <- jsonlite::fromJSON(d3flare,simplifyDataFrame=FALSE)

jq(d3flare,'..|.name?')

jq(
  d3flare,
  '. as $in | $in.children[] as $c |  [$in.name] + [$c.name] + [$c | .children[] | .name]'
)

jq(d3flare,'..|.children? | select(. != null) | select(length > 0)')

jq(d3flare,'..|.children? | select(. != null) | select(length > 0) | .name')

jq(d3flare,'.children[0] | ..|.children? | select(. != null) | select(length > 0) | [..|.name?] | select(length > 0) | [.]')

jq(d3flare,'..|.name? as $names | ..|.size?  as $sizes | [$names,$sizes] | ..|select(length=2)')

paths <- d3flare %>% paths()

path_r <- paths %>%
  map(jsonlite::fromJSON,simplifyVector=FALSE)

reduce(
  path_r[[752]],
  function(x,y){
    if(exists("y") && is.numeric(y)) y <- y + 1
    x[[y]]
  },
  .init = d3flare_r
)

leafs <- d3flare %>%
  jq('leaf_paths') %>%
  map(jsonlite::fromJSON,simplifyVector=FALSE)

map(
  leafs,
  ~reduce(
    .x,
    function(x,y){
      if(exists("y") && is.numeric(y)) y <- y + 1
      x[[y]]
    },
    .init = d3flare_r
  )
)
