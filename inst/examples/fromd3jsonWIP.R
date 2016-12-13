library(d3r)
library(jsonlite)
library(pipeR)
library(tibble)
library(dplyr)
library(purrr)

tmd <- treemap::random.hierarchical.data(
  depth = 3
)

data_json <- tmd %>>%
  d3r::d3_nest(value_cols="x")

tbl_d <- data_json %>>%
  jsonlite::fromJSON(
    simplifyDataFrame = FALSE
  ) %>>%
  as_tibble() %>>%
  group_by(id) %>>%
  summarize(children = list(children))

c1 <- tbl_d$children[[1]]
# try to manually do what we will need to recurse
c1_step1 <- map(c1,as_tibble)
c1_step2 <- map(
  c1_step1,
  ~.x %>>%
      group_by(id) %>>%
      summarize(children = list(children))
)
c1_step3 <- bind_rows(c1_step2)

step1 <- function(l){
  if("children" %in% names(l) && length(l$children) > 0) {
    l$children = map(l$children,as_tibble)
  } else {
    l$children = NULL
    l <- as_tibble(l)
  }
  l
}

step2 <- function(tbl){
  if("children" %in% names(tbl) && length(tbl$children) > 0) {
    tbl %>>%
      group_by(id) %>>%
      summarize(children = list(children))
  } else {
    tbl
  }
}

step3 <- function(tbl){
  bind_rows(tbl)
}

function(){
tbl_d_step1 <- mutate(
  tbl_d,
  children = map(children,step1)
)

tbl_d_step2 <- mutate(
  tbl_d_step1,
  children = map(children,~map(.x,step2))
)

tbl_d_step3 <- mutate(
  tbl_d_step2,
  children = map(children,step3)
)

tbl_d3 <- tmd %>>%
  d3r::d3_nest(json=FALSE)
}

tbl_children_steps <- function(l){
  if(
    is.list(l) && "id" %in% names(l) &&
    "children" %in% names(l) &&
    length(l$children) > 0
  ) {
    if("children" %in% unlist(map(l$children,~map(.x,names)))){
        l$children <- map(l$children, ~map(.x,step1))
        l$children <- map(l$children, ~map(.x,step2))
        l$children <- map(l$children, step3)
        str(l,max.level=1)
    } else {
      l
    }
  }
  as_tibble(l)
}

tbl_children <- function(l) {
  if(is.list(l) && "id" %in% names(l) && "children" %in% names(l) && length(l$children) > 0 ) {
    x <- as_tibble(l) %>>%
      group_by(id) %>>%
      summarize(children = c(children)) %>>%
      ungroup()

    x$children <- map(
      x$children,
      function(xx){
        if("children" %in% names(xx)){
          xx$children <- map(xx$children, as_tibble)
        } else {
          str(xx)
        }
        xx
      }
    )
  } else {
    l <- NULL
  }
  l
}

#d3r:::recurse(tbl_d, tbl_children)

