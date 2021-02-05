library(tidyverse)
library(plotrix)

readPoints <- function(filename){
  coords = strsplit(readLines(filename), ", ")[[1]]
  data = enframe(coords) %>% separate(value, into = c("x", "y"), sep = "/") %>% 
    mutate(x = as.numeric(x), y = as.numeric(y)) %>%
    select(x, y)
  return(data)
}

contains <- function(P, x, y){
  Xmin = P@x - P@width
  Xmax = P@x + P@width
  Ymin = P@y - P@width
  Ymax = P@y + P@width
  return( Xmin <= x & x <= Xmax &  Ymin <= y & y <= Ymax)
}

containsWithExpansion <- function(P, x, y){
  Xmin = P@x - (P@width + expansion)
  Xmax = P@x + (P@width + expansion)
  Ymin = P@y - (P@width + expansion)
  Ymax = P@y + (P@width + expansion)
  return( Xmin <= x & x <= Xmax & Ymin <= y & y <= Ymax)
}

P = readPoints("points.dat")
setClass("partition", slots=list(id="numeric", x="numeric", y="numeric", width="numeric"))
P1 <- new("partition", id=1, x=2.5,  y=7.5,  width=2.5)
P2 <- new("partition", id=2, x=7.5,  y=7.5,  width=2.5)
P3 <- new("partition", id=3, x=1.25, y=3.75, width=1.25)
P4 <- new("partition", id=4, x=3.75, y=3.75, width=1.25)
P5 <- new("partition", id=5, x=1.25, y=1.25, width=1.25)
P6 <- new("partition", id=6, x=3.75, y=1.25, width=1.25)
P7 <- new("partition", id=7, x=7.5,  y=2.5,  width=2.5)
partitions = c(P1,P2,P3,P4,P5,P6,P7)
expansion = 1

pointsAndPartitions <- P %>%
  pmap_dfr(function(...) {
    p <- tibble(...)
    p %>% mutate(
      P1 = containsWithExpansion(P1, p$x, p$y),
      P2 = containsWithExpansion(P2, p$x, p$y),
      P3 = containsWithExpansion(P3, p$x, p$y),
      P4 = containsWithExpansion(P4, p$x, p$y),
      P5 = containsWithExpansion(P5, p$x, p$y),
      P6 = containsWithExpansion(P6, p$x, p$y),
      P7 = containsWithExpansion(P7, p$x, p$y),
    ) 
  })
