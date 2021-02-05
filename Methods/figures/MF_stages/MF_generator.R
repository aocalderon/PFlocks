library(tidyverse)
library(sqldf)
library(plotrix)

readPoints <- function(filename){
  coords = strsplit(readLines(filename), ", ")[[1]]
  data = enframe(coords) %>% separate(value, into = c("x", "y"), sep = "/") %>% 
    mutate(x = as.numeric(x), y = as.numeric(y)) %>%
    select(x, y)
  return(data)
}

n = 100
epsilon = 1
mu = 4
precision = 0.001
size = 10
lim=c(0-epsilon/2, size+epsilon/2)
r2 = (epsilon/2)^2
P = readPoints("points.dat")
#x = runif(n, 0, size)
#y = runif(n, 0, size)
x = P$x
y = P$y
pointset=data.frame(x = x, y = y)
pointset2 = data.frame(id=1:n, x=x, y=y, t=rep(0,n))
write.table(pointset2, "sample.tsv", sep="\t", row.names = F, col.names = F)
data = sqldf("SELECT p1.x AS x1, p1.y AS y1, p2.x AS x2, p2.y AS y2 FROM pointset p1 CROSS JOIN pointset p2 ")
dim = 10
margin = 0

pdf("pflock.pdf", width = dim, height = dim, onefile = T)
par(mar=c(margin,margin,margin,margin))

distance <- function(x1, y1, x2, y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}

computeCenters <- function(x1, y1, x2, y2){
  x = x1 - x2
  y = y1 - y2
  d2 = x^2 + y^2
  if(d2 != 0){
    root = sqrt(abs(4 * (r2 / d2) - 1))
    h1 = ((x + y * root) / 2) + x2
    h2 = ((x - y * root) / 2) + x2
    k1 = ((y - x * root) / 2) + y2
    k2 = ((y + x * root) / 2) + y2
    return(c(h1, k1, h2, k2))
  }
  return(NULL)
}

drawCanvas <- function(){
  plot(1, asp=1, axes = F, , xlab = "", ylab = "", type='n', xlim=lim, ylim=lim)
}

drawPoints <-function(){
  points(x, y, pch=21, col=1, bg=1, cex=1)
}

drawBorder <- function(x1, y1, x2, y2){
  rect(x1, y1, x2, y2, border = "lightgrey", lty=5, lwd=1)
}

drawPartitions <- function(cells){
  width = size / cells
  n = cells - 1
  for(i in 0:n){
    for(j in 0:n){
      x = i * width
      y = j * width
      drawBorder(x, y, x + width - precision, y + width - precision)
    }
  }
}

drawPairs <- function(){
  pairs = data.frame(x1=double(), y1=double(), x2=double(), y2=double())
  for(i in 1:nrow(data)){
    x1 = data[i,1]
    y1 = data[i,2]
    x2 = data[i,3]
    y2 = data[i,4]
    d = distance(x1,y1,x2,y2)
    if(d <= epsilon && d != 0){
      pairs[nrow(pairs) + 1, ] = list(x1, y1, x2, y2)
      lines(c(x1,x2), c(y1,y2), lwd=2)
    }
  }
  return(pairs)
}

drawPairs2 <- function(pairs){
  for(i in 1:nrow(pairs)){
    x1 = pairs[i,1]
    y1 = pairs[i,2]
    x2 = pairs[i,3]
    y2 = pairs[i,4]
    lines(c(x1,x2), c(y1,y2), lwd=2)
  }
}

drawCenters <- function(){
  centers0 = data.frame(x=double(), y=double())
  for(i in 1:nrow(data)){
    x1 = data[i,1]
    y1 = data[i,2]
    x2 = data[i,3]
    y2 = data[i,4]
    d = distance(x1,y1,x2,y2)
    if(d <= epsilon && d != 0){
      centers = computeCenters(x1,y1,x2,y2)
      centers0[nrow(centers0) + 1, ] = list(centers[1], centers[2])
      centers0[nrow(centers0) + 1, ] = list(centers[3], centers[4])
      points(centers[1], centers[2], pch=21, col=2, bg=2, cex=0.5)
      points(centers[3], centers[4], pch=21, col=2, bg=2, cex=0.5)
    }
  }
  return(centers0)
}

drawCenters2 <- function(centers){
  data = sqldf("SELECT p1.x AS x1, p1.y AS y1, p2.x AS x2, p2.y AS y2 FROM centers p1 CROSS JOIN pointset p2 ")
  disks0 = data.frame(center=character(), point=character(), stringsAsFactors=FALSE)
  for(i in 1:nrow(data)){
    x1 = data[i,1]
    y1 = data[i,2]
    x2 = data[i,3]
    y2 = data[i,4]
    d = distance(x1,y1,x2,y2)
    if(d <= epsilon/2 && d != 0){
      disks0[nrow(disks0) + 1, ] = list(paste(x1, y1), paste(x2, y2))
    }
  }
  disks = as_tibble(disks0) %>% group_by(center) %>% tally() %>% filter(n >= mu) %>% 
    separate(center, c("x", "y"), sep=" ") %>% mutate(x=as.numeric(x), y=as.numeric(y))
  disks = as.data.frame(disks)
  return(disks)
}

drawDisks <- function(disks){
  for(i in 1:nrow(disks)){
    x = disks[i,1]
    y = disks[i,2]
    draw.circle(x, y, epsilon/2, nv = 25, border = 1, col = NA, lty = 2, lwd = 0.5)
  }
}

# Set of points...
drawCanvas()
drawBorder(0,0,size,size)
drawPoints()

# Partitioning points...
drawCanvas()
drawBorder(0,0,size,size)
drawPoints()
drawPartitions(2)

# Finding pairs...
drawCanvas()
drawBorder(0,0,size,size)
drawPoints()
drawPartitions(2)
pairs = drawPairs()

# Computing centers...
drawCanvas()
drawBorder(0,0,size,size)
drawPoints()
drawPartitions(2)
drawPairs2(pairs)
centers = drawCenters()

# Finding disks...
drawCanvas()
drawBorder(0,0,size,size)
drawPoints()
drawPartitions(2)
disks = drawCenters2(centers)
drawDisks(disks)

dev.off()
