library(xpose4)
library(ggplot2)

## load example
data(simpraz.xpdb)
xpdb <- simpraz.xpdb
xpdb@Prefs@Graph.prefs$aspect <-  "fill"

## coder settings
andy <- FALSE # change
if (andy) {
  cur.dir <- getwd()
  setwd("/Users/ahooker/Documents/Xpose/Examples/other_examples/Andy_Simpraz")
  xpdb <- xpose.data(5)
  setwd(cur.dir)
} else {
  setwd("~/git/ggXpose")
}

## Try lattice basic plot
xpose.plot.default("TIME","DV",xpdb) # lattice version

## ggplot2 basic plot
source("xpose.plot.default.R") 
xpose.plot.default("TIME","DV",xpdb) # new ggplot version
rm("xpose.plot.default")
xpose.plot.default("TIME","DV",xpdb) # lattice version again

## standard xpose plots (lattice)
source("xpose.plot.default.R") 
wres.vs.idv(xpdb) # standard xpose version
dv.vs.pred(xpdb)

## standard xpose plots (ggplot2)
environment(wres.vs.idv) <- .GlobalEnv
environment(dv.vs.pred) <- .GlobalEnv
wres.vs.idv(xpdb) # ggplot version
dv.vs.pred(xpdb)

## Feature: adding geoms [works]
tmp <- wres.vs.idv(xpdb)
tmp + geom_line(aes(group=ID))

## Feature: stratify using facet_wrap/grid 
## status: mostly working, but all .by.cov functions need to be changed.
rm("xpose.plot.default")
change.xvardef(xpdb, "covariates") <- c("SEX", "RACE")
dv.vs.pred.by.cov(xpdb, grid = TRUE, ids = TRUE) # lattice version
source("xpose.plot.default.R") 
environment(dv.vs.pred.by.cov) <- .GlobalEnv
dv.vs.pred.by.cov(xpdb, grid = TRUE, ids = TRUE) # ggplot2 version

# 1 var, facet_grid:
xpose.plot.default(xvardef("pred", xpdb), xvardef("dv", xpdb), object=xpdb, by=c("SEX"), facet="grid") 
# 1 var, facet_wrap: [not working yet, using facet_grid in code]
xpose.plot.default(xvardef("pred", xpdb), xvardef("dv", xpdb), object=xpdb, by=c("SEX"), facet="wrap") 
# 2 vars:
xpose.plot.default(xvardef("pred", xpdb), xvardef("dv", xpdb), object=xpdb, by=c("SEX","SMOK")) 
# higher-level function:
source("dv.vs.pred.by.cov.gg.R") 
dv.vs.pred.by.cov(xpdb, grid=TRUE, ids=TRUE)

## Feature: coloring of lines

## Feature: adding ablines / smooths

## Feature: ...


