library(xpose4)
library(ggplot2)

data(simpraz.xpdb)
xpdb <- simpraz.xpdb
xpdb@Prefs@Graph.prefs$aspect <-  "fill"

## only works for andy
#cur.dir <- getwd()
#setwd("/Users/ahooker/Documents/Xpose/Examples/other_examples/Andy_Simpraz")
#xpdb <- xpose.data(5)
#setwd(cur.dir)

xpose.plot.default("TIME","DV",xpdb) # lattice version

source("xpose.plot.default.R") 
xpose.plot.default("TIME","DV",xpdb) # new ggplot version

rm("xpose.plot.default")
xpose.plot.default("TIME","DV",xpdb) # lattice version again

source("xpose.plot.default.R") 
wres.vs.idv(xpdb) # standard xpose version

environment(wres.vs.idv) <- .GlobalEnv
wres.vs.idv(xpdb) # ggplot version
tmp <- wres.vs.idv(xpdb)
tmp+geom_line(aes(group=ID))

rm("wres.vs.idv")
wres.vs.idv(xpdb) # xpose lattice version

