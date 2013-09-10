Description
-----------
Ports of Xpose4 lattice functions to ggplot2.

Usage
------

The goal is to allow the user to use the same code used for creating plots as for lattice-Xpose, although some additional arguments may sometimes be required:
  
    library(xpose4)
    source("kaplan.plot.R") # update plot definition

    xpose4()  # read in Xpose tables
    kaplan.plot(xpdb)  
    kaplan.plot(xpdb, by=c("ARM==0","ARM==1","ARM==2","ARM==3"), xlim=c(0,120), facet="wrap")  

Status
-------
* ``kaplan.plot()``: basic version implemented, allows stratification. VPC for KM-plot not tested yet.
* ``xpose.VPC()``: basic version implemented, allows stratification.

Planned
--------
* ``xpose.plot.default()``: if implemented, most Xpose plots should be covered
* ``ind.plots()``
* ...


