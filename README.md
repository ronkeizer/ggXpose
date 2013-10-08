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
* ``xpose.panel.default()``: Panel functionality should be moved to ``xpose.plot.default()``
* ``xpose.plot.histogram()``: Histogram plots
* ``xpose.panel.histogram()``: Panel functionality should be moved to ``xpose.plot.histogram()``
* ``xpose.plot.bw()``: bw plots
* ``xpose.panel.bw()``: Panel functionality should be moved to ``xpose.plot.bw()``
* ``xpose.plot.qq()``: qq plots
* ``xpose.panel.qq()``: Panel functionality should be moved to ``xpose.plot.qq()``
* ``xpose.plot.splom()``: splom plots
* ``xpose.panel.splom()``: Panel functionality should be moved to ``xpose.plot.splom()``
* ``ind.plots()``
* ``xpose.VPC.categorical()``: uses ``xyplot()`` directly
* plots in ``bootgam.plots.R``
* ``data.checkout()``
* ``dOFV.vs.id()``: uses ``xyplot()`` directly
* ``npc.coverage()``: uses ``xyplot()`` directly
* ``xp.akaike.plot()``: uses ``dotplot()`` directly
* ``xp.plot()``: uses lattice plots directly
* Any other GAM plots
* All other plots from xpose4specific will hopefully work unchanged once the above are translated
* ...


