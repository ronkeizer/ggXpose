## for testing purposes, need to be put in global namespace

wres.vs.pred <- function (object, smooth = TRUE, abline = c(0, 0), ...) 
{
  if (is.null(check.vars(c("pred", "wres"), object, silent = FALSE))) {
    return()
  }
  xplot <- xpose.plot.default(xvardef("pred", object), xvardef("wres", 
                                                               object), object, smooth = smooth, abline = abline, ...)
  return(xplot)
}
wres.vs.idv <- function (object, abline = c(0, 0), smooth = TRUE, ...) 
{
  if (is.null(check.vars(c("idv", "wres"), object, silent = FALSE))) {
    return()
  }
  xplot <- xpose.plot.default(xvardef("idv", object), xvardef("wres", 
                                                              object), smooth = smooth, abline = abline, object, ...)
  return(xplot)
}
dv.vs.pred <- function (object, abline = c(0, 1), smooth = TRUE, ...) 
{
  if (is.null(check.vars(c("dv", "pred"), object, silent = FALSE))) {
    return()
  }
  xplot <- xpose.plot.default(xvardef("pred", object), xvardef("dv", 
                                                               object), abline = abline, smooth = smooth, object, ...)
  return(xplot)
}