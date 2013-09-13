xpose.plot.default <- function (
          x, y, 
          object, 
          inclZeroWRES = FALSE, 
          onlyfirst = FALSE, 
          samp = NULL, 
          panel = xpose.panel.default, 
          groups = object@Prefs@Xvardef$id, 
          ids = object@Prefs@Graph.prefs$ids, 
          logy = FALSE, 
          logx = FALSE, 
          yscale.components = "default", 
          xscale.components = "default", 
          aspect = object@Prefs@Graph.prefs$aspect, 
          funx = NULL, 
          funy = NULL, 
          iplot = NULL,
          PI = NULL, 
          by = object@Prefs@Graph.prefs$condvar, 
          force.by.factor = FALSE, 
          ordby = object@Prefs@Graph.prefs$ordby, 
          byordfun = object@Prefs@Graph.prefs$byordfun, 
          shingnum = object@Prefs@Graph.prefs$shingnum, 
          shingol = object@Prefs@Graph.prefs$shingol, 
          by.interval = NULL, 
          strip = function(...) { strip.default(..., strip.names = c(TRUE, TRUE)) }, 
          use.xpose.factor.strip.names = TRUE, 
          subset = xsubset(object), 
          autocorr = FALSE, 
          main = xpose.create.title(x, y, object, subset, funx, funy, ...), 
          xlb = xpose.create.label(x, object, funx, logx, autocorr.x = autocorr, ...), 
          ylb = xpose.create.label(y, object, funy, logy, autocorr.y = autocorr, ...), 
          scales = list(), 
          suline = object@Prefs@Graph.prefs$suline, 
          bwhoriz = object@Prefs@Graph.prefs$bwhoriz, 
          dilution = FALSE, 
          dilfrac = object@Prefs@Graph.prefs$dilfrac, 
          diltype = object@Prefs@Graph.prefs$diltype, 
          dilci = object@Prefs@Graph.prefs$dilci, 
          seed = NULL,
          mirror = FALSE, 
          max.plots.per.page = 4, 
          mirror.aspect = "fill", 
          pass.plot.list = FALSE, 
          x.cex = NULL, 
          y.cex = NULL, 
          main.cex = NULL, 
          mirror.internal = list(strip.missing = missing(strip)), 
          # ggplot2 specific
          facet = "wrap",
          ...) 
{
  if (!(class(use.xpose.factor.strip.names) == "logical" & 
          length(use.xpose.factor.strip.names) == 1)) {
    stop("The provided use.xpose.factor.strip.names argument is not a logical of length 1")
  }
  plotTitle <- main
  arg.list <- formals(xpose.plot.default)
  arg.names <- names(arg.list)
  new.arg.list <- vector("list", length(arg.names))
  names(new.arg.list) <- arg.names
  for (argnam in arg.names) {
    if (argnam == "...") {
      next
    }
    tmp <- get(argnam)
    if (is.null(tmp)) {
    }
    else {
      new.arg.list[[argnam]] = tmp
    }
  }
  if (mirror) {
    if (is.null(object@Nsim)) {
      cat(paste("The current Xpose database does not have any simulation data.\n"))
      cat(paste("The mirror option cannot be used.\n"))
      return(NULL)
    }
    create.mirror(xpose.plot.default, new.arg.list, mirror, 
                  plotTitle, ...)
  }
  else {
    if (any(is.null(iplot))) {
      if (!is.null(samp)) {
        data <- SData(object, inclZeroWRES, onlyfirst = onlyfirst, subset = subset, samp = samp)
      }
      else {
        data <- Data(object, inclZeroWRES, onlyfirst = onlyfirst, subset = subset)
      }
    }
    else {
      data <- Data(object, inclZeroWRES, onlyfirst = onlyfirst, subset = NULL)
    }
    data <- subset(data, get(x) != object@Prefs@Miss)
    data <- subset(data, get(y) != object@Prefs@Miss)
    if (any(is.null(data))) 
      return("The subset expression is invalid.")
    if (!is.null(by) && force.by.factor) {
      for (b in by) {
        data[, b] <- as.factor(data[, b])
      }
    }
    dilsubset <- TRUE
    dilname <- NULL
    if (dilution) {
      if (is.null(diltype)) {
        data <- create.rand(data, object, dilfrac, seed = seed)
        if (is.null(seed)) {
          dilsubset <- parse(text = "Rnoseed==0")
          dilname <- "Rnoseed"
        }
        else {
          dilsubset <- parse(text = paste("R", seed, "==0", sep = ""))
          dilname <- paste("R", seed, "==0", sep = "")
        }
      }
      else {
        data <- create.strat.rand(data, object, x, y, dilfrac, dilci, seed = seed)
        if (is.null(seed)) {
          dilsubset <- parse(text = "RSnoseed==0")
          dilname <- "RSnoseed"
        }
        else {
          dilsubset <- parse(text = paste("RS", seed, , "==0", sep = ""))
          dilname <- paste("RS", seed, , "==0", sep = "")
        }
      }
    }
    if (length(x) > 1 && length(y) > 1) {
      cat("x and y can not both be longer than 1\n")
      return()
    }
    if (length(x) > 1) {
      reps <- c(xvardef("id", object), xvardef("idlab", object), xvardef("wres", object), y, groups)
      if (!is.null(dilname)) 
        reps <- c(reps, dilname)
      if (!is.null(by)) 
        reps <- c(reps, by)
      data <- xpose.stack(data, object, x, reps)
      object <- new("xpose.data", Runno = object@Runno, Data = NULL, Prefs = object@Prefs)
      Data(object) <- data
      if (is.null(main.cex)) 
        main.cex <- 0.9
      onlyfirst = FALSE
      if (is.null(by)) {
        by <- "ind"
      }
      else {
        by <- c("ind", by)
      }
      x <- "values"
      if (length(scales) == 0) {
        scales = list(x = list(relation = "free"))
      }
    }
    if (length(y) > 1) {
      reps <- c(object@Prefs@Xvardef["id"], object@Prefs@Xvardef["idlab"], xvardef("wres", object), x, groups)
      if (!is.null(dilname)) 
        reps <- c(reps, dilname)
      if (!is.null(by)) 
        reps <- c(reps, by)
      data <- xpose.stack(data, object, y, reps)
      object <- new("xpose.data", Runno = object@Runno, Data = NULL, Prefs = object@Prefs)
      Data(object) <- data
      if (is.null(main.cex)) 
        main.cex <- 0.9
      onlyfirst = FALSE
      if (is.null(by)) {
        by <- "ind"
      } else {
        by <- c("ind", by)
      }
      y <- "values"
      if (length(scales) == 0) {
        scales = list(y = list(relation = "free"))
      }
    }
    bb <- NULL
    groups <- groups
    if (any(is.null(by))) {
      if (bwhoriz) {
        formel <- paste(x, "~", y, sep = "")
      }
      else {
        formel <- paste(y, "~", x, sep = "")
      }
    }
    else {
      for (b in by) {
        bb <- c(bb, xlabel(b, object))
        if (!is.factor(data[, b])) {
          if (is.null(by.interval)) {
            data[, b] <- equal.count(data[, b], number = shingnum, overl = shingol)
          }
          else {
            data[, b] <- shingle(data[, b], intervals = by.interval)
          }
        }
        else {
          if (any(!is.null(ordby))) {
            data[, b] <- reorder(data[, b], data[, ordby], byordfun)
          }
          if (names(data[, b, drop = F]) != "ind") {
            if (use.xpose.factor.strip.names) {
              levels(data[, b]) <- paste(xlabel(names(data[, b, drop = F]), object), ":", levels(data[, b]), sep = "")
            }
          }
        }
      }
      bys <- paste(by, collapse = "*")
      if (bwhoriz) {
        formel <- paste(x, "~", y, "|", bys, sep = "")
      }
      else {
        formel <- paste(y, "~", x, "|", bys, sep = "")
      }
    }
    if (missing(strip)) {
      strip <- function(var.name, ...) strip.default(var.name = bb, strip.names = c(F, T), ...)
    }
    if (any(!is.null(groups))) 
      groups <- data[, groups]
    if (!is.null(suline)) {
      suline <- data[, suline]
    }
    if (ids) {
      ids <- data[, xvardef("idlab", object)]
    }
    else {
      ids <- NULL
    }
    if (!is.null(funx)) {
      data[, x] <- do.call(funx, list(data[, x]))
    }
    if (!is.null(funy)) {
      data[, y] <- do.call(funy, list(data[, y]))
    }
    yscale.components.defined <- T
    xscale.components.defined <- T
    if (!is.function(yscale.components)) {
      if (!is.na(match(yscale.components, "default"))) {
        yscale.components = function(...) yscale.components.default(...)
        yscale.components.defined <- F
      }
    }
    if (!is.function(xscale.components)) {
      if (!is.na(match(xscale.components, "default"))) {
        xscale.components = function(...) xscale.components.default(...)
        xscale.components.defined <- F
      }
    }
    if (logy) {
      scales$y$log <- TRUE
      if (!yscale.components.defined) {
        yscale.components = xpose.yscale.components.log10
      }
    }
    if (logx) {
      scales$x$log <- TRUE
      if (!xscale.components.defined) {
        xscale.components = xpose.xscale.components.log10
      }
    }
    xvarnam <- x
    yvarnam <- y
    if (!is.null(x.cex)) {
      if (is.list(xlb)) {
        xlb$cex = x.cex
      } else {
        xlb <- list(xlb, cex = x.cex)
      }
    }
    if (!is.null(y.cex)) {
      if (is.list(ylb)) {
        ylb$cex = y.cex
      } else {
        ylb <- list(ylb, cex = y.cex)
      }
    }
    if (is.null(main)) {} else {
      if (!is.null(main.cex)) {
        if (is.list(main)) {
          main$cex = main.cex
        } else {
          main <- list(main, cex = main.cex)
        }
      }
    }
    if (autocorr) {
      auto.ids <- unique(data[[xvardef("id", object)]])
      auto.n <- 0
      xplt1 <- 0
      xplt2 <- 0
      xgrps <- 0
      for (i in 1:length(auto.ids)) {
        i <- 1
        seli <- data[[xvardef("id", object)]] == ids[i]
        nobs <- length(data[[x]][seli])
        xplt <- matrix(data[[x]][seli], 1, nobs)
        if (nobs > 1) {
          for (j in 1:(nobs - 1)) {
            auto.n <- auto.n + 1
            xplt1[auto.n] <- xplt[1, j]
            xplt2[auto.n] <- xplt[1, j + 1]
            xgrps[auto.n] <- auto.ids[i]
          }
        }
      }
    }
    
    ## plotting (ggplot)
    xplot <- ggplot2::ggplot(data, ggplot2::aes_string(x=x, y=y)) + 
      ggplot2::geom_point()  
    if (!is.null(by)) {
      if (length(by) == 1) {
        if (facet == "wrap") {
          facets <- facet_grid(paste(". ~", by))             
        } else {
          facets <- facet_grid(paste(". ~", by))             
        }
      } 
      if (length(by) > 2) {
        cat ("Warning: only 2 facets allowed in this implementation of xpose.plot.default, using first two.\n")
        by <- by[1:2]
      }
      if (length(by) == 2) {
        facets <- facet_grid(paste(by[1], "~", by[2])) 
      }
      xplot <- xplot + facets
    }
    
#     xplot <- xyplot(formula(formel), data, obj = object, 
#                     prepanel = function(x, y) {
#                       if (is.factor(x)) {
#                         if (length(grep("[A-Z,a-z]", levels(x))) == 
#                               0) {
#                           xlim <- as.character(sort(as.numeric(levels(x))))
#                         }
#                         else {
#                           xlim <- sort(levels(x))
#                         }
#                       }
#                       else {
#                         xlim <- range(x)
#                       }
#                       if (is.factor(y)) {
#                         if (length(grep("[A-Z,a-z]", levels(y))) == 
#                               0) {
#                           ylim <- as.character(sort(as.numeric(levels(y))))
#                         }
#                         else {
#                           ylim <- sort(levels(y))
#                         }
#                       }
#                       else {
#                         ylim <- range(y)
#                       }
#                       list(xlim = xlim, ylim = ylim)
#                     }, onlyfirst = onlyfirst, samp = samp, panel = panel, 
#                     strip = strip, groups = groups, inclZeroWRES = inclZeroWRES, 
#                     PI = PI, logy = logy, logx = logx, xscale.components = xscale.components, 
#                     yscale.components = yscale.components, xvarnam = xvarnam, 
#                     yvarnam = yvarnam, ids = ids, main = main, xlab = xlb, 
#                     ylab = ylb, aspect = aspect, suline = suline, bwhoriz = bwhoriz, 
#                     subset = eval(dilsubset), scales = scales, iplot = iplot, 
#                     autocorr = autocorr, PI.subset = subset, ...)
    return(xplot)
  }
}

#environment(xpose.plot.default) <- as.environment("package:xpose4generic")
