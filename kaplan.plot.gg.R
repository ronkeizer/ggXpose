kaplan.plot <- function (x = "TIME", y = "DV", id = "ID", data = NULL, evid = "EVID", 
          by = NULL, xlab = "Time", ylab = "Survival (%)", object = NULL, 
          events.to.plot = "All", sim.data = NULL, sim.zip.file = NULL, 
          VPC = FALSE, nsim.lab = "simNumber", sim.evct.lab = "counter", 
          probs = c(0.025, 0.975), add.baseline = T, add.last.area = T, 
          subset = NULL, main = "Default", main.sub = "Default", main.sub.cex = 0.8, 
          nbins = NULL, real.type = "l", real.lty = 1, real.lwd = 1, 
          real.col = "blue", real.se = if (!is.null(sim.data)) F else T, 
          real.se.type = "l", real.se.lty = 2, real.se.lwd = 0.5, real.se.col = "red", 
          cens.type = "l", cens.lty = 1, cens.col = "black", cens.lwd = 1, 
          inclZeroWRES = TRUE, onlyfirst = FALSE, samp = NULL, poly.alpha = 1, 
          poly.fill = "lightgreen", poly.line.col = "darkgreen", poly.lty = 2, 
          censor.lines = TRUE, ylim = c(-5, 105), cov = NULL, cov.fun = "mean", 
          # added for ggplot2
          facet="grid", facet_dir="vertical", 
          ...) 
{
  if (is.null(object) && is.null(data)) 
    cat("one of data or object should be defined in function input.")
  if (!is.null(data)) {
    if (!is.null(subset)) {
      on.exit(detach(data))
      attach(data, warn.conflicts = F)
      data <- data[eval(parse(text = paste("data$", subset))),  ]
      if (dim(data)[1] == 0) 
        return(NULL)
    }
  }
  if (!is.null(object)) {
    if (!is.null(samp)) {
      data <- SData(object, inclZeroWRES, onlyfirst = onlyfirst, subset = subset, samp = samp)
    } else {
      data <- Data(object, inclZeroWRES, onlyfirst = onlyfirst, subset = subset)
    }
  }
  if (VPC && !is.null(object)) 
    sim.zip.file <- paste("simtab", object@Runno, ".zip", 
                          sep = "")
  if (!is.null(sim.zip.file)) {
    sim.data <- read.table(unz(sim.zip.file, sub(".zip", 
                                                 "", sim.zip.file)), skip = 0, header = T)
  }
  if (!is.null(sim.data)) {
    if (!is.null(subset)) {
      on.exit(detach(sim.data))
      attach(sim.data, warn.conflicts = F)
      sim.data <- sim.data[eval(parse(text = paste("sim.data$", 
                                                   subset))), ]
      if (dim(sim.data)[1] == 0) 
        return(NULL)
    }
  }
  data$counter <- 0
  counter <- 0
  old.id <- 0
  for (i in 1:length(data$counter)) {
    new.id <- data[[id]][i]
    if (new.id != old.id) {
      counter <- 0
    }
    old.id <- new.id
    if (!is.null(data[[evid]])) {
      if (data[[evid]][i] == 0) 
        counter = counter + 1
    } else {
      counter = counter + 1
    }
    data$counter[i] = counter
  }
  events <- max(data$counter)
  RTTE = FALSE
  if (events > 1) 
    RTTE = TRUE
  max.events <- events
  if (!is.null(sim.data)) {
    sim.events <- max(sim.data[[sim.evct.lab]])
  }
  full.data <- data
  y.true <- y
  if (!is.null(sim.data)) {
    full.sim.data <- sim.data
    rle.result <- rle(full.sim.data[[id]])
    rle.result$values <- 1:length(rle.result$values)
    new.id <- inverse.rle(rle.result)
    full.sim.data$sim.ID <- new.id
  }
  by.no <- length(by)
  if (!all(is.na(match(events.to.plot, "All")))) {
    num.of.plots <- max.events
    if (by.no > 0) 
      num.of.plots <- num.of.plots * by.no
    plotList <- vector("list", num.of.plots)
    event.list <- 1:max.events
  } else {
    num.of.plots <- length(events.to.plot)
    if (by.no > 0) 
      num.of.plots <- num.of.plots * by.no
    plotList <- vector("list", num.of.plots)
    event.list <- events.to.plot
  }
  plot.num <- 0
  for (event.no in event.list) {
    if (is.null(by)) 
      by = c(1)
    comb.data <- c()
    for (by.val in by) {
#      by.val = 1 # remove!      
      if (by.val == 1) {
        by.val = NULL
        by = NULL
      }
      tmp.cex <- main.sub.cex
      if (is.null(main.sub) | num.of.plots == 1) {
        tmp.name <- NULL
      } else {
        if (!is.na(match(main.sub, "Default"))) {
          tmp.name <- paste("Event", event.no, sep = " ")
          if (!is.null(by.val)) 
            tmp.name <- paste(tmp.name, by.val, sep = ", ")
        }
        else {
          tmp.name <- main.sub[plot.num + 1]
        }
      }
      data <- full.data
      if (!is.null(sim.data)) 
        sim.data <- full.sim.data
      data <- subset(data, counter <= event.no)
      if (!is.null(data[[evid]])) {
        data <- data[data[evid] == 0, ]
      }
      if (!is.null(sim.data)) 
        sim.data <- subset(sim.data, eval(parse(text = sim.evct.lab)) <= event.no)
      data <- data[!duplicated(data[[id]], fromLast = TRUE), ]
      if (!is.null(sim.data)) 
        sim.data <- sim.data[!duplicated(sim.data$sim.ID, fromLast = TRUE), ]
      data$tmp.event <- 0
      data$tmp.event[data[y.true] != 0] = 1
      data$tmp.event[data$counter < event.no] = 0
      y = "tmp.event"
      if (!is.null(sim.data)) {
        sim.data$tmp.event <- 0
        sim.data$tmp.event[sim.data[y.true] != 0] = 1
        sim.data$tmp.event[sim.data[[sim.evct.lab]] < event.no] = 0
      }
      if (!is.null(by.val)) {
        data <- subset(data, eval(parse(text = by.val)))
        if (dim(data)[1] == 0) 
          return(NULL)
        if (!is.null(sim.data)) {
          sim.data <- subset(sim.data, eval(parse(text = by.val)))
          if (dim(sim.data)[1] == 0) 
            return(NULL)
        }
      }
      S <- Surv(data[, x], data[, y])
      f.1 <- survfit(S ~ 1)
      a.1 <- summary(f.1)
      cov.plot <- FALSE
      censored.ids <- FALSE
      if (!is.null(cov)) 
        cov.plot <- TRUE
      if (cov.plot) {
        if (is.factor(data[, cov])) {
          cat("\n  Transforming", cov, "from levels to numeric\n", 
              sep = " ")
          data[, cov] <- as.numeric(levels(data[, cov]))[data[, cov]]
        }
        tmp.y.cov <- c()
        for (i in 1:length(f.1$time)) {
          if (f.1$n.censor[i] > 0 && f.1$n.event[i] == 0) {
            tmp.cov <- data[, cov][data[, x] >= f.1$time[i]]
          }
          else {
            tmp.cov <- data[, cov][data[, x] > f.1$time[i]]
          }
          tmp.y.cov <- c(tmp.y.cov, do.call(cov.fun, list(tmp.cov)))
        }
      }
      if (!is.null(sim.data)) {
        times <- sort(unique(sim.data[, x]))
        m1 <- matrix(nrow = length(unique(sim.data[, nsim.lab])), ncol = length(times))
        if (cov.plot) {
          m2 <- m1
        }
        ii = 0
        for (i in unique(sim.data[, nsim.lab])) {
          ii = ii + 1
          tmp <- sim.data[eval(parse(text = paste("sim.data$", nsim.lab, "==", i))), ]
          S.sim <- Surv(tmp[, x], tmp[, y])
          f.1.sim <- survfit(S.sim ~ 1)
          a.1.sim <- summary(f.1.sim)
          tmp.times <- f.1.sim$time
          col.index <- match(tmp.times, times)
          if (cov.plot) {
            if (is.factor(tmp[, cov])) {
              cat("\n  Transforming", cov, "from levels to numeric\n", 
                  sep = " ")
              tmp[, cov] <- as.numeric(levels(tmp[, cov]))[tmp[, cov]]
            }
            tmp.y.cov.sim.n <- c()
            for (j in 1:length(f.1.sim$time)) {
              if (f.1.sim$n.censor[j] > 0 && f.1.sim$n.event[j] == 0) {
                tmp1.cov <- tmp[, cov][tmp[, x] >= f.1.sim$time[j]]
              }
              else {
                tmp1.cov <- tmp[, cov][tmp[, x] > f.1.sim$time[j]]
              }
              tmp.y.cov.sim.n <- c(tmp.y.cov.sim.n, do.call(cov.fun, list(tmp1.cov)))
            }
          }
          m1[ii, col.index] <- f.1.sim$surv
          if (is.na(m1[ii, 1])) {
            m1[ii, 1] <- 1
          }
          for (j in 2:length(times)) {
            if (is.na(m1[ii, j])) {
              m1[ii, j] <- m1[ii, j - 1]
            }
          }
          if (cov.plot) {
            m2[ii, col.index] <- tmp.y.cov.sim.n
            if (is.na(m2[ii, 1])) {
              m2[ii, 1] <- do.call(cov.fun, list(tmp[, cov]))
            }
            for (j in 2:length(times)) {
              if (is.na(m2[ii, j])) {
                m2[ii, j] <- m2[ii, j - 1]
              }
            }
          }
        }
        if (cov.plot) {
          m1 <- m2
        }
        if (is.null(nbins)) {
          quants <- matrix(nrow = 2, ncol = dim(m1)[2])
          time.bin <- times
          for (j in 1:dim(quants)[2]) {
            quants[, j] <- quantile(m1[, j], probs = probs, na.rm = F)
          }
        }
        else {
          remainder.times <- 0
          ncomb <- floor(dim(m1)[2]/nbins)
          if (ncomb == 0) {
            ncomb <- 1
            quants <- matrix(nrow = 2, ncol = dim(m1)[2])
            time.bin <- c()
          }
          else {
            remainder.times <- dim(m1)[2] - ncomb * nbins
            quants <- matrix(nrow = 2, ncol = nbins)
            time.bin <- c()
          }
          k.add.this <- 0
          for (j in 1:dim(quants)[2]) {
            tmp.bin <- c()
            if (remainder.times > 0) {
              ncomb.tmp <- ncomb + 1
              remainder.times <- remainder.times - 1
            }
            else {
              ncomb.tmp <- ncomb
            }
            for (k in 1:ncomb.tmp) {
              if (k == 1) 
                time.bin <- c(time.bin, times[k + k.add.this])
              if (k + k.add.this > dim(m1)[2]) 
                next
              tmp.bin <- c(tmp.bin, m1[, k + k.add.this])
            }
            k.add.this <- k.add.this + ncomb.tmp
            quants[, j] <- quantile(tmp.bin, probs = probs, na.rm = F)
          }
        }
        if (add.baseline) {
          tmp.mat <- matrix(nrow = dim(quants)[1], ncol = dim(quants)[2] + 1)
          tmp.mat[, 1] <- 1
          if (cov.plot) 
            tmp.mat[, 1] <- do.call(cov.fun, list(sim.data[, cov]))
          tmp.mat[, -1] <- quants
          tmp.x <- c(0, time.bin)
        }
        else {
          tmp.mat <- quants
          tmp.x <- time.bin
        }
        PI.up <- c()
        PI.down <- c()
        PI.times <- c()
        n.times <- length(tmp.x)
        for (i in 1:n.times) {
          PI.reps = 2
          time.reps = 2
          if (i == 1) 
            time.reps = 1
          if (i == n.times) 
            PI.reps = 1
          PI.up <- c(PI.up, rep(tmp.mat[2, i], PI.reps))
          PI.down <- c(PI.down, rep(tmp.mat[1, i], PI.reps))
          PI.times <- c(PI.times, rep(tmp.x[i], time.reps))
        }
        if (add.last.area) {
          if (tail(tail(PI.times, n = 1)) == tail(times, n = 1)) {
            time.pt <- (tail(PI.times, n = 1) - PI.times[1]) * 1.01
          }
          else {
            time.pt <- (tail(times, n = 1) - PI.times[1]) * 1.01
          }
          PI.times = c(PI.times, time.pt)
          PI.up <- c(PI.up, tail(PI.up, n = 1))
          PI.down <- c(PI.down, tail(PI.down, n = 1))
        }
        if (!cov.plot) {
          PI.up <- PI.up * 100
          PI.down <- PI.down * 100
        }
      } else {
        PI.up = NULL
        PI.down = NULL
        PI.times = NULL
      }
      if (add.baseline) {
        tmp.y <- c(1, f.1$surv)
        tmp.x <- c(0, f.1$time)
        tmp.y.upper <- c(1, f.1$upper)
        tmp.y.lower <- c(1, f.1$lower)
        if (cov.plot) 
          tmp.y.cov <- c(do.call(cov.fun, list(data[, cov])), tmp.y.cov)
      } else {
        tmp.y <- c(f.1$surv)
        tmp.x <- c(f.1$time)
        tmp.y.upper <- c(f.1$upper)
        tmp.y.lower <- c(f.1$lower)
      }
      if (cov.plot) {
        tmp.y <- tmp.y.cov
      }
      real.data <- c()
      real.times <- c()
      real.data.upper <- c()
      real.data.lower <- c()
      for (i in 1:length(tmp.y)) {
        y.reps = 2
        x.reps = 2
        if (i == 1) 
          x.reps = 1
        if (i == length(tmp.y)) 
          y.reps = 1
        real.data <- c(real.data, rep(tmp.y[i], y.reps))
        real.times <- c(real.times, rep(tmp.x[i], x.reps))
        real.data.upper <- c(real.data.upper, rep(tmp.y.upper[i], y.reps))
        real.data.lower <- c(real.data.lower, rep(tmp.y.lower[i], y.reps))
      }
      if (!cov.plot) {
        real.data <- real.data * 100
        real.data.upper <- real.data.upper * 100
        real.data.lower <- real.data.lower * 100
      }
      if (cov.plot) {
        if (!is.null(sim.data)) {
          ylim <- c(min(range(real.data)[1], range(PI.down)[1]) * 0.9, max(range(real.data)[2], range(PI.up)[2]) * 1.1)
        } else {
          ylim <- c(min(range(real.data)[1]) * 0.9, max(range(real.data)[2]) * 1.1)
        }
        real.se <- FALSE
        ylab <- paste(cov.fun, cov)
        cen.x0 <- f.1$time[f.1$n.censor > 0]
        cen.x1 <- f.1$time[f.1$n.censor > 0]
        cen.y0 <- tmp.y[match(f.1$time[f.1$n.censor > 0], tmp.x)] - (ylim[2] - ylim[1]) * 0.01
        cen.y1 <- tmp.y[match(f.1$time[f.1$n.censor > 0], tmp.x)] + (ylim[2] - ylim[1]) * 0.01
      }
      as.num <- function(x) { as.numeric(as.character(x)) }
      comb.tmp <- na.omit(cbind("time" = real.times,
                                    "obs" = as.num(real.data), 
                                    "obs.lower" = as.num(real.data.lower), 
                                    "obs.upper" = as.num(real.data.upper),
                                     strata = by.val))
      comb.data <- rbind(comb.data, comb.tmp)  
    } # by list
    comb.data <- data.frame(comb.data)
    for (i in 1:4) {
      comb.data[,i] <- as.num(comb.data[,i])
    }
  } # event.list
  #  return(comb.data)
  ## plotting
  default.plot.title <- "Kaplan-Meier plots"
  kaplan_gg <- ggplot (comb.data, aes(x=time, y=obs)) + 
    geom_ribbon(aes(ymin=obs.lower, ymax=obs.upper), fill=rgb(0.5,0.5,0.5,0.5), colour=NA) +
    geom_step() +
    xlab(xlab) + 
    ylab(ylab)
  if (!is.null(default.plot.title)) {
    kaplan_gg <- kaplan_gg + ggtitle(default.plot.title)
  }
  if (!is.null(by)) {
    if (facet == "grid") {
      if (facet_dir == "vertical") {
        kaplan_gg <- kaplan_gg + facet_grid(strata ~ .)              
      } else {
        kaplan_gg <- kaplan_gg + facet_grid(. ~ strata)      
      }
    } else {
      kaplan_gg <- kaplan_gg + facet_wrap(~strata)      
    }
  }
  return(kaplan_gg)
  
#       xplot <- xyplot(real.data ~ real.times, main = list(tmp.name, 
#                                                           cex = tmp.cex), ylim = ylim, xlab = xlab, ylab = ylab, 
#                       real.type = real.type, PI.up = PI.up, PI.down = PI.down, 
#                       PI.times = PI.times, real.data.upper = real.data.upper, 
#                       real.data.lower = real.data.lower, real.se.type = real.se.type, 
#                       real.se.lty = real.se.lty, real.se.col = real.se.col, 
#                       f.1 = f.1, ..., panel = function(x, y, PI.up, 
#                                                        PI.down, PI.times, real.data.upper, real.data.lower, 
#                                                        real.se.type = real.se.type, real.type = real.type, 
#                                                        real.se.lty = real.se.lty, real.se.col = real.se.col, 
#                                                        f.1 = f.1, ...) {
#                         if (!is.null(sim.data)) {
#                           grid.polygon(c(PI.times, rev(PI.times)), 
#                                        c(PI.up, rev(PI.down)), default.units = "native", 
#                                        gp = gpar(fill = poly.fill, alpha = poly.alpha, 
#                                                  col = poly.line.col, lty = poly.lty))
#                         }
#                         panel.xyplot(x, y, type = real.type, lwd = real.lwd, 
#                                      col = real.col, lty = real.lty, ...)
#                         if (real.se) 
#                           panel.xyplot(x, real.data.upper, type = real.se.type, 
#                                        lty = real.se.lty, col = real.se.col, lwd = real.se.lwd, 
#                                        ...)
#                         if (real.se) 
#                           panel.xyplot(x, real.data.lower, type = real.se.type, 
#                                        lty = real.se.lty, col = real.se.col, lwd = real.se.lwd, 
#                                        ...)
#                         if (censor.lines) {
#                           if (any(f.1$n.censor > 0)) {
#                             if (cov.plot) {
#                               panel.segments(x0 = cen.x0, y0 = cen.y0, 
#                                              x1 = cen.x1, y1 = cen.y1, ...)
#                             }
#                             else {
#                               panel.segments(x0 = c(f.1$time[f.1$n.censor > 
#                                                                0]), y0 = c(f.1$surv[f.1$n.censor > 
#                                                                                       0] * 100 - 2), x1 = c(f.1$time[f.1$n.censor > 
#                                                                                                                        0]), y1 = c(f.1$surv[f.1$n.censor > 
#                                                                                                                                               0] * 100 + 2), type = cens.type, lty = cens.lty, 
#                                              col = cens.col, lwd = cens.lwd, ...)
#                             }
#                           }
#                         }
#                       })
#       plot.num <- plot.num + 1
#       plotList[[plot.num]] <- xplot

  #   default.plot.title <- "Kaplan-Meier plots"
#   if (cov.plot) 
#     default.plot.title <- paste(cov.fun, cov)
#   if (num.of.plots == 1) 
#     default.plot.title <- paste("Kaplan-Meier plot for event", 
#                                 event.list[1])
#   if (num.of.plots == 1 && cov.plot) 
#     default.plot.title <- paste(cov.fun, cov, "for event", 
#                                 event.list[1])
#   if (!is.null(object)) {
#     plotTitle <- xpose.multiple.plot.title(object = object, 
#                                            plot.text = default.plot.title, main = main, ...)
#   }
#   else {
#     if (is.null(main)) {
#       plotTitle <- NULL
#     }
#     else {
#       if (!is.na(match(main, "Default"))) {
#         plotTitle <- default.plot.title
#         if (!is.null(subset)) {
#           plotTitle <- paste(plotTitle, "\n[", subset, 
#                              "]", sep = "")
#         }
#       }
#       else {
#         plotTitle <- main
#       }
#     }
#   }
#  obj <- xpose.multiple.plot(plotList, plotTitle, ...)
 
  #obj <- kaplan_gg
  #return(obj)
}
environment(kaplan.plot) <- as.environment("package:xpose4specific")