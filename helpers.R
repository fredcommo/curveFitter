
############################
# HELPERS
############################
.getData <- function(filepath, h){
  if (is.null(filepath))
    return(NULL)

  else{
    dat <- try(read.delim(filepath, header=h, sep="\t"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=","))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=";"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
        dat <- try(read.delim(filepath, header=h, sep=" "))
    if(inherits(dat, "try-error") || ncol(dat)<3){
          cat("format not supported.\n")
          return(NULL)
        }

    # Check whether ',' is used as decimal sep
    if(any(grepl(",", dat[,2])))
    	dat[,2] <- as.numeric(as.character(gsub(",", ".", dat[,2])))
    if(any(grepl(",", dat[,3])))
    	dat[,3] <- as.numeric(as.character(gsub(",", ".", dat[,3])))

    dat <- dat[dat[,2]!=0,]
    return(split(dat, dat[,1]))
  }
}

############################
# HELPERS PLOT
############################
# .plot <- function(object, showAsLog,...){
#     x <- getX(object)
#     y <- getY(object)
# 
#     plot(x, y, type = "n", bty = "n", axes = FALSE, cex.axis = .85,
#         ylim = range(min(c(y, 0), na.rm = TRUE), max(c(y, 1), na.rm = TRUE)+.1), ...)
#     .addXaxis(x, showAsLog)
#     .addYaxis(y)
#     
#     if(min(y, na.rm = TRUE) < 0)
#         abline(h = 0, lty = 2)
#     if(max(y, na.rm = TRUE) > 1)
#         abline(h = 1, lty = 2)
# }
.addPolygon <- function(object){
    newx <- getXcurve(object)
    newy <- getYcurve(object)
    bounds <- nplr:::.confInt(getStdErr(object), getY(object), getFitValues(object), newy)
    xx <- c(newx, rev(newx))
    yy <- c(bounds$lo, rev(bounds$hi))
    polygon(xx, yy, border = NA, col = rgb(.8,.8,.8,.4))
}
# .addEstim <- function(object, showEstim, unit, B, conf.level){
#     stdErr <- getStdErr(object)
#     estim <- nplr:::.estimateRange(showEstim, stdErr, getPar(object)$params, B, object@useLog, conf.level)
#     newx <- getXcurve(object)
#     newy <- getYcurve(object)
#     legend1 <- sprintf("IC%d : %s%s", showEstim*100, format(estim[2], scientific=TRUE, digits=2), unit)
#     legend2 <- sprintf("[%s, %s]", format(estim[1], scientific=TRUE, digits=2), format(estim[3], scientific=TRUE, digits=2))
#     legend(ifelse(newy[length(newy)]<newy[1], 'bottomleft', 'topleft'),
#            legend = c(legend1, legend2), cex = 1, text.col = 'steelblue4', bty = 'n')
    
# }
# .addGOF <- function(object){
#     gof <- format(getGoodness(object), digits=3, scientific = TRUE)
#     newx <- getXcurve(object)
#     newy <- getYcurve(object)
#     legend(ifelse(newy[length(newy)]<newy[1], 'topright', 'bottomright'),
#            legend = paste('Goodness of fit:', gof), bty = 'n', cex = 1)
# }
.addPoints <- function(object, pcol, pSize, ...){
    x <- getX(object)
    y <- getY(object)
    points(x, y, col = pcol, pch = 19, cex = 3*pSize)
#    points(x, y, pch = 1)
}
.addCurve <- function(object, lcol, lWidth, ...){
    x <- getXcurve(object)
    y <- getYcurve(object)
    lines(y ~ x, col=lcol, lwd=6*lWidth, ...)
}
.SE <- function(x, y){
    .len <- function(x){ sum(!is.na(x)) }
    n <- by(y, x, .len)
    er <- by(y, x, sd, na.rm = TRUE)
    sEr <- as.vector(er/sqrt(n))
    sEr    
}
.addMeans <- function(object, pSize, ...){
    x <- getX(object)
    y <- getY(object)
    my <- as.vector(by(y, x, mean, na.rm = TRUE))
    points(unique(x), my, pch = 19, cex = 3*pSize, ...)
}
.addErr <- function(object, pSize, ...){
    x <- getX(object)
    y <- getY(object)
    my <- as.vector(by(y, x, mean, na.rm = TRUE))
    sEr <- .SE(x, y)
    e <- diff(range(x, na.rm = TRUE))/60
    segments(x0 = unique(x), y0 = my - sEr, y1 = my + sEr, lwd = 5*pSize, ...)
    segments(x0 = unique(x) - e, x1 = unique(x) + e, y0 = my - sEr, lwd = 5*pSize, ...)
    segments(x0 = unique(x) - e, x1 = unique(x) + e, y0 = my + sEr, lwd = 5*pSize, ...)
}
.addXaxis <- function(x, showAsLog){
    x <- unique(x)
    x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), len = length(x))
    if(!showAsLog){
        l <- 10^seq(-20, 20)
        axis(1, at = log10(l), labels = format(l, digits = 1, scientific = TRUE), cex.axis = .85)
    } else{
        axis(1, at = x, labels = format(x, digits = 2, scientific = FALSE), cex.axis = .85)
    }
}
.addYaxis <- function(y){
    y <- seq(-1, 2, by = .25)
    axis(2, at = y, labels = format(y, digits = 2), cex.axis = .85, las = 1)
}

.multiCurve <- function(models, showPoints = FALSE, showMeans = FALSE, showSDerr = TRUE, pSize = 1, lWidth=1, legendSize=1, showAsLog = TRUE, Legend = TRUE, Cols = NULL,...){


    showAsLog <- ifelse(showAsLog == "TRUE", TRUE, FALSE)
    K <- length(models)
        
    allX <- do.call(c, lapply(models, function(tmp) getX(tmp) ))
    allY <- do.call(c, lapply(models, function(tmp) getY(tmp) ))
    plot(range(allX, na.rm = TRUE), range(min(allY, na.rm = TRUE), max(allY, na.rm = TRUE)+.3),
         type = "n", bty = "n", axes = FALSE, cex.axis = .95,
         ylim = range(min(min(allY, na.rm = TRUE), 0), max(max(allY, na.rm = TRUE), 1.25)+.2), ...)

    .addXaxis(allX, showAsLog)
    .addYaxis(allY)

    if(is.null(Cols))
        Cols <- rep("black", K)

    for(k in seq_len(K)){
        tmp <- models[[k]]
        Col <- Cols[k]
        if(showPoints) .addPoints(tmp, pSize = pSize, pcol = Col, ...)
        if(showMeans) .addMeans(tmp, pSize = pSize, col = Col, ...)
        if(showSDerr) .addErr(tmp, pSize, col = Col, ...)
        .addCurve(tmp, Col, lWidth)
    }

    if(min(allY, na.rm = TRUE) < 0)
        abline(h = 0, lty = 2)
    if(max(allY, na.rm = TRUE) > 1)
        abline(h = 1, lty = 2)
    
    if(Legend){
        nm <- length(names(models))
        nc <- sum(nchar(names(models)))
#        Cex <- 1 - min(nc/90, .7)
        nc <- nc + 10*nm
        Cex <- 1.5*legendSize
        if(nc > 100){
            K <- ceiling(K/2)            
        }
        legend("top", legend = names(models), ncol = K,
            col = Cols, bty = "n", cex = Cex, lwd = 2, pch = 19)
    }
}
############################
# HELPERS color picker
############################
.renderDiv <- function(items){
    out <- c()
#     for(item in items){
#         txt <- sprintf("div(class = 'col-sm-12', checkboxInput('', '%s', FALSE))", item)
#         out <- c(out, txt)
#     }
    N <- length(items)
    for(ii in seq_len(N)){
        item <- items[ii]
        col_i <- sprintf('col_%s', ii)
        txt <- sprintf(
            "div(class = 'col-md-12', style='display: inline',
                div(class = 'col-xs-7 cellName', '%s'),
                div(class = 'col-xs-5 colPicker', colourInput('%s', '', '#888'))
            )",
            item, col_i)
        out <- c(out, txt)
    }
    out
}

############################
# HELPERS SUMMARY TABLE
############################
buildSummary <- function(models){
    pars <- lapply(models, function(model){
            p <- getPar(model)
            p <- unlist(p)
            p[-1] <- format(p[-1], digits = 3, scientific = TRUE)
            p
        })
    gof <- lapply(models, function(model) format(getGoodness(model), digits = 3, scientific = TRUE) )
    auc <- lapply(models, function(model) signif(getAUC(model), 3) )
    inflpt <- lapply(models, function(model){
            infl <- as.numeric(getInflexion(model))
            cbind.data.frame(
                xInfl = format(infl[1], digits = 3),
                yInfl = format(infl[2], digits = 3)
                )
        })
    ic50 <- lapply(models, function(model){
            estim <- getEstimates(model, .5)
            interv <- sprintf("[%s | %s]",
                format(estim$x.025, digits=3, scientific = TRUE),
                format(estim$x.975, digits=3, scientific = TRUE)
                )
            cbind(resp = format(estim$y, digits = 2),
                IC = format(estim$x, digits = 3, scientific = TRUE),
                "[95%]" = interv)
        })
    nplrv <- as.character(packageVersion("nplr"))
    nplrDate <- as.character(packageDescription("nplr")["Date"])
    rv <- as.character(version["version.string"])

    out <- cbind.data.frame(cellLine = names(models),
              do.call(rbind, pars),
              GOF = do.call(c, gof),
              do.call(rbind, auc),
              do.call(rbind, inflpt),
              do.call(rbind, ic50),
              "date (Y-m-d)" = format(Sys.Date(), "%Y-%m-%d"),
              "nplr version" = sprintf("%s (%s)",nplrv, nplrDate),
              "R version" = gsub("R version ", "", rv)
              )
    rownames(out) <- sprintf("model-%s", seq_len(length(models)))
    out <- as.data.frame(t(out))
    out
}