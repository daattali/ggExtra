MarginalPlot <- R6::R6Class("MarginalPlot",
                            
  public = list(
    
    marg = NULL,
    type = NULL, 
    scatPbuilt = NULL,
    prmL = NULL,
    groupColour = NULL,
    groupFill = NULL,
    
    initialize = function(marg, type, scatPbuilt, prmL, groupColour, 
                          groupFill) {
      self$marg <- marg
      self$type <- type
      self$scatPbuilt <- scatPbuilt
      self$prmL <- prmL
      self$groupColour <- groupColour
      self$groupFill <- groupFill
    },
    
    build = function() {
      data <- private$getVarDf()
      noGeomPlot <- private$margPlotNoGeom(data)
      finalParms <- private$alterParams()
      rawMarg <- private$addLayers(noGeomPlot, finalParms)
      margThemed <- rawMarg + ggplot2::theme_void()
      private$addLimits(margThemed)
    }
  ),
  
  private = list(
    
    getVarDf = function() {
      marg <- self$marg
      if (private$wasFlipped()) {
        marg <- switch(marg,
                       "x" = "y",
                       "y" = "x"
        )
      }
      # Get data frame with geom_point layer data
      scatDF <- private$getGeomPointDf()
      
      # When points are excluded from the scatter plot via a limit on the x
      # axis, the y values in the built scatter plot's `data`` object will be NA 
      # (and visa-versa for the y axis/x values). Exclude these NA points from
      # the data, as they don't actually show up in the scatter plot (and thus 
      # shouldn't be in the marginal plots either).
      scatDF <- scatDF[!(is.na(scatDF$x) | is.na(scatDF$y)), ]
      
      if (marg == "y") {
        scatDF$x <- scatDF$y
      }
      
      scatDF$y <- scatDF$x
      scatDF[, c("x", "y", "fill", "colour", "group")]
    },
    
    wasFlipped = function() {
      classCoord <- class(self$scatPbuilt$plot$coordinates)
      any(grepl("flip", classCoord, ignore.case = TRUE))
    },
    
    getGeomPointDf = function() {
      layerBool <- vapply(
        self$scatPbuilt$plot$layers, 
        function(x) grepl("geom_?point", class(x$geom)[1], ignore.case = TRUE),
        logical(1)
      )
      if (!any(layerBool)) {
        stop("No geom_point layer was found in your scatter plot", call. = FALSE)
      }
      self$scatPbuilt[["data"]][layerBool][[1]]
    },
    
    margPlotNoGeom = function(data) {
      mapping <- "x"
      haveMargMap <- self$groupColour || self$groupFill
      
      if (haveMargMap) {
        # Make sure user hasn't mapped a non-factor
        if (data[["group"]][1] < 0) {
          stop(
            "Colour must be mapped to a factor or character variable ",
            "(not a numeric variable) in your scatter plot if you set ",
            "groupColour = TRUE or groupFill = TRUE (i.e. use ",
            "`aes(colour = ...)`)"
          )
        }
        
        data <- data[, c("x", "y", "colour", "group"), drop = FALSE]
        if (self$groupFill) {
          data[, "fill"] <- data[, "colour"]
        }
        
        values <- unique(data$colour)
        names(values) <- values
        
        if (self$groupColour && !self$groupFill) {
          xtraMapNames <- c("colour", "group")
        } else if (self$groupColour && self$groupFill) {
          xtraMapNames <- c("colour", "fill")
        } else {
          xtraMapNames <- c("fill", "group")
        }
        
        mapping <- c(mapping, xtraMapNames)
      }
      
      # Boxplot and violin plots need y aes
      if (self$type %in% c("boxplot", "violin")) {
        mapping <- c(mapping, "y")
      }
      
      # Build plot (sans geom)
      plot <- ggplot2::ggplot(data, ggplot2::aes_all(mapping))
      
      if (haveMargMap) {
        if ("colour" %in% xtraMapNames) {
          plot <- plot + ggplot2::scale_colour_manual(values = values)
        }
        if ("fill" %in% xtraMapNames) {
          plot <- plot + ggplot2::scale_fill_manual(values = values)
        }
      }
      
      plot
    },
    
    alterParams = function() {
      prmL2 <- self$prmL
      if (is.null(prmL2$exPrm$colour) && !self$groupColour) {
        prmL2$exPrm[["colour"]] <- "black"
      }
      
      # default to an alpha of .5 if user specifies a margin mapping
      if (is.null(prmL2$exPrm[["alpha"]]) && 
          (self$groupColour || self$groupFill)) {
        prmL2$exPrm[["alpha"]] <- .5
      }
      
      # merge the parameters in an order that ensures that marginal plot params
      # overwrite general params
      prmL2$exPrm <- append(prmL2[[paste0(self$marg, "Prm")]], prmL2$exPrm)
      prmL2$exPrm <- prmL2$exPrm[!duplicated(names(prmL2$exPrm))]
      
      # pull out limit function and use if histogram
      panScale <- private$getPanelScale(self$marg)
      lim_fun <- panScale$get_limits
      if (self$type %in% c("histogram", "densigram") && !is.null(lim_fun)) {
        prmL2$exPrm[["boundary"]] <- lim_fun()[1]
      }
      
      prmL2 <- private$overrideMappedParams(prmL2, "colour", self$groupColour)
      prmL2 <- private$overrideMappedParams(prmL2, "fill", self$groupFill)
      
      prmL2$exPrm
    },
    
    overrideMappedParams = function(prmL2, paramName, groupVar) {
      if (!is.null(prmL2$exPrm[[paramName]]) && groupVar) {
        message(
          "You specified group", paramName, " = TRUE as well as a ", paramName,
          " parameter for a marginal plot. The ", paramName, " parameter will ",
          " be ignored in favor of using ", paramName, "s mapped from the ",
          "scatter plot."
        )
        prmL2$exPrm[[paramName]] <- NULL
      }
      prmL2
    },
    
    addLayers = function(noGeomPlot, finalParms) {
      geomFun <- private$getGeomFun()
      if (self$type %in% c("density", "densigram")) {
        # to create a density plot, we use both geom_density geom_line b/c of issue
        # mentioned at https://twitter.com/winston_chang/status/957057715750166528.
        # also note that we get the colour of the density plot from geom_line and
        # the fill from geom_density (hence the calls to dropParams when creating
        # densityParams and lineParams)
        
        # we get a boundary param added in alterParams if the type is either 
        # histogram or densigram, but we don't want this param to be used when 
        # creating the density plot component of the densigram (only the histogram
        # component needs it), so drop it here
        finalParmsDplot <- private$dropParams(finalParms, "boundary")
        
        # first create geom_density layer
        densityParms <- private$dropParams(finalParmsDplot, "colour")
        if (self$type == "densigram") {
          # drop fill b/c of https://github.com/daattali/ggExtra/issues/123
          densityParms <- private$dropParams(densityParms, "fill")
        }
        layer1 <- do.call(geomFun, densityParms)
        
        # now create geom_line layer
        # we have to drop alpha param b/c of issue mentioned at
        # https://github.com/rstudio/rstudio/issues/2196
        lineParms <- private$dropParams(finalParmsDplot, c("fill", "alpha"))
        lineParms$stat <- "density"
        layer2 <- do.call(ggplot2::geom_line, lineParms)
        
        layers <- list(layer1, layer2)
        
        if (self$type == "densigram") {
          # if type is densigram, have to add a histogram layer to layers list
          layer3 <- do.call(geom_histogram2, finalParms)
          layers <- list(layer3, layers)
        }
      } else {
        layer <- do.call(geomFun, finalParms)
        layers <- list(layer)
      }
      
      plot <- Reduce(`+`, c(list(noGeomPlot), layers))
      
      if (private$needsFlip()) {
        plot <- plot + ggplot2::coord_flip()
      }
      plot
    },
    
    getGeomFun = function() {
      switch(self$type,
             "density" = geom_density2,
             "densigram" = geom_density2,
             "histogram" = ggplot2::geom_histogram,
             "boxplot" = ggplot2::geom_boxplot,
             "violin" = ggplot2::geom_violin
      )
    },
    
    dropParams = function(finalParams, toDrop) {
      finalParams[!names(finalParams) %in% toDrop]
    },
    
    addLimits = function(margThemed) {
      limits <- private$getLimits()
      # for plots with y aes we have to use scale_y_continuous instead of
      # scale_x_continuous.
      if (self$type %in% c("boxplot", "violin")) {
        margThemed +
          ggplot2::scale_y_continuous(limits = limits, oob = scales::squish)
      } else {
        margThemed +
          ggplot2::scale_x_continuous(limits = limits, oob = scales::squish)
      }
    },

    getPanelScale = function(marg) {
      above_221 <- utils::packageVersion("ggplot2") > "2.2.1"
      if (above_221) {
        if (marg == "x") {
          self$scatPbuilt$layout$panel_scales_x[[1]]
        } else {
          self$scatPbuilt$layout$panel_scales_y[[1]]
        }
      } else {
        if (marg == "x") {
          self$scatPbuilt$layout$panel_scales$x[[1]]
        } else {
          self$scatPbuilt$layout$panel_scales$y[[1]]
        }
      }
    },
    
    needsFlip = function() {
      # If the marginal plot is: (for the x margin (top) and is a boxplot) or
      #                          (for the y margin (right) and is not a boxplot),
      # ... then have to flip
      topAndBoxP <- self$marg == "x" && self$type %in% c("boxplot", "violin")
      rightAndNonBoxP <- 
        self$marg == "y" && !(self$type %in% c("boxplot", "violin"))
      topAndBoxP || rightAndNonBoxP
    },
    
    # Get the axis range of the x or y axis of the given ggplot build object
    # This is needed so that if the range of the plot is manually changed, the
    # marginal plots will use the same range
    getLimits = function() {
      marg <- self$marg
      if (private$wasFlipped()) {
        marg <- switch(marg,
                       "x" = "y",
                       "y" = "x"
        )
      }
      scale <- private$getPanelScale(marg)
      range <- scale$get_limits()
      if (is.null(range)) {
        range <- scale$range$range
      }
      range
    }
  )
)

geom_density2 <- function(...) {
  ggplot2::geom_density(colour = "NA", ...)
}

geom_histogram2 <- function(...) {
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), ...)
}

utils::globalVariables("..density..")
