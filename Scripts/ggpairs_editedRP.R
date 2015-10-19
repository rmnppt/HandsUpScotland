ggpairs <- 
function (data, columns = 1:ncol(data), title = "", upper = list(), 
          lower = list(), diag = list(), params = NULL, ..., axisLabels = "show", 
          columnLabels = colnames(data[, columns]), legends = FALSE, 
          verbose = FALSE) 
{
  printInfo <- FALSE
  verbose = verbose || printInfo
  axisLabelChoices <- c("show", "internal", "none")
  axisLabelChoice <- pmatch(axisLabels, axisLabelChoices)
  if (is.na(axisLabelChoice)) {
    warning("axisLabels not in c('show', 'internal', 'none').  Reverting to 'show'")
    axisLabelChoice <- 1
  }
  axisLabels <- axisLabelChoices[axisLabelChoice]
  if (any(columns > ncol(data))) {
    stop(paste("Make sure your 'columns' values are less than ", 
               ncol(data), ".\n\tcolumns = c(", paste(columns, collapse = ", "), 
               ")", sep = ""))
  }
  if (any(columns < 1)) {
    stop(paste("Make sure your 'columns' values are positive.", 
               "\n\tcolumns = c(", paste(columns, collapse = ", "), 
               ")", sep = ""))
  }
  if (any((columns%%1) != 0)) {
    stop(paste("Make sure your 'columns' values are integers.", 
               "\n\tcolumns = c(", paste(columns, collapse = ", "), 
               ")", sep = ""))
  }
  if (length(columnLabels) != length(columns)) {
    stop("The length of the 'columnLabels' does not match the length of the 'columns' being used.")
  }
  if (!is.list(upper) && upper == "blank") {
    upper <- list()
    upper$continuous = "blank"
    upper$combo = "blank"
    upper$discrete = "blank"
  }
  if (!is.list(lower) && lower == "blank") {
    lower <- list()
    lower$continuous = "blank"
    lower$combo = "blank"
    lower$discrete = "blank"
  }
  if (!is.list(diag) && diag == "blank") {
    diag <- list()
    diag$continuous = "blank"
    diag$discrete = "blank"
  }
  if (!is.list(upper)) 
    stop("upper is not a list")
  if (is.null(upper$continuous)) {
    upper$continuous <- "cor"
  }
  if (is.null(upper$combo)) {
    upper$combo <- "box"
  }
  if (is.null(upper$discrete)) {
    upper$discrete <- "facetbar"
  }
  if (!is.list(lower)) 
    stop("lower is not a list")
  if (is.null(lower$continuous)) {
    lower$continuous <- "points"
  }
  if (is.null(lower$combo)) {
    lower$combo <- "facethist"
  }
  if (is.null(lower$discrete)) {
    lower$discrete <- "facetbar"
  }
  if (is.null(diag$continuous)) {
    diag$continuous <- "density"
  }
  if (is.null(diag$discrete)) {
    diag$discrete <- "bar"
  }
  data <- as.data.frame(data)
  for (i in 1:dim(data)[2]) {
    if (is.character(data[, i])) {
      data[, i] <- as.factor(data[, i])
    }
  }
  numCol <- length(columns)
  if (printInfo) 
    cat("data col: ", numCol, "\n")
  ggpairsPlots <- list()
  grid <- rev(expand.grid(y = 1:ncol(data[columns]), x = 1:ncol(data[columns])))
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data[columns])[ycol], yvar = names(data[columns])[xcol])
  }))
  if (printInfo) {
    cat("\n\n\nALL\n")
    print(all)
  }
  dataTypes <- plot_types(data[columns])
  if (printInfo) {
    cat("\n\n\nDATA TYPES\n")
    print(dataTypes)
  }
  if (identical(axisLabels, "internal")) {
    dataTypes$Type <- as.character(dataTypes$Type)
    dataTypes$Type[dataTypes$posx == dataTypes$posy] <- "label"
    dataTypes$Type <- as.factor(dataTypes$Type)
  }
  for (i in 1:nrow(dataTypes)) {
    p <- "blank"
    type <- dataTypes[i, "Type"]
    posX <- as.numeric(as.character(dataTypes[i, "posx"]))
    posY <- as.numeric(as.character(dataTypes[i, "posy"]))
    xColName <- as.character(dataTypes[i, "xvar"])
    yColName <- as.character(dataTypes[i, "yvar"])
    up <- posX > posY
    if (printInfo) 
      cat("Pos #", i, "\t(", posX, ",", posY, ")\t type: ")
    section_aes <- section_params <- NULL
    if (type == "scatterplot") {
      if (printInfo) 
        cat("scatterplot\n")
      subType <- "points"
      if (up) {
        subType <- upper$continuous
        section_aes <- upper$aes_string
        section_params <- upper$params
      }
      else {
        subType <- lower$continuous
        section_aes <- lower$aes_string
        section_params <- lower$params
      }
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 y = yColName, ...), section_aes)
      if (subType == "density") {
        combo_aes <- addAndOverwriteAes(combo_aes, aes_string(group = combo_aes$colour))
        combo_aes
      }
      combo_params <- addAndOverwriteAes(params, section_params)
      p <- make_ggpair_text(subType, combo_aes, combo_params, 
                            printInfo)
    }
    else if (type == "box-hori" || type == "box-vert") {
      if (printInfo) 
        cat("box-hori-vert\n")
      subType <- "box"
      section_aes <- NULL
      if (up) {
        subType <- upper$combo
        section_aes <- upper$aes_string
        section_params <- upper$params
      }
      else {
        subType <- lower$combo
        section_aes <- lower$aes_string
        section_params <- lower$params
      }
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 y = yColName, ...), section_aes)
      if (subType != "dot") 
        combo_aes <- mapping_color_fill(combo_aes)
      combo_params <- addAndOverwriteAes(params, section_params)
      p <- make_ggpair_text(subType, combo_aes, combo_params, 
                            printInfo)
    }
    else if (type == "mosaic") {
      if (printInfo) 
        cat("mosaic\n")
      subType <- "facetbar"
      section_aes <- NULL
      if (up) {
        subType <- upper$discrete
        section_aes <- upper$aes_string
        section_params <- upper$params
      }
      else {
        subType <- lower$discrete
        section_aes <- lower$aes_string
        section_params <- lower$params
      }
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 y = yColName, ...), section_aes)
      combo_params <- addAndOverwriteAes(params, section_params)
      if (subType == "ratio") {
        p <- ggally_ratio(data[, c(yColName, xColName)])
      }
      else if (subType == "facetbar") {
        if (!is.null(combo_aes$colour)) {
          combo_aes <- addAndOverwriteAes(combo_aes, 
                                          aes_string(fill = combo_aes$colour))
        }
        p <- make_ggpair_text(subType, combo_aes, combo_params, 
                              printInfo)
      }
      else if (subType == "blank") {
        p <- "ggally_blank('blank')"
      }
      else {
        p <- ggally_text("Incorrect\nPlot", size = 6)
      }
    }
    else if (type == "stat_bin-num") {
      if (printInfo) 
        cat("stat_bin-num\n")
      subType <- diag$continuous
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 ...), diag$aes_string)
      if (subType != "density") 
        combo_aes <- mapping_color_fill(combo_aes)
      combo_params <- addAndOverwriteAes(params, diag$params)
      if (subType != "blank") {
        p <- make_ggpair_text(paste(subType, "Diag", 
                                    sep = "", collapse = ""), combo_aes, combo_params, 
                              printInfo)
      }
      else {
        p <- "blank"
      }
    }
    else if (type == "stat_bin-cat") {
      if (printInfo) 
        cat("stat_bin-cat\n")
      subType <- diag$discrete
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 ...), diag$aes_string)
      combo_aes <- mapping_color_fill(combo_aes)
      combo_params <- addAndOverwriteAes(params, diag$params)
      p <- make_ggpair_text(paste(subType, "Diag", sep = "", 
                                  collapse = ""), combo_aes, combo_params, printInfo)
    }
    else if (type == "label") {
      combo_aes <- addAndOverwriteAes(aes_string(x = xColName, 
                                                 ...), diag$aes_string)
      combo_params <- addAndOverwriteAes(params, diag$params)
      combo_params <- addAndOverwriteAes(combo_params, 
                                         c(label = columnLabels[posX]))
      p <- make_ggpair_text("diagAxis", combo_aes, combo_params, 
                            printInfo)
    }
    ggpairsPlots[[length(ggpairsPlots) + 1]] <- p
  }
  plotMatrix <- list(data = data, columns = columns, plots = ggpairsPlots, 
                     title = title, verbose = verbose, printInfo = printInfo, 
                     axisLabels = axisLabels, columnLabels = columnLabels, 
                     legends = legends, gg = NULL)
  attributes(plotMatrix)$class <- c("gg", "ggpairs")
  plotMatrix
}