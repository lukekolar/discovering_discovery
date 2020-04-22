#### data test

library(tidyverse)
library(RColorBrewer)

### page 1 functions:

## ranks plot 

library(grid)

data.main <- readRDS("scraped_data/discogs_samp_data.RDS")
total_ranks <- readRDS("scraped_data/total_ranks.RDS")
mycolors <- colorRampPalette(c("steelblue1", "turquoise2", "turquoise3"))(15)


create_ranks.graph <- function(year.slide){
    ranks.graph <- ggplot(total_ranks %>% filter(year == year.slide), 
                      aes(rank, group = main.song, 
                          fill = as.factor(main.song))) +
    geom_tile(aes(y = total.all/2,
                  height = total.all,
                  width = 0.9), alpha = 0.8, color = "black") +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) +
    labs(y = "", x = "") +
    geom_text(aes(y = 0, label = paste0(main.song, " ")), vjust = 0.2, 
              family = "Courier", hjust = 1, size = 3) +
    geom_text(aes(y = total.all, label = paste0(Value_lbl, " total "), 
                  hjust = 0, vjust = -0.7, family = "Courier", fontface = "bold"), size = 2.5) +
    geom_text(aes(y = total.all, label = paste0(" ", total.year, " new"), 
                  hjust = 0, vjust = 1.7, family = "Courier", fontface = "italic"), size = 2.5) +
    scale_fill_manual(values = c(mycolors)) +
    scale_x_reverse() + 
    coord_flip(clip = "off", expand = FALSE) +
    guides(color = FALSE, fill = FALSE) +
    theme(text = element_text(family = "Courier"),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic"),
          plot.margin = margin(1, 1.3, 0.8, 3, "cm"))
    ranks.graph
}

create.type.summary.plot <- function(year.slide){
 
    label <- data.frame(type = "Covers", count = 135, label = year.slide)    
       
    ggplot((data.main %>% 
                filter(!year > year.slide,
                       !type == "samples") %>% 
                mutate(type = str_replace(type, "sampled", "Samples"),
                       type = str_replace(type, "remix", "Remixes"),
                       type = str_replace(type, "cover", "Covers")) %>% 
                group_by(type) %>%  
                distinct(track, year) %>% 
                summarize(count = n())), aes(x = type, y = count)) + geom_col(fill = "turquoise2", color = "grey") + 
        coord_flip() + theme_classic() + ylim(0, 150) + geom_text(aes(x = type, label = count, hjust = -0.5, 
                                                                      family = "Courier"), size = 5) +
        theme(text = element_text(family = "Courier"),
              axis.text.y = element_text(angle = 60, size = 12, hjust = 0.66, vjust = 0),
              axis.text.x = element_text(size = 10),
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(0, 20, 0, 0), "pt")) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 155)) +
        labs(x = "", y = "") +
        geom_label(data = label, aes(label = label), fontface = "bold", 
                   size = 8, family = "Courier", label.padding = unit(0.5, "lines"), color = "white", fill = "steelblue")
    
}

## ggenealogy trees

library(ggenealogy)

sampd.network.raw <- readRDS("scraped_data/sampd_network_raw.RDS") %>% 
    filter(!style == "Breakbeat")

find.ind.network <- function(songg){
    list1 <- sampd.network.raw %>% filter(main.song == songg, !song == songg) %>% distinct(song)
    list2 <- sampd.network.raw %>% filter(main.song %in% list1$song, !song %in% c(list1$song, songg)) %>% distinct(song)
    list3 <- sampd.network.raw %>% filter(main.song %in% list2$song, !song %in% c(list2$song, songg)) %>% distinct(song)
    list4 <- sampd.network.raw %>% filter(main.song %in% list3$song, !song%in% c(list3$song, songg)) %>% distinct(song)
    cant.be <- rbind(list1, list2, list3, list4)
    cant.be
}

buildMinusPathDF = function(path, geneal, ig, colName, colNameY, bin = 12){
    
    if(class(ig)!="igraph"){
        stop("ig must be an igraph object")
    }
    
    if(class(bin) != "numeric"){
        stop("bin must contain a number")
    }
    
    if(mode(path)=="character"){
        if(length(path)!=2){
            stop("path needs to contain two variety names")
        }
        varieties <- path
        path <- getPath(varieties[1], varieties[2], ig, colName)
    } else if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
        stop("path does not appear to be a result of the getPath() function")
    } 
    
    tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
    #eG <- igraph::get.data.frame(ig, "edges")
    
    if (colNameY !=""){
        rowNametG <- rownames(tG)
        rownames(tG) <- 1:nrow(tG)
        rowNameG <- rownames(geneal)
        rownames(geneal) <- 1:nrow(geneal)
        tG$y <- geneal[match(tG$name, geneal$child),][[colNameY]]
        rownames(tG) <- rowNametG
        rownames(geneal) <- rowNameG
    }
    
    label=tG$name
    x=tG[[colName]]
    y=tG$y
    # If the label is part of the path, then we change its value to NA
    for (i in 1:length(label)){
        if (label[i]%in%path$pathVertices){
            label[i]=NA
        }
    }
    plotMinusPathDF = data.frame(label,x,y)
    
    # Return the data frame object of the full genealogy
    plotMinusPathDF
}

buildPlotTotalDF = function(path, geneal, ig, colName, colNameY = "", bin = 12){
    if(class(ig)!="igraph"){
        stop("ig must be an igraph object")
    }
    
    if(mode(path)=="character"){
        if(length(path)!=2){
            stop("path needs to contain two variety names")
        }
        varieties <- path
        path <- getPath(varieties[1], varieties[2], ig, colName)
    } else if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
        stop("path does not appear to be a result of the getPath() function")
    } 
    
    if(class(bin) != "numeric"){
        stop("bin must contain a number")
    }
    
    tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
    
    if (colNameY !=""){
        rowNametG <- rownames(tG)
        rownames(tG) <- 1:nrow(tG)
        rowNameG <- rownames(geneal)
        rownames(geneal) <- 1:nrow(geneal)
        tG$y <- geneal[match(tG$name, geneal$child),][[colNameY]]
        rownames(tG) <- rowNametG
        rownames(geneal) <- rowNameG
    }
    
    label=path$pathVertices
    x=as.numeric(path$variableVertices)
    xstart=x
    xend=rep(0,length(label))
    ystart=rep(0,length(label))
    yend=rep(0,length(label))
    for (i in 2:length(label)){
        ystart[i-1] = tG$y[match(label[i-1], tG$name)]
        yend[i-1] = tG$y[match(label[i], tG$name)]
        xend[i-1] = xstart[i]
    }
    ystart[i] = yend[i-1]
    yend[i] = ystart[i]
    xend[i] = xstart[i]
    y = ystart
    plotTotalDF = data.frame(label,xstart,ystart,xend,yend,x,y)
    
    plotTotalDF
}

buildSpreadTotalDF = function(geneal, ig, colName, bin = 12){
    if(class(ig)!="igraph"){
        stop("ig must be an igraph object.")
    }
    
    if(class(bin) != "numeric"){
        stop("bin must contain a number")
    }
    
    totalDF = igraph::get.data.frame(ig, "vertices")
    #totalDF = totalDF[!is.na(totalDF$name),]
    
    dateVector = c()
    for (i in 1:dim(totalDF)[1]){
        currYear = getVariable(totalDF[i,],geneal,colName)
        dateVector = c(dateVector, currYear)
    }
    
    totalDF2 = cbind(totalDF, dateVector)
    colnames(totalDF2)[2] = colName
    totalDF = totalDF2
    
    totalDF = totalDF[order(totalDF[[colName]], decreasing=FALSE), ]
    
    numrows <- ceiling(nrow(totalDF)/bin)
    
    idx <- matrix(1:(numrows*bin), ncol=bin, nrow=numrows, byrow=TRUE)
    idx <- idx[, 1:bin]
    idx <- as.numeric(t(idx))[1:nrow(totalDF)]
    
    spreadTotalDF <- totalDF
    spreadTotalDF$y <- jitter(rep(1:numrows, length.out=nrow(totalDF)), amount=.5)[idx]
    
    spreadTotalDF
}

buildEdgeTotalDF = function(geneal, ig, colName, bin = 12){
    
    if(class(ig)!="igraph"){
        stop("ig must be an igraph object")
    }
    
    if(class(bin) != "numeric"){
        stop("bin must contain a number")
    }
    
    tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
    eG <- igraph::get.data.frame(ig, "edges")
    
    # edgeTotalDF used in function plotPathOnAll()
    numEdges = length(igraph::E(ig))
    x=as.numeric(rep("",numEdges))
    y=as.numeric(rep("",numEdges))
    xend=as.numeric(rep("",numEdges))
    yend=as.numeric(rep("",numEdges))
    name=as.numeric(rep("",numEdges))
    nameEnd=as.numeric(rep("",numEdges))
    # For each edge in the graph
    for (i in 1:numEdges){
        xname = as.character(eG[i,]$from)
        xendname = as.character(eG[i,]$to)
        x_i = getVariable(xname, tG, colName)
        xend_i = getVariable(xendname, tG, colName)
        if(!xname%in%tG$name) {
            stop(paste(xname, "cannot be found in ig vertices"))
        }
        if(!xendname%in%tG$name) {
            stop(paste(xendname, "cannot be found in ig vertices"))
        }
        y_i = tG$y[which(tG$name==xname)]
        yend_i = tG$y[which(tG$name==xendname)]
        x[i] = x_i
        xend[i] = xend_i
        y[i] = y_i
        yend[i] = yend_i
        name[i] = xname
        nameEnd[i] = xendname
    }
    # Create a dataframe containing the start and end positions of the x and y axes
    edgeTotalDF = as.data.frame(cbind(x, y, xend, yend, name, nameEnd))
    
    edgeTotalDF
}

plotPathOnAll.new <- function (path, geneal, ig, colName, colNameY = "", bin = 12, edgeCol = "gray84", 
                               pathEdgeCol = "seagreen", nodeSize = 3, pathNodeSize = 3, 
                               pathNodeFont = "bold", nodeCol = "black", animate = FALSE, 
                               nodeFam = "Courier", pathNodeFam = "Courier") {
    x <- y <- xend <- yend <- xstart <- ystart <- label <- NULL
    if (class(ig) != "igraph") {
        stop("ig must be an igraph object")
    }
    if (mode(path) == "character") {
        if (length(path) != 2) {
            stop("path needs to contain two variety names")
        }
        varieties <- path
        path <- getPath(varieties[1], varieties[2], ig, colName)
    }
    else if (sum(names(path) %in% c("pathVertices", "variableVertices")) != 
             2) {
        stop("path does not appear to be a result of the getPath() function")
    }
    pMPDF <- buildMinusPathDF(path, geneal, ig, colName, colNameY, 
                              bin)
    eTDF <- buildEdgeTotalDF(geneal, ig, colName, bin)
    pTDF <- buildPlotTotalDF(path, geneal, ig, colName, colNameY, 
                             bin)
    eTDF <- stats::na.omit(eTDF)
    textFrame = data.frame(x = pMPDF$x, y = pMPDF$y, label = pMPDF$label)
    textFrame = transform(textFrame, w = graphics::strwidth(pMPDF$label, 
                                                            "inches") + 0.25, h = graphics::strheight(pMPDF$label, 
                                                                                                      "inches") + 0.25)
    textFrame <- stats::na.omit(textFrame)
    eTDF <- eTDF[(eTDF$x %in% textFrame$x) & (eTDF$xend %in% 
                                                  textFrame$x), ]
    rowETDF <- rownames(eTDF)
    rowTextFrame <- rownames(textFrame)
    rownames(eTDF) <- 1:nrow(eTDF)
    rownames(textFrame) <- 1:nrow(textFrame)
    eTDF$y <- textFrame[match(eTDF$name, textFrame$label), ]$y
    eTDF$yend <- textFrame[match(eTDF$nameEnd, textFrame$label), 
                           ]$y
    eTDF <- eTDF[, -c(5, 6)]
    rownames(eTDF) <- rowETDF
    rownames(textFrame) <- rowTextFrame
    eTDF$x <- as.integer(as.character(eTDF$x))
    eTDF$xend <- as.integer(as.character(eTDF$xend))
    plotTotalImage = ggplot2::ggplot(data = pMPDF, ggplot2::aes(x = x, 
                                                                y = y)) + ggplot2::geom_segment(data = eTDF, ggplot2::aes(x = x, 
                                                                                                                          y = y, xend = xend, yend = yend), colour = edgeCol) + 
        ggplot2::geom_segment(data = pTDF, ggplot2::aes(x = xstart, 
                                                        y = ystart, xend = xend, yend = yend), colour = pathEdgeCol, 
                              size = 1) + ggplot2::geom_text(data = textFrame, 
                                                             ggplot2::aes(x = x, y = y, label = label), size = nodeSize, 
                                                             colour = nodeCol, family = nodeFam)
    plotTotalImage = plotTotalImage + ggplot2::geom_text(data = pTDF, 
                                                         ggplot2::aes(x = x, y = y, label = label), size = pathNodeSize, 
                                                         fontface = pathNodeFont, family = pathNodeFam) + ggplot2::xlab(colName) + ggplot2::theme(legend.position = "none", 
                                                                                                                                                  panel.grid.minor = ggplot2::element_blank())
    if (colNameY == "") {
        plotTotalImage = plotTotalImage + ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                                                         axis.ticks.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), 
                                                         panel.grid.major.y = ggplot2::element_blank())
    }
    if (colNameY != "") {
        plotTotalImage = plotTotalImage + ggplot2::ylab(colNameY)
    }
    if (animate == FALSE) {
        plotTotalImage
    }
    else {
        if (colNameY == "") {
            animatePlotTotalImage <- plotly::plotly_build(plotly::ggplotly(plotTotalImage, 
                                                                           tooltip = c("x", "label")))
            for (i in 1:length(animatePlotTotalImage$x$data[[3]]$hovertext)) {
                animatePlotTotalImage$x$data[[3]]$hovertext[i] <- strsplit(animatePlotTotalImage$x$data[[3]]$hovertext[i], 
                                                                           "<br />x:", fixed = TRUE)[[1]][1]
                animatePlotTotalImage$x$data[[3]]$hovertext[i] <- gsub("^.", 
                                                                       colName, animatePlotTotalImage$x$data[[3]]$hovertext[i])
            }
            for (i in 1:length(animatePlotTotalImage$x$data[[4]]$hovertext)) {
                animatePlotTotalImage$x$data[[4]]$hovertext[i] <- strsplit(animatePlotTotalImage$x$data[[4]]$hovertext[i], 
                                                                           "<br />x:", fixed = TRUE)[[1]][1]
                animatePlotTotalImage$x$data[[4]]$hovertext[i] <- gsub("^.", 
                                                                       colName, animatePlotTotalImage$x$data[[4]]$hovertext[i])
            }
            animatePlotTotalImage
        }
        else {
            animatePlotTotalImage <- plotly::plotly_build(plotly::ggplotly(plotTotalImage, 
                                                                           tooltip = c("x", "label")))
            for (i in 1:length(animatePlotTotalImage$x$data[[3]]$hovertext)) {
                animatePlotTotalImage$x$data[[3]]$hovertext[i] <- strsplit(animatePlotTotalImage$x$data[[3]]$hovertext[i], 
                                                                           "<br />x:", fixed = TRUE)[[1]][1]
                animatePlotTotalImage$x$data[[3]]$hovertext[i] <- gsub("^.", 
                                                                       colName, animatePlotTotalImage$x$data[[3]]$hovertext[i])
                animatePlotTotalImage$x$data[[3]]$hovertext[i] <- paste0(animatePlotTotalImage$x$data[[3]]$hovertext[i], 
                                                                         "<br />", colNameY, ": ", animatePlotTotalImage$x$data[[3]]$y[i])
            }
            for (i in 1:length(animatePlotTotalImage$x$data[[4]]$hovertext)) {
                animatePlotTotalImage$x$data[[4]]$hovertext[i] <- strsplit(animatePlotTotalImage$x$data[[4]]$hovertext[i], 
                                                                           "<br />x:", fixed = TRUE)[[1]][1]
                animatePlotTotalImage$x$data[[4]]$hovertext[i] <- gsub("^.", 
                                                                       colName, animatePlotTotalImage$x$data[[4]]$hovertext[i])
                animatePlotTotalImage$x$data[[4]]$hovertext[i] <- paste0(animatePlotTotalImage$x$data[[4]]$hovertext[i], 
                                                                         "<br />", colNameY, ": ", animatePlotTotalImage$x$data[[4]]$y[i])
            }
            animatePlotTotalImage
        }
    }
}

plotAncDes.new <- function(v1, geneal, mAnc=3, mDes=3, vColor="#D35C79", family = "Courier"){
    color <- x <- y <- label2 <- size <- xstart <- ystart <- xend <- yend <- branchx <- branchy <- NULL
    # Plot the data frame, if it exists
    geneal = geneal[which(geneal$parent!=""),]
    gDF = buildAncDesTotalDF(v1, geneal, mAnc, mDes)
    gDF[gDF$root.gen==0&gDF$gen==0,]$color = vColor
    if(nrow(gDF)>0){
        plotGenImage = ggplot2::qplot(data=gDF, x=x, y=y, label=label2, geom="text", vjust=-.25, hjust=.5, family = family,
                                      size=size, colour=color) +
            ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend),inherit.aes=F) + 
            # Draw the underline of the variety
            ggplot2::geom_segment(ggplot2::aes(x=xend, y=yend, xend=branchx, yend=branchy),inherit.aes=F) +
            # ggplot2::facet_wrap(~variety, scales="free", ncol=2) +
            ggplot2::scale_size_continuous(range=c(3,3),guide="none") +
            ggplot2::scale_colour_identity() +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           axis.text=ggplot2::element_blank(), 
                           axis.ticks=ggplot2::element_blank()) +
            ggplot2::labs(x="",y="")
    } else {
        plotGenImage = ggplot2::ggplot() + 
            ggplot2::geom_text(ggplot2::aes(x=0, y=0, label="Please select varieties\n\n Note: It may take a moment to process the v1")) +         
            ggplot2::theme_bw() + 
            ggplot2::theme(axis.text=ggplot2::element_blank(), 
                           axis.ticks=ggplot2::element_blank(), 
                           axis.title=ggplot2::element_blank()) +
            ggplot2::labs(x="",y="")
    }
    plotGenImage
}

create.sample.net.plot <- function(songg1 = "Harder, Better, Faster, Stronger", songPath){
    
    song.ind.network <- sampd.network.raw %>%
        filter(artist != "Daft Punk") %>%
        mutate(child = song,
               parent = main.song) %>%
        filter(!child == "Με ποιο σκεπτικό") %>% 
        filter(!child == "One More Bootleg") %>% 
        filter(!child == parent) %>%
        select(child, parent, year) %>% 
        distinct(child, parent, year) %>% 
        filter(parent == songg1 | parent %in% find.ind.network(songg = songg1)$song) %>% 
        rbind(tibble(child = songg1, parent = "", year = 2001)) %>% 
        mutate(year = as.numeric(year)) %>% 
        mutate(child = str_replace(child, "Bitch", "B***h"),
               child = str_replace(child, "Fuck", "F**k"),
               parent = str_replace(parent, "Bitch", "B***h"),
               parent = str_replace(parent, "Fuck", "F**k"),
               child = str_replace(child, "Hoes", "H**s"),
               parent = str_replace(parent, "Hoes", "H**s"),
               child = str_replace(child, "DOUCHE", "D****E"),
               parent = str_replace(parent, "DOUCHE", "D****E"))
    
    song.ind.network.IG <- dfToIG(song.ind.network)
    song.ind.network.CP <- na.omit(c(song.ind.network$child, song.ind.network$parent))
    
    uChild <- unique(na.omit(song.ind.network$child))
    uParent <- unique(na.omit(song.ind.network$parent))
    
    pathRandom <- getPath(songg1, songPath, 
                          song.ind.network.IG, song.ind.network, "year")
    
    plotPathOnAll.new(pathRandom, geneal = song.ind.network, ig = song.ind.network.IG, 
                      colName = "year", bin = ceiling(nrow(song.ind.network)/10), 
                      edgeCol = "grey", pathEdgeCol = "steelblue1", 
                      nodeSize = 2, pathNodeSize = 2.5,
                      nodeCol = "grey", pathNodeFont = "bold") +
        theme_classic() + xlim(1998, 2022) + 
        theme(panel.border = element_rect(colour = "black", fill = NA),
              text = element_text(family = "Courier"),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.line.x = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = paste0("Sample Network of '", songg1, "'"))
}

create.sample.tree.plot <- function(songg2){
    
    song.ind.network.anc <- sampd.network.raw %>%
        filter(artist != "Daft Punk") %>%
        mutate(child = song,
               parent = main.song) %>%
        filter(!child == "Με ποιο σκεπτικό") %>% 
        filter(!child == "One More Bootleg") %>% 
        filter(!child == parent) %>%
        select(child, parent, year) %>% 
        distinct(child, parent, year) %>% 
        filter(parent == songg2 | parent %in% find.ind.network(songg = songg2)$song) %>% 
        rbind(tibble(child = songg2, parent = "", year = 2001)) %>% 
        mutate(year = as.numeric(year)) %>% 
        mutate(child = str_replace(child, "Bitch", "B***h"),
               child = str_replace(child, "Fuck", "F**k"),
               parent = str_replace(parent, "Bitch", "B***h"),
               parent = str_replace(parent, "Fuck", "F**k"),
               child = str_replace(child, "DOUCHE", "D****E"),
               parent = str_replace(parent, "DOUCHE", "D****E"),
               child = str_replace(child, "Hoes", "H**s"),
               parent = str_replace(parent, "Hoes", "H**s")) %>% 
        mutate(child = str_trunc(child, 35, side = "center"),
               parent = str_trunc(parent, 35, side = "center"))
    
    plotAncDes.new(songg2, song.ind.network.anc, vColor = "blue") + 
        theme(text = element_text(family = "Courier"),
              panel.border = element_rect(color = "black", fill = NA),
              plot.title = element_text(face = "bold")) + 
        labs(title = paste0("Sample Tree of '", songg2, "'"))
}

HBFS.tracks <- (find.ind.network("Harder, Better, Faster, Stronger") %>% 
        filter(!song == "Harder, Better, Faster, Stronger", 
               !song == "Face to Face / Short Circuit") %>% 
        arrange(song) %>% 
        mutate(song = str_replace(song, "DOUCHE", "D****E"),
               song = str_replace(song, "Hoes", "H**s")))$song

tree.tracklist <- c("One More Time", "Aerodynamic", "Digital Love",
               "Crescendolls", "Nightvision", "Superheroes", "High Life", 
               "Something About Us", "Voyager", "Veridis Quo",
               "Short Circuit", "Face to Face", "Too Long")

##


#### shiny app

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "Discovering \n'Discovery'",
                    titleWidth = 300),
    dashboardSidebar(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),                    # hides warning messages
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        width = 300,
        sidebarMenu(
            menuItem("Influence", tabName = "influence", icon = icon("th")),
            menuItem("Lyrics", tabName = "lyrics", icon = icon("th")),
            menuItem("Popularity", tabName = "popularity", icon = icon("th"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "influence",
                fluidRow(column(width = 12,
                    box(tags$div(class = "header", 
                                 tags$b(textOutput("ranks.graph.title"))),
                        plotOutput("ranks.graph")),
                    
                    box(sliderInput("year.slide", label = NULL, min = 2000, max = 2020,
                                    sep = '', ticks = 21, value = 2020),
                        plotOutput("type.graph"),
                        helpText("Shows total unique uses by type, excluding repeats.")),
                    padding = 15
                    )
                ),
                fluidRow(column(width = 12,
                    box(plotOutput("HBFS.network"),
                        selectInput("net.track", "Show path to..", 
                                    choices = HBFS.tracks),
                                    helpText("Tracks that sample 'Harder, Better,...', or tracks sampling those tracks.")),
                    box(plotOutput("tree.plot"),
                        selectInput("tree.choice", "Show samples of...", 
                                    choices = tree.tracklist),
                        helpText("Covers and remixes are not shown - only samples.")),
                    padding = 15
                    )
                )),
            
            tabItem(tabName = "lyrics",
                    h2("Coming soon...")),
            tabItem(tabName = "popularity",
                    h2("Coming soon..."))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ranks.graph.title <- renderText({
        paste0("Total Uses by Song (year: ", input$year.slide, ")")
    })
    output$ranks.graph <- renderPlot({
        create_ranks.graph(input$year.slide)
    })
    output$type.graph <- renderPlot({
        create.type.summary.plot(input$year.slide)
    }, height = 150)
    output$HBFS.network <- renderPlot({
        create.sample.net.plot(songg1 = "Harder, Better, Faster, Stronger",
                               songPath = input$net.track)
    })
    output$tree.plot <- renderPlot({
        create.sample.tree.plot(songg2 = input$tree.choice)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
