#### data test

library(plotly)
library(lubridate)
library(tidyverse)
library(RColorBrewer)


### page 1 functions:

## ranks plot 

library(grid)
library(patchwork)
library(gganimate)

data.main <- readRDS("scraped_data/discogs_samp_data.RDS")
total_ranks <- readRDS("scraped_data/total_ranks.RDS")
mycolors <- colorRampPalette(c("steelblue1", "turquoise2", "turquoise3"))(15)
daft <- readRDS("scraped_data/discovery_song_qualities.RDS")

all.14.tracks <- c("One More Time", "Aerodynamic", "Digital Love", "Harder, Better, Faster, Stronger",
                   "Crescendolls", "Nightvision", "Superheroes", "High Life", 
                   "Something About Us", "Voyager", "Veridis Quo",
                   "Short Circuit", "Face to Face", "Too Long")

all.14.tracks.mod <- c("One More Time", "Aerodynamic", "Digital Love", "Harder, Better,...",
                   "Crescendolls", "Nightvision", "Superheroes", "High Life", 
                   "Something About Us", "Voyager", "Veridis Quo",
                   "Short Circuit", "Face to Face", "Too Long")

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

create.non.ranked.plot <- function(year.slide){
    
    total_ranks.plot <- total_ranks %>%
        mutate(main.song = str_replace(main.song, "Harder, Better, Faster, Stronger", 
                                       "Harder, Better,...")) %>% 
        mutate(main.song = factor(main.song, levels = all.14.tracks.mod))
    
    ggplot(total_ranks.plot %>% filter(year == year.slide), aes(x = main.song, y = total.all)) + 
        geom_col(fill = "lightblue2", color = "grey") + labs(x = "", y = "Cumulative Uses") + 
        theme_classic() + scale_y_continuous(expand = c(0, 0), limits = c(0, 99)) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              text = element_text("Courier")) +
        geom_text(aes(y = total.all, x = main.song, 
                      label = paste0(ifelse(total.all != 0, total.all, ""))),
                  family = "Courier", size = 4.5, fontface = "bold", vjust = -1) +
        geom_label(aes(x = "Short Circuit", y = 65, label = year.slide), fontface = "bold",
                   size = 8, family = "Courier", label.padding = unit(0.5, "lines"), 
                   color = "white", fill = "steelblue")
    
}

create.samps.anim <- function(){
    
    songs.anim <- ggplot(total_ranks, aes(rank, group = main.song, 
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
                  family = "Courier", hjust = 1) +
        geom_text(aes(y = total.all, label = Value_lbl, hjust = 0, 
                      family = "Courier", fontface = "bold")) +
        scale_fill_manual(values = c(mycolors)) +
        scale_x_reverse() +
        transition_states(year, transition_length = 3, state_length = 1.5) + 
        coord_flip(clip = "off", expand = FALSE) +
        guides(color = FALSE, fill = FALSE) + 
        labs(title = 'Total Uses by Song: {closest_state}',  
             subtitle  =  "Cumulative samples, covers, and remixes over time") +
        theme(text = element_text(family = "Courier"),
              plot.title = element_text(hjust = 2, size = 23, face = "bold"),
              plot.subtitle = element_text(hjust = -28, size = 10, face = "italic"),
              plot.margin = margin(1, 1.2, 0.8, 4.8, "cm")) +
        view_follow(fixed_x = TRUE)
    
    animate(songs.anim, 200, fps = 20)
}

create.samp.donut <- function(year.slide, song){
    
    songs.with.years <- 
        tibble(main.song = 
                   unlist(map(as.list(pull(data.main %>% 
                                               filter(!type == "samples") %>% 
                                               distinct(main.song))), 
                              ~rep(., length(pull(data.main %>% 
                                                      filter(!type == "samples") %>% 
                                                      distinct(year)))))),
               year = rep(pull(data.main %>% 
                                   filter(!type == "samples") %>% 
                                   distinct(year) %>% 
                                   arrange(year)), 
                          length(pull(data.main %>% 
                                          filter(!type == "samples") %>%
                                          distinct(main.song))))) %>% 
        mutate(year = as.numeric(year)) %>% 
        slice(rep(1:n(), each = 3)) %>% 
        mutate(type = rep(c("sampled", "cover", "remix"), n()/3))
    
    set1 <- data.main %>% 
        select(type, main.song, year) %>% 
        mutate(year = as.numeric(year)) %>% 
        filter(!type == "samples") %>% 
        group_by(year, type, main.song) %>% 
        summarize(total.i = n()) %>% 
        full_join(songs.with.years, by = c("main.song", "year", "type")) %>% 
        mutate(total.i = replace_na(total.i, 0)) %>% 
        arrange(year) %>% 
        group_by(main.song, type) %>% 
        mutate(total.each = cumsum(total.i))
    
    set2 <- data.main %>% 
        select(type, main.song, year) %>% 
        mutate(year = as.numeric(year)) %>% 
        filter(!type == "samples") %>% 
        group_by(year, main.song) %>% 
        summarize(total.year = n()) %>% 
        ungroup() %>% 
        full_join(tibble(year = rep(c(2000:2020), 14), 
                         main.song = unlist(map(list("One More Time", "Aerodynamic", "Digital Love",
                                                     "Harder, Better, Faster, Stronger", "Crescendolls", 
                                                     "Nightvision", "Superheroes", "High Life", 
                                                     "Something About Us", "Voyager", "Veridis Quo",
                                                     "Short Circuit", "Face to Face", "Too Long"), ~ rep(.x, 21)))),
                  by = c("year", "main.song")) %>% 
        mutate(total.year = replace_na(total.year, 0)) %>% 
        arrange(desc(total.year)) %>% 
        distinct(year, main.song, .keep_all = TRUE) %>% 
        group_by(main.song) %>% 
        arrange(year) %>% 
        mutate(total.all = cumsum(total.year))
    
    set3 <- set2 %>% 
        full_join(set1, by = c("year", "main.song")) %>%  
        mutate(fraction = total.each/total.all)
    
    set <- set3 %>% filter(year == year.slide, 
                           main.song == song) %>% 
        mutate(type = factor(type, levels = c("sampled", "cover", "remix")))
    
    samps <- set %>% filter(type == "sampled") %>% pull(total.each)
    covs <- set %>% filter(type == "cover") %>% pull(total.each)
    rems <- set %>% filter(type == "remix") %>% pull(total.each)
    
    plot <- 
        
        if(samps == 0 & covs == 0 & rems == 0){
            ggplot(tibble(main.song = song, val = 1), aes(xmin = 3.25, xmax = 4, ymin = 0, 
                                                          ymax = val), fill = NA) + xlim(1, 5) + coord_polar(theta="y", start = 0) +
                annotate(geom = "text", x = 5, y = 0.5, 
                         label = ifelse(song != "Harder, Better, Faster, Stronger", 
                                        ifelse(song != "Something About Us", song, "Something About..."), 
                                        "Harder, Better,..."), size = 2.75, 
                         fontface = "bold", family = "Courier") + 
                annotate(geom = "text", x = 1, y = 0, 
                         label = paste0("Samples: 0\nCovers: 0\nRemixes: 0"), 
                         size = 2.25, family = "Courier") + theme_void() +
                theme(plot.margin = margin(-0.4, -0.5, -0.4, -0.5, "cm"))
        }else{ 
            ggplot(set, aes(xmin = 3.25, xmax = 4, fill = type, ymin = c(0, head(cumsum(fraction), n=-1)), 
                            ymax = cumsum(fraction))) + xlim(1, 5) +
                coord_polar(theta="y", start = 0) + 
                scale_fill_manual(values = c("turquoise2", "steelblue1", "turquoise4"), name = "",
                                  breaks = c("sampled", "cover", "remix")) +
                geom_rect(color = "grey", size = 0.25) + labs(x = "", y = "") + theme_classic() + 
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank(),
                      text = element_text(family = "Courier"),
                      plot.title = element_text(hjust = 0.5, face = "bold"),
                      plot.margin = margin(-0.4, -0.5, -0.4, -0.5, "cm"),
                      legend.position = "none") + 
                annotate(geom = "text", x = 5, y = 0, 
                         label = ifelse(song != "Harder, Better, Faster, Stronger", 
                                        ifelse(song != "Something About Us", song, "Something About..."), 
                                        "Harder, Better,..."), size = 2.75, 
                         fontface = "bold", family = "Courier") + 
                annotate(geom = "text", x = 1, y = 0, 
                         label = paste0("Samples: ", samps, "\nCovers: ", covs, "\nRemixes: ", rems), 
                         size = 2.25, family = "Courier")
        }
}

create.pie.plots <- function(year.slide){
    
    track1 <- create.samp.donut(year.slide = year.slide, song = "One More Time")
    track2 <- create.samp.donut(year.slide = year.slide, song = "Aerodynamic")
    track3 <- create.samp.donut(year.slide = year.slide, song = "Digital Love")
    track4 <- create.samp.donut(year.slide = year.slide, song = "Harder, Better, Faster, Stronger")
    track5 <- create.samp.donut(year.slide = year.slide, song = "Crescendolls")
    track6 <- create.samp.donut(year.slide = year.slide, song = "Nightvision")
    track7 <- create.samp.donut(year.slide = year.slide, song = "Superheroes")
    track8 <- create.samp.donut(year.slide = year.slide, song = "High Life")
    track9 <- create.samp.donut(year.slide = year.slide, song = "Something About Us")
    track10 <- create.samp.donut(year.slide = year.slide, song = "Voyager")
    track11 <- create.samp.donut(year.slide = year.slide, song = "Veridis Quo")
    track12 <- create.samp.donut(year.slide = year.slide, song = "Short Circuit")
    track13 <- create.samp.donut(year.slide = year.slide, song = "Face to Face")
    track14 <- create.samp.donut(year.slide = year.slide, song = "Too Long")
     
    year.lab <- ggplot() + ylim(-1,1) + xlim(-1,1) + theme_void() + 
        theme(plot.margin = margin(-0.4, -0.5, -0.4, -0.5, "cm")) +
        annotate("label", x = 0.1, y = 0, label = year.slide,
                 family = "Courier", size = 7, label.padding = unit(0.5, "lines"), 
                 color = "white", fill = "steelblue", face = "bold")
    
    (track1 | track2 | track3 | track4 | track5) /
        (track6 | track7 | year.lab | track8 | track9) / 
        (track10 | track11 | track12 | track13 | track14)
    
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
                summarize(count = n())), aes(x = type, y = count)) + geom_col(fill = "lightblue2", color = "grey") + 
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

create.uses.line.plot <- function(year.slide){
    
    data <- data.main %>% 
        group_by(year) %>% 
        filter(!type == "samples") %>% 
        mutate(type = str_replace(type, "sampled", "Samples"),
               type = str_replace(type, "remix", "Remixes"),
               type = str_replace(type, "cover", "Covers")) %>% 
        group_by(type, year) %>%  
        distinct(track, year) %>% 
        summarize(count = n()) %>% 
        ungroup() %>% 
        rbind(tibble(year = rep(2000:2020, 3), type = c(rep("Covers", 21), rep("Remixes", 21), 
                                                        rep("Samples", 21)), count = 0)) %>% 
        group_by(type) %>% 
        arrange(year) %>% 
        mutate(total.type = cumsum(count)) %>% 
        arrange(desc(count)) %>% 
        distinct(type, year, .keep_all = T) %>% 
        ungroup() %>% 
        mutate(year = as.numeric(year)) %>% 
        mutate(type = factor(type, levels = c("Samples", "Covers", "Remixes")))
    
    ggplot(data, aes(x = year, y = total.type, color = type)) + 
        geom_point(size = 2) + geom_line(aes(group = type), size = 1.25) + 
        geom_vline(aes(xintercept = year.slide), color = "red", linetype = "dashed") + theme_classic() + 
        labs(y = "", x = "") + ylim(0, 150) +
        theme(text = element_text(family = "Courier"),
              axis.text.x = element_text(angle = 60, hjust = 1),
              legend.title = element_blank(),
              legend.position = "none") +
        scale_color_manual(values = c("turquoise2", "steelblue1", "turquoise4"))
    
}

create.uses.per.year.plot <- function(year.slide){
    
    data <- data.main %>%
        mutate(year = as.numeric(year)) %>% 
        filter(!type == "samples") %>% 
        mutate(type = str_replace(type, "sampled", "Samples"),
               type = str_replace(type, "remix", "Remixes"),
               type = str_replace(type, "cover", "Covers")) %>% 
        mutate(type = as.factor(type)) %>% 
        mutate(type = factor(type, levels = c("Samples", "Covers", "Remixes"))) %>% 
        group_by(track, artist, year, type) %>% 
        distinct(track) %>%
        summarize(n = n())
    
    ggplot(data, aes(x = year, y = n, fill = type)) + 
        geom_bar(position = "stack", stat = "identity", color = "white", size = 0.25) + 
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        labs(y = NULL, x = NULL) + 
        theme(text = element_text(family = "Courier"),
              plot.title = element_text(hjust = 0.5, size = 23, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              plot.caption = element_text(hjust = 1, size = 9, face = "italic"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9),
              axis.title.y = element_text(size = 11),
              axis.text.x = element_text(size = 9),
              legend.position = "top") + ylim(0, 25) + 
        scale_y_continuous(expand = c(0,0.15), limits = c(0, 24)) +
        scale_fill_manual(values = c("turquoise2", "steelblue1", "turquoise4"), name = "",
                          breaks = c("Samples", "Covers", "Remixes")) +
        theme(plot.margin = unit(c(1,1,1,1.5), "cm")) +
        geom_rect(aes(xmin = year.slide - 0.5, xmax = year.slide + 0.5, ymin = 0, 
                      ymax = (data %>% filter(year == year.slide) %>% nrow())),
                  fill = NA, color = "red", size = 0.4) +
        geom_label(aes(label = paste0((data %>% filter(year == year.slide) %>% nrow()), 
                                      " use", if(year.slide != 2020){"s"}),
                       x = 2004, y = 20), 
                   size = 4, family = "Courier", 
                   color = "white", fill = "steelblue")
    
}

create.line.and.uses.patch <- function(year.slide){
 
    p1 <- create.type.summary.plot(year.slide = year.slide)
    p2 <- create.uses.line.plot(year.slide = year.slide)
    p3 <- create.uses.per.year.plot(year.slide = year.slide)

    p1 / (p2 + p3) / guide_area() + 
        plot_layout(guides = "collect", heights = unit(c(8, 12, 1), "null"))
    
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

## song analytics

create.song.comparison.analytics <- function(input1, input2){
    
    tracks <- c(input1, input2)
    filt.daft <- daft %>% 
        filter(track_name %in% tracks) %>% 
        mutate(track_name = as.factor(track_name)) %>% 
        mutate(track_name = factor(track_name, levels = c(input1, input2)))
    
    (ggplot(filt.daft, aes(x = track_name, y = energy)) + geom_col(aes(fill = track_name)) + 
            geom_hline(yintercept = c(0.1860, 0.9110)) + labs(title = "Energy", x = NULL,
                                                              y = NULL) +
            scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "grey", fill = NA),
                  text = element_text(family = "Courier"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 8),
                  legend.position = "bottom") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
            
            ggplot(filt.daft, aes(x = track_name, y = instrumentalness)) + geom_col(aes(fill = track_name)) + 
            geom_hline(yintercept = c(0, 0.95))  + labs(title = "Instrumentalness", x = NULL,
                                                        y = NULL) +
            scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "grey", fill = NA),
                  text = element_text(family = "Courier"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 8.2),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 8),
                  legend.position = "bottom") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
            
            ggplot(filt.daft, aes(x = track_name, y = liveness)) + geom_col(aes(fill = track_name)) + 
            geom_hline(yintercept = c(0.0293, 0.3580)) + labs(title = "Liveness", x = NULL,
                                                              y = NULL) +
            scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "grey", fill = NA),
                  text = element_text(family = "Courier"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 8),
                  legend.position = "bottom") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
            
            ggplot(filt.daft, aes(x = track_name, y = valence)) + geom_col(aes(fill = track_name)) + 
            geom_hline(yintercept = c(0.1190, 0.963)) + labs(title = "Valence", x = NULL,
                                                             y = NULL) +
            scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "grey", fill = NA),
                  text = element_text(family = "Courier"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 8),
                  legend.position = "bottom") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
            
            ggplot(filt.daft, aes(x = track_name, y = danceability)) + geom_col(aes(fill = track_name)) + 
            geom_hline(yintercept = c(0.491, 0.875)) + labs(title = "Danceability", x = NULL,
                                                            y = NULL) +
            scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "grey", fill = NA),
                  text = element_text(family = "Courier"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 9),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 8),
                  legend.position = "bottom") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)))) /  
        
        (ggplot(filt.daft, aes(x = track_name, y = tempo)) + geom_col(aes(fill = track_name)) + 
             geom_hline(yintercept = c(99.96, 140.88)) + labs(title = "Tempo", x = NULL,
                                                              y = NULL) +
             scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
             theme(panel.background = element_blank(),
                   panel.grid.major = element_line(colour = "lightgrey"),
                   panel.border = element_rect(color = "grey", fill = NA),
                   text = element_text(family = "Courier"),
                   axis.text.x = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 10),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size = 8),
                   legend.position = "bottom") +
             scale_y_continuous(expand = expansion(mult = c(0, 0.1))) |
             
             ggplot(filt.daft, aes(x = track_name, y = acousticness)) + geom_col(aes(fill = track_name)) + 
             geom_hline(yintercept = c(0.00135, 0.845)) + labs(title = "Acousticness", x = NULL,
                                                               y = NULL) +
             scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
             theme(panel.background = element_blank(),
                   panel.grid.major = element_line(colour = "lightgrey"),
                   panel.border = element_rect(color = "grey", fill = NA),
                   text = element_text(family = "Courier"),
                   axis.text.x = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 9),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size = 8),
                   legend.position = "bottom") +
             scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
             
             ggplot(filt.daft, aes(x = track_name, y = speechiness)) + geom_col(aes(fill = track_name)) + 
             geom_hline(yintercept = c(0.0321, 0.301)) + labs(title = "Speechiness", x = NULL,
                                                              y = NULL) +
             scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
             theme(panel.background = element_blank(),
                   panel.grid.major = element_line(colour = "lightgrey"),
                   panel.border = element_rect(color = "grey", fill = NA),
                   text = element_text(family = "Courier"),
                   axis.text.x = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 9),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size = 8),
                   legend.position = "bottom") +
             scale_y_continuous(expand = expansion(mult = c(0, 0.1))) |
             
             ggplot(filt.daft, aes(x = track_name, y = duration_ms)) + geom_col(aes(fill = track_name)) + 
             geom_hline(yintercept = c(104466, 600293)) + labs(title = "Duration", x = NULL,
                                                               y = NULL) +
             scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
             theme(panel.background = element_blank(),
                   panel.grid.major = element_line(colour = "lightgrey"),
                   panel.border = element_rect(color = "grey", fill = NA),
                   text = element_text(family = "Courier"),
                   axis.text.x = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 10),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size = 6),
                   legend.position = "bottom") +
             scale_y_continuous(expand = expansion(mult = c(0, 0.1))) | 
             
             ggplot(filt.daft, aes(x = track_name, y = total.uses)) + geom_col(aes(fill = track_name)) + 
             geom_hline(yintercept = c(14, 91)) + labs(title = "Total Uses", x = NULL,
                                                       y = NULL) +
             scale_fill_manual(values = c("turquoise2", "steelblue1"), name = NULL) +
             theme(panel.background = element_blank(),
                   panel.grid.major = element_line(colour = "lightgrey"),
                   panel.border = element_rect(color = "red", fill = NA),
                   text = element_text(family = "Courier"),
                   axis.text.x = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size = 8),
                   legend.position = "bottom") +
             scale_y_continuous(expand = expansion(mult = c(0, 0.1)))) /  
        guide_area() + 
        plot_layout(guides = "collect", heights = unit(c(6, 6, 1), "null")) 
    
}

energy.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = energy, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Energy") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = energy, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Energy") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
instrumentalness.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = instrumentalness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Instrumentalness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = instrumentalness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Instrumentalness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
liveness.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = liveness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Liveness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = liveness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Liveness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
valence.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = valence, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Valence") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = valence, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Valence") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
danceability.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = danceability, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Danceability") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = danceability, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Danceability") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
tempo.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = tempo, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Tempo") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = tempo, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Tempo") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
acousticness.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = acousticness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Acousticness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = acousticness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Acousticness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
speechiness.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = speechiness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Speechiness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = speechiness, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Speechiness") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}
duration.reg <- function(type){
    ifelse(type == "Linear", type <- "lm", 
           ifelse(type == "Loess", type <- "loess", "none"))
    if(type == "none"){
        ggplot(data = daft, aes(x = duration_ms, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Duration (ms)") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA))
    }else{
        ggplot(data = daft, aes(x = duration_ms, y = total.uses)) +     
            geom_point() + labs(y = "Total Uses", x = "Duration (ms)") + 
            theme(text = element_text(family = "Courier"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.border = element_rect(color = "black", fill = NA)) +
            geom_smooth(method = type, se = F, formula = y ~ x)
    }
}

create.reg.plot <- function(variable, type){
    if(variable == "Energy"){energy.reg(type)}else{
        if(variable == "Instrumentalness"){instrumentalness.reg(type)}else{
            if(variable == "Liveness"){liveness.reg(type)}else{
                if(variable == "Valence"){valence.reg(type)}else{
                    if(variable == "Danceability"){danceability.reg(type)}else{
                        if(variable == "Tempo"){tempo.reg(type)}else{
                            if(variable == "Acousticness"){acousticness.reg(type)}else{
                                if(variable == "Speechiness"){speechiness.reg(type)}else{
                                    if(variable == "Duration"){duration.reg(type)}}}}}}}}}
}

## repetition, etc.

repetition <- readRDS("scraped_data/discovery_repetition_index.RDS")

rep.graph.options <- c("Word Repetition (remove outlier)", 
                       "Word Repetition (with outlier)",
                       "Line Repetition (remove outlier)",
                       "Line Repetition (with outlier)")

create.rep.graph <- function(type, method){
    
    if(type == "Word Repetition (remove outlier)"){
        create.no.outlier.word.rep(method)
    }else{
        if(type == "Word Repetition (with outlier)"){
            create.outlier.word.rep(method)
        }else{if(type == "Line Repetition (remove outlier)"){
            create.no.outlier.line.rep(method)
        }else{
            create.outlier.line.rep(method)
        }
        }
    }
    
}

create.outlier.word.rep <- function(method){
    
    p1outl <- ggplot(repetition %>% 
                         mutate(song_name = str_replace(song_name, "Harder, Better, Faster, Stronger",
                                                        "Harder, Better,...")) %>% 
                         mutate(song_name = factor(song_name, levels = c("Digital Love", "Face to Face",
                                                                         "Something About Us", "One More Time",
                                                                         "Harder, Better,...", "Too Long", "Superheroes"))), 
                     aes(x = word_rep, y = song_name)) + geom_col(fill = "steelblue", color = "grey2") + 
        labs(x = "word repetition\n(total words / unique words)", y = "ordered songs") + theme_classic() + 
        scale_x_continuous(expand = c(0.01,0.15), limits = c(0, 99)) + 
        theme(text = element_text(family = "Courier"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_rect(color = "black", fill = NA)) +
        geom_text(aes(label = paste(" ", song_name, ":
 ", round(word_rep, digits = 2), sep = "")), hjust = 0, 
                  vjust = 0.5, size = 2.5, family = "Courier")
    p2out <- ggplot(repetition, aes(x = word_rep, y = total.uses)) + geom_point() + 
        scale_x_continuous(expand = c(0.005,0.15), limits = c(0, 85)) + theme_classic() +
        scale_y_continuous(expand = c(0.005,0.15), limits = c(0, 99)) + 
        labs(y = "total uses", x = "word repetition\n(total words / unique words)") + 
        theme(text = element_text(family = "Courier"),
              panel.grid.major = element_line(colour = "lightgrey"))
    
    p2outlm <- p2out + geom_smooth(method = "lm", formula = y ~ x, se = F)
    
    if(method == "Linear"){
        (p2outlm | p1outl) + plot_layout(widths = unit(c(12, 12), "null"))
    }else{
        (p2out | p1outl) + plot_layout(widths = unit(c(12, 12), "null")) 
    }
}

create.no.outlier.word.rep <- function(method){
    
    p1noutl <- ggplot(repetition %>% 
                          filter(song_name != "Superheroes") %>% 
                          mutate(song_name = str_replace(song_name, "Harder, Better, Faster, Stronger",
                                                         "Harder, Better,...")) %>% 
                          mutate(song_name = factor(song_name, levels = c("Digital Love", "Face to Face",
                                                                          "Something About Us", "One More Time",
                                                                          "Harder, Better,...", "Too Long"))), 
                      aes(x = word_rep, y = song_name)) + geom_col(fill = "steelblue", color = "grey2") + 
        labs(x = "word repetition\n(total words / unique words)", y = "ordered songs") + theme_classic() + 
        scale_x_continuous(expand = c(0.01,0.15), limits = c(0, 23)) + 
        theme(text = element_text(family = "Courier"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_rect(color = "black", fill = NA)) +
        geom_text(aes(label = paste(" ", song_name, ":
 ", round(word_rep, digits = 2), sep = "")), hjust = 0, 
                  vjust = 0.5, size = 2.5, family = "Courier")
    p2nout <- ggplot(repetition %>% 
                         filter(song_name != "Superheroes"), aes(x = word_rep, y = total.uses)) + 
        geom_point() + scale_x_continuous(expand = c(0.005,0.15), limits = c(0, 18)) + theme_classic() +
        scale_y_continuous(expand = c(0.005,0.15), limits = c(0, 99)) + 
        labs(y = "total uses", x = "word repetition\n(total words / unique words)") + 
        theme(text = element_text(family = "Courier"),
              panel.grid.major = element_line(colour = "lightgrey"))
    
    p2noutlm <- p2nout + geom_smooth(method = "lm", formula = y ~ x, se = F)
    
    if(method == "Linear"){
        (p2noutlm | p1noutl) + plot_layout(widths = unit(c(12, 12), "null"))
    }else{
        (p2nout | p1noutl) + plot_layout(widths = unit(c(12, 12), "null")) 
    }
}

create.no.outlier.line.rep <- function(method){
    
    line.p1noutl <- ggplot(repetition %>% 
                               filter(song_name != "Superheroes") %>% 
                               mutate(song_name = str_replace(song_name, "Harder, Better, Faster, Stronger",
                                                              "Harder, Better,...")) %>% 
                               mutate(song_name = factor(song_name, levels = c("Digital Love", "Something About Us", 
                                                                               "Face to Face", "Harder, Better,...",
                                                                               "One More Time", "Too Long"))), 
                           aes(x = line_rep, y = song_name)) + geom_col(fill = "steelblue", color = "grey2") + 
        labs(x = "line repetition\n(total lines / unique lines)", y = "ordered songs") + theme_classic() + 
        scale_x_continuous(expand = c(0.01,0.15), limits = c(0, 9)) + 
        theme(text = element_text(family = "Courier"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_rect(color = "black", fill = NA)) +
        geom_text(aes(label = paste(" ", song_name, ":
 ", round(line_rep, digits = 2), sep = "")), hjust = 0, 
                  vjust = 0.5, size = 2.5, family = "Courier")
    line.p2nout <- ggplot(repetition %>% 
                              filter(song_name != "Superheroes"), aes(x = line_rep, y = total.uses)) + 
        geom_point(alpha = 0.7) + scale_x_continuous(expand = c(0.005,0.15), limits = c(0, 7.5)) + theme_classic() +
        scale_y_continuous(expand = c(0.005,0.15), limits = c(0, 99)) + 
        labs(y = "total uses", x = "line repetition\n(total lines / unique lines)") + 
        theme(text = element_text(family = "Courier"),
              panel.grid.major = element_line(colour = "lightgrey")) +
        annotate("text", x = 1.18, y = 38, label = "(two points)", size = 2.25, family = "Courier")
    line.p2noutlm <- line.p2nout + geom_smooth(method = "lm", formula = y ~ x, se = F)
    
    if(method == "Linear"){
        (line.p2noutlm | line.p1noutl) + plot_layout(widths = unit(c(12, 12), "null"))
    }else{
        (line.p2nout | line.p1noutl) + plot_layout(widths = unit(c(12, 12), "null")) 
    }
}

create.outlier.line.rep <- function(method){
    
    line.p1outl <- ggplot(repetition %>% 
                              mutate(song_name = str_replace(song_name, "Harder, Better, Faster, Stronger",
                                                             "Harder, Better,...")) %>% 
                              mutate(song_name = factor(song_name, levels = c("Digital Love", "Something About Us", 
                                                                              "Face to Face", "Harder, Better,...",
                                                                              "One More Time", "Too Long",
                                                                              "Superheroes"))), 
                          aes(x = line_rep, y = song_name)) + geom_col(fill = "steelblue", color = "grey2") + 
        labs(x = "line repetition\n(total lines / unique lines)", y = "ordered songs") + theme_classic() + 
        scale_x_continuous(expand = c(0.01,0.15), limits = c(0, 100)) + 
        theme(text = element_text(family = "Courier"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_rect(color = "black", fill = NA)) +
        geom_text(aes(label = paste(" ", song_name, ":
 ", round(line_rep, digits = 2), sep = "")), hjust = 0, 
                  vjust = 0.5, size = 2.5, family = "Courier")
    line.p2out <- ggplot(repetition, aes(x = line_rep, y = total.uses)) + 
        geom_point(alpha = 0.7) + scale_x_continuous(expand = c(0.005,0.15), limits = c(0, 80)) + theme_classic() +
        scale_y_continuous(expand = c(0.005,0.15), limits = c(0, 99)) + 
        labs(y = "total uses", x = "line repetition\n(total lines / unique lines)") + 
        theme(text = element_text(family = "Courier"),
              panel.grid.major = element_line(colour = "lightgrey")) +
        annotate("text", x = 9, y = 39, label = "(two points)", size = 1.9, family = "Courier")
    line.p2outlm <- line.p2out + geom_smooth(method = "lm", formula = y ~ x, se = F)
    
    if(method == "Linear"){
        (line.p2outlm | line.p1outl) + plot_layout(widths = unit(c(12, 12), "null"))
    }else{
        (line.p2out | line.p1outl) + plot_layout(widths = unit(c(12, 12), "null")) 
    }
}

## google trends

daft_punk_time_search <- readRDS("scraped_data/daft_punk_time_search.RDS")
peaks.list <- readRDS("scraped_data/peaks_list.RDS")

create.google.trends.plot <- function(peak.selection){
    
    peak.selection.year <- if(peak.selection == "March 2005"){"2005"}else{
        if(peak.selection == "December 2007"){"2007"}else{
            if(peak.selection == "December 2010"){"2010"}else{
                if(peak.selection == "May 2013"){"2013"}else{
                    if(peak.selection == "January 2014"){"2014"}else{
                        if(peak.selection == "February 2017"){"2017"}}}}}}
    
    peaks.list <- peaks.list %>% 
        mutate(selected = map_chr(date, 
                                  ~ if(as.character(format(.,'%Y')) == peak.selection.year){
                                      "red"
                                  }else{
                                      "black"
                                  }))
    
    terms.needed <- unlist(peaks.list %>% 
                               mutate(selected = map_chr(date, 
                                                         ~ if(as.character(format(.,'%Y')) == peak.selection.year){
                                                             "red"
                                                         }else{
                                                             "black"
                                                         })) %>% 
                               filter(selected == "red") %>% 
                               pull(terms))
    terms.needed <- paste(terms.needed[1], terms.needed[2], terms.needed[3], 
                          terms.needed[4], terms.needed[5], sep = "
")
    
    p <- ggplot(data = daft_punk_time_search, aes(x = date, y = hits))+
        geom_line(col = "grey") + xlab(NULL) + ylab("Relative Interest") + theme_bw() +
        geom_area(fill = "steelblue1") + theme_classic() + 
        labs(title = "'Daft Punk' Google Search Volume from 2004 to 2020",
             subtitle = "with top search terms shown for graph peaks") +
        theme(text = element_text(family = "Courier"),
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 12),
              plot.caption = element_text(hjust = 1, size = 9, face = "italic"),
              legend.title = element_text(size = 10)) + 
        geom_point(data = peaks.list, aes(x = date, y = hits, color = selected, shape = selected), 
                   size = 2.5) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("black", "red")) + 
        scale_y_continuous(expand = c(0,5), limits = c(0, 105)) + 
        annotate("text", x = ymd("2006-09-01"), y = 98, label = paste0("Top Google Searches"), 
                 family = "Courier", size = 3) + 
        annotate("label", x = ymd("2006-09-01"), y = 88, label = paste0(peak.selection, ":"), 
                 family = "Courier", padding.size = 0.5, label.padding = unit(0.5, "lines"), 
                 color = "white", fill = "steelblue", size = 4, fontface = "bold") + 
        annotate("label", x = ymd("2006-09-01"), y = 65, label = paste0(terms.needed), 
                 family = "Courier", size = 3)
    p
}

peak.months <- c("March 2005", "December 2007", "December 2010", 
                 "May 2013", "January 2014", "February 2017")

create.alt.uses.per.year.plot <- function(peak.selection){
    
    peak.selection.year <- if(peak.selection == "March 2005"){"2005"}else{
        if(peak.selection == "December 2007"){"2007"}else{
            if(peak.selection == "December 2010"){"2010"}else{
                if(peak.selection == "May 2013"){"2013"}else{
                    if(peak.selection == "January 2014"){"2014"}else{
                        if(peak.selection == "February 2017"){"2017"}}}}}}
    
    year.peak <- as.numeric(peak.selection.year)
    
    data <- data.main %>%
        mutate(year = as.numeric(year)) %>% 
        filter(!type == "samples") %>% 
        mutate(type = str_replace(type, "sampled", "Samples"),
               type = str_replace(type, "remix", "Remixes"),
               type = str_replace(type, "cover", "Covers")) %>% 
        mutate(type = as.factor(type)) %>% 
        mutate(type = factor(type, levels = c("Samples", "Covers", "Remixes"))) %>% 
        group_by(track, artist, year, type) %>% 
        distinct(track) %>%
        summarize(n = n()) %>% 
        filter(year >= 2004)
    
    ggplot(data, aes(x = year, y = n, fill = type)) + 
        geom_bar(position = "stack", stat = "identity", color = "white", size = 0.25) + 
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        labs(title = "'Discovery' Uses by Year from 2004 to 2020", y = NULL, x = NULL,
             subtitle = "red highlight indicates selected year, not month") + 
        theme(text = element_text(family = "Courier"),
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 12),
              plot.caption = element_text(hjust = 1, size = 9, face = "italic"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9),
              axis.title.y = element_text(size = 11),
              axis.text.x = element_text(size = 9),
              legend.position = "bottom") + 
        scale_y_continuous(expand = c(0,0.15), limits = c(0, 24)) +
        scale_fill_manual(values = c("turquoise2", "steelblue1", "turquoise4"), name = NULL,
                          breaks = c("Samples", "Covers", "Remixes")) +
        theme(plot.margin = unit(c(1,1,1,1.5), "cm")) +
        geom_rect(aes(xmin = year.peak - 0.5, xmax = year.peak + 0.5, ymin = 0, 
                      ymax = (data %>% filter(year == year.peak) %>% nrow())),
                  fill = NA, color = "red", size = 0.4)
    
}

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
            menuItem("About the Project", tabName = "about", icon = icon("address-card")),
            menuItem("Samples and Uses", tabName = "influence", icon = icon("th")),
            menuItem("Potential Connections", tabName = "connections", icon = icon("th"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "about",
                fluidRow(column(12, align = "center",
                                box(width = 12, h2("About the Project"),
                                    p("Why does a 20-year-old electronic dance music album still matter today? And why do samples, remixes, and covers matter in examining it?"))
                                    
                    )
                ),
                fluidRow(column(12,
                    box(width = 12,
                        h2("Background"),
                        p("Thomas Bangalter and Guy-Manuel de Homen-Christo met in secondary school, recording electronic demos in the late 1980s. With a third friend, they formed a band", "\"Darlin’,\"", "after the Beach Boys song of the same name.", "\"Darlin’\"", "was a short-lived experiment, and a British music magazine described their music as", "\"daft punky.\"", "When Bangalter and Homen-Christo returned to the electronic music scene in the mid-90s, they released their debut album", em("Homework,"), "under the name Daft Punk.", em("Homework"), "was an instant classic, aweing critics and fans alike with its ability to balance incredible innovation and addictive listenability; few predecessors in dance music could so naturally spin club hits out of dissected Billy Joel horn notes, cryptic autotuned lyrics, and drum loops.", em("Homework"), "ranks high among most critics’ lists of the best 90s albums, but it wasn’t until 2001 when Daft Punk would release their definitive work:", em("Discovery.")),
                        p("I don’t play an instrument, much less understand how to use a soundboard or a sampler. Truthfully, I don’t even like dance music that much. But the brilliance of", em("Discovery"), "is far from esoteric; when I first heard the album, I was blown away by the creativity, energy, and intelligence of every track. Reading further into the album’s story (for a quick summary, watch", a("this", href = "https://pitchfork.com/tv/15-docs/discovery-when-daft-punk-became-robots/"), "video), I grew more interested in the album’s relationship with sampling.", em("Discovery"), "is often cited as massively influential on modern dance music, and samples, covers, and remixes may be a good way to measure substantive influence, not just popularity. To visualize its influence, I wanted to explore uses of the album’s fourteen songs from the release of the first single, \"One More Time,\" in 2000, to early 2020 when I began this project."),
                        p("Additionally, my project touches on another fundamental question of popular music. What measurable qualities make a song, or an album, influential? While finding a definitive answer, or proving that the answer is nonexistent, may be impossible, I attempted to display how certain qualities of a song, both music and nonmusical, are or are not correlated with \"influence\" shown through uses."),
                        h2("The Data"),
                        p("The data for this project came from a multitude of sources, some accessible and others unintentionally hostile to data scientists."),
                        p("In the latter category was", a("whosampled.com", href = "https://www.whosampled.com"), "with no existing API. Because of this, I had to resort to creating various functions taking advantage of the site’s CSS selectors and methodical URLs. This is an awesome website for music nerds and casual listeners alike, being the largest database for samples, remixes, and covers of all genres of music."), 
                        p("I also took advantage of the Internet’s largest music database", a("Discogs", href = "https://www.discogs.com"), "using an R package to gather data on country, genre, and style of songs in the sample data."),
                        p("To gather and examine song lyrics, I used the", a("Genius", href = "https://genius.com"), "database.", a("Spotify", href = "https://www.spotify.com/uk/"), "provided me with music analytical data (variables like danceability and valence)."),
                        p("Finally, I toyed with", a("Google Trends", href = "https://trends.google.com/trends/?geo=US"), "data to look at changes in Internet popularity over time; unfortunately, 'plotly' graphics were incompatible with shiny."),
                        p("The code for this project is public on my", a("GitHub", href = "https://github.com/lukekolar/final-project-data-scraping"), "page."),
                        
                        h2("About Me"),
                        p("My name is Luke Kolar, and I’m currently an undergraduate student at Harvard University studying Government and Statistics."), 
                        p("You can reach me via email:", a("lukekolar@college.harvard.edu", href = "mailto: lukekolar@college.harvard.edu"))
                        )
                        )
                ),
                fluidRow(column(12, align = "center",
                                imageOutput("intro.gif")     
                         )
                )  
            ),
            tabItem(tabName = "influence",
                fluidRow(column(12, align = "center",
                                    box(width = 12, h2("Samples and Uses"),
                                        p("This page demonstrates the musical influence of all 14 tracks on", em("Discovery"), "as represented by samples, remixes, and covers by other artists, as well as samples of samples..."))
                                    
                    )
                ),
                fluidRow(column(width = 12,
                    box(sliderInput("year.slide2", label = NULL, min = 2000, max = 2020,
                                    sep = '', ticks = 21, value = 2020),
                        plotOutput("plot.tab1"),
                        selectInput("plot.choice.tab1", "Show...", 
                                    choices = c("Cumulative Uses by Year",
                                                "Use Type Distributions by Song and Year"))),
                    
                    box(sliderInput("year.slide", label = NULL, min = 2000, max = 2020,
                                    sep = '', ticks = 21, value = 2020),
                        plotOutput("line.and.uses.patch"),
                        helpText("Shows total unique uses by type, excluding repeats.")),
                    padding = 15
                    )
                ),
                fluidRow(column(width = 12,
                    box(plotOutput("HBFS.network"),
                        selectInput("net.track", "Show path to...", 
                                    choices = HBFS.tracks),
                                    helpText("Tracks that sample 'Harder, Better,...', or tracks sampling those tracks.")),
                    box(plotOutput("tree.plot"),
                        selectInput("tree.choice", "Show samples of...", 
                                    choices = tree.tracklist),
                        helpText("Covers and remixes are not shown - only samples.")),
                    padding = 15
                    )
                )),
            
            tabItem(tabName = "connections",
                    fluidRow(column(12, align = "center",
                                    box(width = 12, h2("Potential Connections"),
                                        p("This page examines several distinctions between the 14 tracks on", em("Discovery"), "and potential correlation between them and our influence measurement; namely, total uses."))
                                    )
                    ),
                    fluidRow(
                        box(plotOutput("comparing.analytics"),
                            selectInput("song.info.choice1", "Compare:", 
                                        choices = all.14.tracks, selected = "One More Time"),
                            selectInput("song.info.choice2", NULL, 
                                        choices = all.14.tracks, selected = "Nightvision"),
                        ),
                        box(
                            h2("What's going on here?"),
                            p(" "),
                            p("Can certain objective features of a song explain its influence? In the first two left-side panels, I explore how certain song qualities differ between songs, and whether they have an impact on a song's influence as represented by total uses. The variables, explained in the", a("Spotify Web API Developer Guide,", href = "https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-audio-features/"), "offer interesting insight into the songs themselves, but none of them show any significant correlation with total uses."),
                            p("Below this box, I examine lyrical repetition's relationship with total uses. Repetition is common in electronic dance music, both instrumentally and lyrically, but the latter is already somewhat represented for its impact on the aforementioned music variables, like danceability. Only seven songs contain lyrics, and one of these, \"Superheroes,\" can be classified as an outlier, lyrically consisting of the repeated line, \"Something's in the air...,\" seventy-two times. However, again, even without this outlier, lyrical repetition shows no significant correlation with influence, or total uses."),
                            p("The data I gathered seems to tell us little to nothing about each song's influence (I discuss this in further depth below), but it's still intriguing to toy around with. The goal of this project was also to visualize", em("Discovery"), "as a work, and seeing its range in instrumentalness or acousticness, the variety of lyrical approaches, and more still allows one to appreciate and further understand the record.")
                        )
                    ),
                    fluidRow(
                        box(plotOutput("reg.plot.details"),
                            selectInput("reg.variable", "Choose Musical Variable:",
                                        choices = c("Energy", "Instrumentalness", "Liveness", "Valence", "Danceability", "Tempo", "Acousticness", "Speechiness", "Duration")),
                            selectInput("reg.type", "Regression:",
                                        choices = c("Linear", "Loess", "None"))
                        ),
                        box(plotOutput("rep.plot.choice"),
                            selectInput("rep.plot.pick", "Choose Lyric Repetition Visual:",
                                        choices = rep.graph.options),
                            selectInput("rep.reg.pick", label = "Regression",
                                        choices = c("Linear", "None"))
                            
                        )
                    ),
                    fluidRow(column(12,
                              box(width = 12, selectInput("month.select", "Choose peak:",
                                                          choices = peak.months),
                                  plotOutput("google.trends.plot.and.alt")
                             ))
                    ),
                    fluidRow(column(12,
                                    box(width = 12, 
                                        h2("Conclusion"),
                                        p("Maybe the extent of a song's influence is in part due to chance; \"One More Time\" is widely considered the most influential track on", em("Discovery"), "in part due to its mass appeal, though I've always considered \"Face to Face\" and \"Digital Love\" to be more impressive tracks. Another possibility, and a much more likely one, relates to the fact that my data is in itself arbitrary, and perhaps there's an answer in data science somewhere else. The subjectivity versus objectivity debate about art is an ancient one, but considering the formulaic nature of modern dance and electronic music, this may be the genre which settles the debate someday. But Thomas Bangalter and Guy-Manuel de Homen-Christo never meant to solve an age-old philosophical puzzle; they just wanted to make catchy songs. In that respect, they triumphed. Happy listening.")
                             ))
                    )
                    
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$intro.gif <- renderImage({
        
        list(src = "./misc/daft.gif")
        
    }, deleteFile = FALSE)
    
    output$ranks.graph.title <- renderText({
        paste0("Total Uses by Song (year: ", input$year.slide, ")")
    })
    output$plot.tab1 <- renderPlot({
        if(input$plot.choice.tab1 == "Use Type Distributions by Song and Year"){
            create.pie.plots(input$year.slide2)
        }else{
            create.non.ranked.plot(input$year.slide2)
        }
        
    })
    output$line.and.uses.patch <- renderPlot({
        create.line.and.uses.patch(input$year.slide)
    })
    output$uses.line.plot <- renderPlot({
        create.uses.line.plot(input$year.slide)
    })
    output$HBFS.network <- renderPlot({
        create.sample.net.plot(songg1 = "Harder, Better, Faster, Stronger",
                               songPath = input$net.track)
    })
    output$tree.plot <- renderPlot({
        create.sample.tree.plot(songg2 = input$tree.choice)
    })
    
    output$comparing.analytics <- renderPlot({
        create.song.comparison.analytics(input1 = input$song.info.choice1, input2 = input$song.info.choice2)
    })
    output$reg.plot.details <- renderPlot({
        create.reg.plot(type = input$reg.type, variable = input$reg.variable)
    })
    output$google.trends.plot.and.alt <- renderPlot({
        create.google.trends.plot(input$month.select) + create.alt.uses.per.year.plot(input$month.select)
    })
    output$rep.plot.choice <- renderPlot({
        create.rep.graph(type = input$rep.plot.pick, method = input$rep.reg.pick)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
