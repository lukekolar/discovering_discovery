#### data test

library(tidyverse)
library(RColorBrewer)

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

#### shiny app

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "Discovering \n'Discovery'",
                    titleWidth = 300),
    dashboardSidebar(
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
                fluidRow(
                    box(plotOutput("ranks.graph")),
                    
                    box(
                        title = "Total Uses by Song and Year:",
                        sliderInput("year.slide", label = NULL, min = 2000, max = 2020,
                                    sep = '', ticks = 21, value = 2020)
                        )
                )
            ),
            
            tabItem(tabName = "lyrics",
                    h2("Coming soon...")),
            tabItem(tabName = "popularity",
                    h2("Coming soon..."))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ranks.graph <- renderPlot({
        create_ranks.graph(input$year.slide)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
