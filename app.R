library(readr)
library(shiny)
library(ggplot2)
library(dplyr)
library(ggimage)
library(reactable)
library(shinythemes)
library(scico)
library(waiter)
library(forcats)

# in
court_points <- read_csv("data/bballcourt.csv")
Ostats <- read_csv("data/Ostats20.csv") %>% as.matrix()
row.names(Ostats) <- colnames(Ostats)
shotsdf <- read_csv("data/shotsdf20.csv")
thumbs <- read_csv("data/thumbs.csv")


shotchartr <- function(player1){
    player1_shots <- shotsdf %>% filter(namePlayer==player1) %>% 
        mutate(face="focal")
    player1_img <- thumbs %>% filter(namePlayer==player1) %>% 
        mutate(x=177,y=365) %>% slice(1)
    Ostats[player1,] %>% tibble::enframe() %>% 
        top_n(3,value)  %>%
        mutate(value=round(value,4)) %>%
        rename(Player=1,Overlap=2) %>% 
        arrange(desc(Overlap),Player) ->top3_Ostats
    top3_names <- top3_Ostats$Player
    top3_shots <- shotsdf %>% filter(namePlayer %in% top3_names) %>% 
        mutate(face="compare") %>% left_join(top3_Ostats,by=c("namePlayer"="Player"))
    top3_imgs <- thumbs %>% filter(namePlayer %in% top3_names) %>% 
        mutate(x=177,y=364) %>% left_join(top3_Ostats,by=c("namePlayer"="Player"))
    list(top3_Ostats,player1_shots,top3_shots,player1_img,top3_imgs)
}

draw_court <- 
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = "black")
imgline <- tibble::tibble(y=c(316,316),x=c(110,250))
underline_imgs <- 
    geom_path(data = imgline,
              aes(x,y),color="black")


ui <- fluidPage(theme=shinytheme("cosmo"),
                use_waiter(),
                br(),
                fluidRow(
                    column(11,
                           titlePanel("Using point-proximity-overlap statistics to find similar shooting preferences",
                                      windowTitle="NBA shot preference overlap")
                    ),
                    column(1,
                           actionButton("github",
                                        label = "View Code",
                                        width = "95px",
                                        onclick ="window.open(`https://github.com/luisDVA/nba-overlap-app`, '_blank')",
                                        style="color: #fff; background-color: #4c9ed9; border-color: black"),
                           style="position:absolute;right:4em")
                ),
                fluidRow(
                    column(6,h2("Luis D. Verde Arregoitia"),offset = 3),
                    column(3,h4("Twitter -",a(href="https://twitter.com/LuisDVerde","@LuisDVerde"))),
                    column(3,h4("Website -",a(href="https://www.liomys.mx","liomys.mx")))
                ),
                br(),
                br(),
                p("The point-proximity metric",em("O"),"measures patterns of co-aggregation by comparing nearest-neighbor distances within and between two sets of XY coordinates. 
    The value of",em("O")," is bounded between 0 and 1. Values close to zero indicate little spatial overlap, a value of ~0.5 is expected if the occurrence points of the two samples are randomly and independently distributed across the same area, and values > 0.5 indicate spatial clustering between the two samples."),
                br(),
                h3("Select a player in the input box below"),
                h5("This app needs an internet connection to retrieve player photos"),
                sidebarPanel(width = 3,
                             selectInput(inputId = "player_name1",
                                         label = "Player",
                                         choices = c("enter a player..."="",sort(colnames(Ostats))),
                                         selected = "",selectize = TRUE),
                             p("How it works:"),
                             img(src="sideimg.png",width="100%")),
                mainPanel(width = 9,
                          fluidRow(
                              column(6,plotOutput("shotts1"),align="center"),
                              column(4,uiOutput("ovals"),offset = 0),
                              column(2)),
                          em("Ties are sorted alphabetically"),
                          plotOutput("shotts3")
                ),
                br(),
                hr(),
                br(),
                h4("Further reading"),
                p(a(href="https://luisdva.github.io/rstats/nba-overlap/","Point proximity overlap for NBA shot chart data by Luis Verde Arregoitia")),
                p(a(href="https://toddwschneider.com/posts/ballr-interactive-nba-shot-charts-with-r-and-shiny/","BallR web app by Todd Schneider")),
                p(a(href="http://www.macroevoeco.com/uploads/3/9/1/8/39186089/geb_2016__cardillo___warren_.pdf","Cardillo and Warren (2016) Analysing patterns of spatial and nicheoverlap among species at multipleresolutions, Global Ecology and Biogeography")),
                waiter_show_on_load(color = "white",html = spin_3k())
)



server <- function(input, output){
    W1 <- Waiter$new(color = "white",id = c("shotts1","shotts3"),html=spin_3k())
    observeEvent(input$player_name1,{
        req(input$player_name1)
        players_xy_out <- shotchartr(input$player_name1)
        W1$show()
        
        output$shotts1 <- renderPlot({
            ggplot(players_xy_out[[2]], aes(x=X, y=Y)) + 
                coord_equal()+
                geom_point(size=4,color="black",
                           aes(fill=num_shots),shape=25,position = "jitter") +
                scale_fill_scico(palette = "imola",guide=FALSE,direction = -1)+
                guides(fill=guide_colorbar(barheight  = 0.6,barwidth = 8,
                                           ticks = FALSE,direction = "horizontal",
                                           title="shots",title.position = "top",
                                           label.theme = element_text(size = 8),
                                           title.hjust = 0.5,title.theme = element_text(size = 11)))+
                geom_image(data=players_xy_out[[4]],aes(x=x,y=y,image=urlPlayerThumbnail),size=0.2)+
                draw_court+labs(x="",y="",title = paste(players_xy_out[[2]]$namePlayer))+
                underline_imgs+
                theme(text = element_text(size = 18,family = "NanumGothic"),
                      panel.grid = element_blank(),
                      plot.title = element_text(hjust = 0.5),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      plot.caption = element_text(color="white"),
                      strip.background = element_blank(),
            legend.position = c(0.23,0.85),
            legend.background = element_rect(fill="transparent"))
        })
        
        output$ovals <- 
            renderUI({
                div(h2("Similar shooting patterns"),
                    em("Data spans 1997-2020 seasons"),
                    p("Players with highest values of", em("O")," relative to focal player"),
                    reactable(players_xy_out[[1]],
                              defaultColDef = colDef(
                                  headerStyle = list(background = "#255f85", color = "white")),
                              bordered = TRUE,compact = TRUE,
                              striped = TRUE))
            })
        
        
        output$shotts3 <- renderPlot({
            ggplot(players_xy_out[[3]], aes(x=X, y=Y)) + 
                coord_equal()+
                geom_point(size=4,color="black",
                           aes(fill=num_shots),shape=21,position = "jitter") +
                scale_fill_scico(palette = "lajolla",direction = 1,guide=FALSE)+
                guides(fill=guide_colorbar(barheight  = 0.6,barwidth = 10,
                                           ticks = FALSE,direction = "horizontal",
                                           title="shots",title.position = "top",
                                           label.theme = element_text(size = 8),
                                           title.hjust = 0.5,title.theme = element_text(size = 11)))+
                geom_image(data=players_xy_out[[5]],aes(x=x,y=y,image=urlPlayerThumbnail),size=0.2)+
                draw_court+underline_imgs+
                labs(x="",y="")+
                facet_grid(~fct_reorder(namePlayer,Overlap,.desc=TRUE))+
                theme(text = element_text(size = 18,family = "NanumGothic"),
                      panel.grid = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      plot.caption = element_text(color="white"),
                      strip.background = element_blank(),
                      legend.position = "bottom",
                      legend.background = element_rect(fill="transparent"))  
        })    
    })
    waiter_hide()
}

shinyApp(ui, server)



