

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(readr)
library(htmltools)
library(leaflet)
library(rgdal)
library(raster)


###########loading dataset##########
wq<-read_csv("./www/wq.csv")
var1<-unique(wq$Round)
cystem<-unique(wq$System)


###########loading shapefiles#######
labs<-read_csv("./www/labs_region.csv")
ghana<-rmapshaper::ms_simplify(readOGR("./www/Shapefile.shp"))
labshp<-readOGR("./www/labs.shp")
##############ui module##########
uimodule<-function(id){
  ns<-NS(id)
  sidebarLayout(
    sidebarPanel(width = 4,
                 h2(tags$b("Overview")),
                 h3(tags$p("This app displays water quality test results for systems enrolled in the Assurance Fund in Asutifi North, Ghana.")), 
                 h3(tags$p("The program was launched in March 2020 with 10 water systems. Water samples are tested once a month.")),
                 h3(tags$p("The results tab displays the summary plots, and the data tab displays the raw data.")),
                 h3(tags$p("The map displays the regional water quality testing laboratories for each region and the dynamic area of coverage."))
    ),
    
    mainPanel(column(8,
                     fluidRow(
                       column(6,
                              selectInput(ns("round"),
                                          label = h4("Select results round"),
                                          choices = c("All",var1))
                       ),
                       column(6,
                              selectInput(ns("system"),
                                          label = h4("Select system"),
                                          choices = c("All",cystem))
                       )
                     ),
                     tabsetPanel(
                       tabPanel(h3("Results"),
                                br(),
                                fluidRow(
                                  tabBox(title = "",
                                         width = 12,
                                         tabPanel(title = h4("Count plots"),
                                                  h4(tags$p("The plot shows the E.coli contamination levels count by water system and results round.")),
                                                  plotOutput(ns("result"),
                                                             height = 500,width = "100%")),
                                         tabPanel(title = h4("Percentage plots"),
                                                  h4(tags$p("The plot shows the E.coli contamination levels percent by water system and results round.")),
                                                  plotOutput(ns("perc"),
                                                             height = 500,width = "100%")),
                                         tabPanel(title = h4("General plots"), 
                                                  h4(tags$p("The plot shows the average E.coli contamination levels by water system for all the rounds.")),
                                                  plotOutput(ns("gen"),
                                                             height = 400,width = "100%"))
                                  )
                                )
                                
                                # fluidRow(
                                #    plotlyOutput(ns("result"),height = 500,width = "100%")
                                #          )
                                
                                
                       ),
                       
                       tabPanel(h3("Data"),
                                br(),
                                
                                fluidRow(
                                  column(12,
                                         dataTableOutput(ns("data"))    
                                  )  
                                )
                       ),
                       
                       tabPanel(h3("Map"),
                                fluidRow(
                                  column(12,
                                         h4(tags$p("The map shows the coverage area for each regional lab in metres.")),
                                         numericInput(ns("dist"),label = h4("Input the radius coverage(Metres)"),value = 150000),
                                         leafletOutput(ns("map"),height = 500,width = "100%")  
                                         
                                  )
                                  
                                )
                                
                       )
                       
                     )
    )
    
    )
  )# end sidebar layout
}



###################server module#########################
servermodule<-function(id){
  moduleServer(id, function(input, output, session){
    
    #####system type reactive function#######
    sys_react<-reactive({
      if(input$system!="All" ){
        wq %>% filter(System==input$system)
      }
      else{
        wq
      }
    }) #end
    
    
    ##########round reactive function########
    wq_react<-reactive({
      if(input$round!="All"){
        sys_react() %>% filter(Round==input$round)
      }
      else{
        sys_react()
      }
    }) #end  
    
    ########reactive function for area coverage######
    bufer<-reactive({
      buffer(labshp,width=input$dist,dissolve=FALSE)
    })
    
    
    ###########count plot##########
    output$result<-renderPlot({
      wq_react() %>% count(WHO_Risk_Level,System,Round) %>%
        ggplot(aes(x=1,y=n,fill=WHO_Risk_Level))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = c(0,2,4,6,8,10))+
        facet_wrap(~Round)+
        labs(
          y="Water system count",
          fill="WHO Risk Level"
          
        )+
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size=16),
          axis.text.y = element_text(size=14),
          strip.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)
        )  
      
    })#end
    
    
    ##############percentage plot##########
    # output$perc<-renderPlotly({
    #     ggplotly(
    # 
    #         ggplot(data = wq_react(),aes(x=WHO_Risk_Level,group=1))+
    #             geom_bar(aes(y=..prop..,fill=factor(..x..)),stat ="count")+
    #             geom_text(aes( label = scales::percent(..prop..),
    #                            y= ..prop.. ), stat= "count", vjust =0.5) +
    #             labs(y="Percentage")+
    #             scale_y_continuous(labels = scales::percent_format())+
    #             facet_wrap(~Round)+
    #             theme(
    #                 axis.title.x = element_blank(),
    #                 axis.text.x = element_text(angle = 45),
    #                 #axis.text.y = element_text(size = 7),
    #                 #axis.title.y =element_text(size=7,face = "bold"),
    #                 axis.ticks.y = element_blank()
    #             )
    #     ) %>% layout(showlegend=FALSE)
    # 
    # })#end
    
    output$perc<-renderPlot({
      
      wq_react() %>% count(WHO_Risk_Level,System,Round) %>%
        #mutate(percentage=100*n/sum(n)) %>%
        ggplot(aes(x=1,y=10*n,fill=WHO_Risk_Level))+
        geom_bar(stat ="identity")+
        scale_y_continuous(breaks = c(0,20,40,60,80,100))+
        labs(
          y="Percentage of water system",
          fill="WHO Risk Level",
          caption = "Sample(n) = 10"
        )+
        #scale_y_continuous(labels = scales::percent_format())+
        facet_wrap(~Round)+
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size=16),
          axis.text.y = element_text(size=14),
          strip.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 14)
        )
      
    })
    
    
    ###############general plot###########
    # output$gen<-renderPlotly({
    #     ggplotly(
    #         ggplot(data = sys_react(),aes(x=WHO_Risk_Level,group=1))+
    #             geom_bar(aes(y=..prop..,fill=factor(..x..)),stat ="count")+
    #             geom_text(aes( label = scales::percent(..prop..),
    #                            y= ..prop.. ), stat= "count", vjust =0.5) +
    #             labs(y="Average source type percentage",x="WHO Risk Level")+
    #             scale_y_continuous(labels = scales::percent_format())+
    #             theme(
    #                 axis.title.x = element_blank()
    #             )
    #     ) %>% layout(showlegend=FALSE)
    # 
    # })#end
    
    output$gen<-renderPlot({
      sys_react() %>% count(WHO_Risk_Level,System) %>%
        mutate(percentage=round(100*n/sum(n)),0) %>%
        ggplot(aes(x=1,y=percentage,fill=WHO_Risk_Level))+
        geom_bar(stat ="identity")+
        scale_y_continuous(breaks = c(0,20,40,60,80,100))+
        coord_flip()+
        labs(
          y="Percentage of water system",
          fill="WHO Risk Level",
          caption = "Sample(n) = 10"
        )+
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 14)
          # legend.direction ="horizontal",
          # legend.position = "bottom"
        )
    })#end
    
    ###############table output##########
    output$data<-renderDataTable({
      DT::datatable(wq_react(), filter= "top")
    })
    
    
    ############map output###########
    output$map<-renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = ghana,
          fill = F,
          color = "blue",
          weight = 3
        )%>%
        addCircleMarkers(
          data=labs,
          fillColor = "red",
          fillOpacity = 1.0,
          radius = 8,
          stroke = FALSE,
          lat = ~lat,
          lng = ~lon,
          label = paste0(labs$`Treatment Plant`)
        ) %>%
        addPolylines(
          data = bufer(),
          color = "red",
          weight = 2.5
        )
      
    }
    
    ) #end of basemap
    
    
    #######observe event for the basemap###########
    # observe({
    # 
    #     leafletProxy("map") %>%
    #         clearShapes() %>%
    #         addPolygons(
    #             data = ghana,
    #             fill = F,
    #             color = "blue",
    #             weight = 3
    #         )%>%
    #         addCircleMarkers(
    #             data=labs,
    #             fillColor = "red",
    #             fillOpacity = 1.0,
    #             radius = 8,
    #             stroke = FALSE,
    #             lat = ~lat,
    #             lng = ~lon,
    #             label = paste0(labs$`Treatment Plant`)
    #         ) %>%
    #         addPolylines(
    #             data = bufer(),
    #             color = "red",
    #             weight = 2.5
    #         )
    # 
    # }
    # )
    
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("WATER QUALITY TEST RESULTS"),
  uimodule("wqresults")
  
)# end fluidpage

# Define server logic required to draw a histogram
server <- function(input, output) {
  servermodule("wqresults")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
