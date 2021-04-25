library(mapview)
library(tigris)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(dplyr)
library(DT)
library(shiny)
library(shinydashboard)
library(viridis)

cook <- blocks(state = "IL",county="Cook", year = 2010)
cook_tracts <- tracts(state= "IL",county = "Cook", year = 2010)

#carSpeeds <- read.csv(file = 'data/car-speeds.csv')
data <- read_excel("energy_usage_2010.xlsx")
data$GEOID10 <- as.character(data$GEOID10)
data$TRACTCE10 <- as.character(data$TRACTCE10)

#str(data)


chi <- merge(x=cook, y = data, by = "GEOID10", all = FALSE)
chi_tracts <- merge(x = cook_tracts, y = data, by ="TRACTCE10", all= FALSE)









#west = ggplot() + geom_polygon(data = nws, aes(x = INPTLON10, y = INPTLAT10, group = GEOID10, fill = TOTAL_KWH), color = "black") + coord_fixed(1.2)+  
#  scale_fill_distiller(palette = "Blues") +
#  labs(fill = "TOTAL_KWH")


################# START UI #################
ui <- fluidPage(
  
  navbarPage("Project 3: We've Got the Power",
             
             
             ################# NEAR WEST SIDE PANEL #################
             
                        tabPanel("Near West Side",

                                 fixedRow(
                                   column(2,style='padding-top:50px;',
                                          selectInput("selectKWH", "Select KWH Total or Monthly:",
                                                      c("None" = "NONE","Total KWH" = "TOTAL_KWH","January" = "KWH_JAN","Feburuary" = "KWH_FEB","March" = "KWH_MAR",
                                                        "April" = "KWH_APR","May" = "KWH_MAY","June" = "KWH_JUN","July" = "KWH_JUL","August" = "KWH_AUG",
                                                        "Septermber" = "KWH_SEP","October" = "KWH_OCT","November" = "KWH_NOV","December" = "KWH_DEC"),
                                                       selected="TOTAL_KWH"
                                                     ),
                                          
                                          selectInput("selectGas", "Select Gas Total or Monthly:",
                                                      c( "None" = "NONE", "Total Gas" = "TOTAL_THERMS","January" = "THERM_JAN","Feburuary" = "THERM_FEB","March" = "THERM_MAR",
                                                         "April" = "THERM_APR","May" = "THERM_MAY","June" = "THERM_JUN","July" = "THERM_JUL","August" = "THERM_AUG",
                                                         "Septermber" = "THERM_SEP","October" = "THERM_OCT","November" = "THERM_NOV","December" = "THERM_DEC"),
                                                        selected="NONE"
                                                      ),
                                          
                                          selectInput("buildingInfo", "Select Building Info:",
                                                      c("None" = "NONE","Building Type" = "TYPE_BUILDING","Building Age" = "AVG_BUILDING_AGE",
                                                        "Building Population" = "TOTAL_POPU","Building Height" = "AVG_STORIES"),
                                                        selected="NONE"
                                                      ),
                                          selectInput("buildingType", "Select Building Type:",
                                                      c("All Building Types" = "ALL", 
                                                        "Commercial" = "COMMERCIAL", 
                                                        "Industrial" = "INDUSTRIAL", 
                                                        "Residential" = "RESIDENTIAL"),
                                                      selected="ALL"
                                          )
                                                      
                                          
                                   ),
                                   column(10,
                                          
                                          box(title = "Near West Side MapView", solidHeader = TRUE, status = "primary", width = 10,
                                              mapviewOutput("nws"))
                                          ,
                                          fixedRow(
                                            column(5,
                                                   box(title = "Graph", solidHeader = TRUE, status = "primary", width = 5,
                                                       plotOutput("graph")
                                                   )

                                            ),
                                            column(5,
                                                   box(title = "Table", solidHeader = TRUE, status = "info", width = 5,
                                                       dataTableOutput("dataTable")
                                                   )
                                            )
                                          )
                                   )
                                 )
                                 
                                 ),#end Gas or Electric Tab,
             
             #################  NEAR WEST SIDE PANEL #################
             
             
             #################  START COMPARE 2 COMMUNITIES PANEL  #################
             
             
             tabPanel("Compare 2 Communities", 
                      fluidRow(
                        column(2,style='padding-top:50px;',
                               selectInput("selectComm","Select Community area:",
                                           unique(chi[[c("AREA")]]),
                                           selected = "Near West Side"
                               ),
                               selectInput("selectKWH2", "Select KWH Total or Monthly:",
                                           c("None" = "NONE","Total KWH" = "TOTAL_KWH","January" = "KWH_JAN","Feburuary" = "KWH_FEB","March" = "KWH_MAR",
                                             "April" = "KWH_APR","May" = "KWH_MAY","June" = "KWH_JUN","July" = "KWH_JUL","August" = "KWH_AUG",
                                             "Septermber" = "KWH_SEP","October" = "KWH_OCT","November" = "KWH_NOV","December" = "KWH_DEC"),
                                           selected="TOTAL_KWH"
                               ),
                               
                               selectInput("selectGas2", "Select Gas Total or Monthly:",
                                           c( "None" = "NONE", "Total Gas" = "TOTAL_THERMS","January" = "THERM_JAN","Feburuary" = "THERM_FEB","March" = "THERM_MAR",
                                              "April" = "THERM_APR","May" = "THERM_MAY","June" = "THERM_JUN","July" = "THERM_JUL","August" = "THERM_AUG",
                                              "Septermber" = "THERM_SEP","October" = "THERM_OCT","November" = "THERM_NOV","December" = "THERM_DEC"),
                                           selected="NONE"
                               ),
                               
                               selectInput("buildingInfo2", "Select Building Info:",
                                           c("None" = "NONE","Building Type" = "TYPE_BUILDING","Building Age" = "AVG_BUILDING_AGE",
                                             "Building Population" = "TOTAL_POPU","Building Height" = "AVG_STORIES"),
                                           selected="NONE"
                               ),
                               selectInput("buildingType2", "Select Building Type:",
                                           c("All Building Types" = "ALL", 
                                             "Commercial" = "COMMERCIAL", 
                                             "Industrial" = "INDUSTRIAL", 
                                             "Residential" = "RESIDENTIAL"),
                                           selected="ALL"
                               ),
                               selectInput("legendColor", "Select Legend Color:",
                                           c("Default" = "DEFAULT", 
                                             "Plasma" = "COLOR1", 
                                             "Rocket" = "COLOR2"),
                                           selected="DEFAULT"
                               )
                               
                        ),#End column One
                        
                        column(4,style='padding-left:50px;',
                               
                               fluidRow(

                                        box(title = textOutput("title1"), solidHeader = TRUE, status = "primary", width = 10,
                                            leafletOutput("mapOne"))

                            ),#end fluidRow
                            fluidRow(

                                     box(title = "Graph 1", solidHeader = TRUE, status = "primary", width = 5,
                                         plotOutput("graph1"))
                                     

                            )#end fluidRow
                            
                            ),#column 10
                      
                      column(2,style='padding-top:50px;',
                             selectInput("selectComm2","Select Community area:",
                                         unique(chi[[c("AREA")]]),
                                         selected = "Loop"
                             ),
                             selectInput("selectKWH3", "Select KWH Total or Monthly:",
                                         c("None" = "NONE","Total KWH" = "TOTAL_KWH","January" = "KWH_JAN","Feburuary" = "KWH_FEB","March" = "KWH_MAR",
                                           "April" = "KWH_APR","May" = "KWH_MAY","June" = "KWH_JUN","July" = "KWH_JUL","August" = "KWH_AUG",
                                           "Septermber" = "KWH_SEP","October" = "KWH_OCT","November" = "KWH_NOV","December" = "KWH_DEC"),
                                         selected="TOTAL_KWH"
                             ),
                             
                             selectInput("selectGas3", "Select Gas Total or Monthly:",
                                         c( "None" = "NONE", "Total Gas" = "TOTAL_THERMS","January" = "THERM_JAN","Feburuary" = "THERM_FEB","March" = "THERM_MAR",
                                            "April" = "THERM_APR","May" = "THERM_MAY","June" = "THERM_JUN","July" = "THERM_JUL","August" = "THERM_AUG",
                                            "Septermber" = "THERM_SEP","October" = "THERM_OCT","November" = "THERM_NOV","December" = "THERM_DEC"),
                                         selected="NONE"
                             ),
                             
                             selectInput("buildingInfo3", "Select Building Info:",
                                         c("None" = "NONE","Building Type" = "TYPE_BUILDING","Building Age" = "AVG_BUILDING_AGE",
                                           "Building Population" = "TOTAL_POPU","Building Height" = "AVG_STORIES"),
                                         selected="NONE"
                             ),
                             selectInput("buildingType3", "Select Building Type:",
                                         c("All Building Types" = "ALL", 
                                           "Commercial" = "COMMERCIAL", 
                                           "Industrial" = "INDUSTRIAL", 
                                           "Residential" = "RESIDENTIAL"),
                                         selected="ALL"
                             ),
                             
                             
                             
                             ),#column2
                      column(4,style='padding-left:50px;',
                             
                             fluidRow(

                                      box(title = textOutput("title2"), solidHeader = TRUE, status = "info", width = 10,
                                          mapviewOutput("mapTwo"))
                               
                             ),#end fluidRow
                             fluidRow(

                                      box(title = "Graph 2", solidHeader = TRUE, status = "primary", width = 5,
                                          plotOutput("graph2"))
                               
                             )#end fluidRow
                             
                             
                             )#last column
                      
                  )#main fluid row
                      
                      
              ),
             
             #################  END COMPARE 2 COMMUNITIES PANEL #################
             
             
             #################  START COMPARE 2 COMMUNITIES PANEL  #################
             
             
             tabPanel("All of Chicago", 
                      fluidRow(
                        column(2,style='padding-top:70px;',
                               selectInput("selectComm3","Select Community area:",
                                           unique(chi[[c("AREA")]]),
                                           selected = "Loop"
                               ),                               
                               selectInput("selectKWH4", "Select KWH Total or Monthly:",
                                           c("None" = "NONE","Total KWH" = "TOTAL_KWH","January" = "KWH_JAN","Feburuary" = "KWH_FEB","March" = "KWH_MAR",
                                             "April" = "KWH_APR","May" = "KWH_MAY","June" = "KWH_JUN","July" = "KWH_JUL","August" = "KWH_AUG",
                                             "Septermber" = "KWH_SEP","October" = "KWH_OCT","November" = "KWH_NOV","December" = "KWH_DEC"),
                                           selected="TOTAL_KWH"
                               ),
                               
                               selectInput("selectGas4", "Select Gas Total or Monthly:",
                                           c( "None" = "NONE", "Total Gas" = "TOTAL_THERMS","January" = "THERM_JAN","Feburuary" = "THERM_FEB","March" = "THERM_MAR",
                                              "April" = "THERM_APR","May" = "THERM_MAY","June" = "THERM_JUN","July" = "THERM_JUL","August" = "THERM_AUG",
                                              "Septermber" = "THERM_SEP","October" = "THERM_OCT","November" = "THERM_NOV","December" = "THERM_DEC"),
                                           selected="NONE"
                               ),
                               
                               selectInput("buildingInfo4", "Select Building Info:",
                                           c("None" = "NONE","Building Type" = "TYPE_BUILDING","Building Age" = "AVG_BUILDING_AGE",
                                             "Building Population" = "TOTAL_POPU","Building Height" = "AVG_STORIES"),
                                           selected="NONE"
                               ),
                               selectInput("buildingType4", "Select Building Type:",
                                           c("All Building Types" = "ALL", 
                                             "Commercial" = "COMMERCIAL", 
                                             "Industrial" = "INDUSTRIAL", 
                                             "Residential" = "RESIDENTIAL"),
                                           selected="ALL"
                               ),
                               selectInput("legendColor2", "Select Legend Color:",
                                           c("Default" = "DEFAULT", 
                                             "Plasma" = "COLOR1", 
                                             "Rocket" = "COLOR2"),
                                           selected="DEFAULT"
                               )
                              ),#End column One
                        
                        column(4,
                               box(title = textOutput("title3"), solidHeader = TRUE, status = "primary", width = 10,
                                   mapviewOutput("cook_tract"), height = 650)
                               ),
                        column(2,style='padding-top:70px;',
                               selectInput("tractInfo", "Select 10% Census tracts with the:",
                                           c("None" = "NONE",
                                             "Oldest Buildings" = "OLD_BUILDING", 
                                             "Newest Building" = "NEW_BUILDING",
                                             "Tallest Building" = "TALL_BUILDING", 
                                             "Most Electricity Used" = "MOST_ELECTIC", 
                                             "Most Gas Used" = "MOST_GAS",
                                             "Most Population" = "MOST_POPU", 
                                             "Most Occupied %"= "MOST_OCCUPIED", 
                                             "High % of Renters" = "HIGH_RENTER"),
                                           selected="OLD_BUILDING"
                               )   
                          
                        ),
                        column(4,
                               box(title = "Chicago Area Tract Level", solidHeader = TRUE, status = "primary", width = 10,
                                   mapviewOutput("cook_comm"), height = 650)
                        )
                        )
                        
                      ),
             
             
             #################  END COMPARE 2 COMMUNITIES PANEL #################
             
             #### START ABOUT PANEL ####
             tabPanel("About",
                      
                      p("This web app was created by Jesus Perez-Serna on 4/23/2021."),
                      p("The data used in this web app is from https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
                      p("First tab is a map of Near West Side where you can view different types of data in a map, table and graph"),
                      p("Second tab is similar to the firs tab, but you can compare two different communities at the same time."),
                      p("The third tab shows a map of the entire city and you can view the 10% tracts of different types of data.")
                      
                      
             )
             
    )#end NAVBAR PAGE
  
  
)
################# END UI #################



################# START SERVER #################
server <- function(input, output, session) {
  
  #used to update the header of box in part two
  output$title1 <- renderText(paste(input$selectComm," Community Area"))
  output$title2 <- renderText(paste(input$selectComm2," Community Area"))
  output$title3 <- renderText(paste(input$selectComm3," Community Area(Tract)"))
  
  ###########################################################################################################################################
  ###########################################################     UPDATE INPUTS     #########################################################
  ###########################################################################################################################################
  
  observe({
  
    
    if(input$selectKWH == "TOTAL_KWH" | input$selectKWH == "KWH_JAN" | input$selectKWH == "KWH_FEB" | input$selectKWH == "KWH_MAR" |  
       input$selectKWH == "KWH_APR" | input$selectKWH == "KWH_MAY" |input$selectKWH == "KWH_JUN" | input$selectKWH == "KWH_JUL" |
       input$selectKWH == "KWH_AUG" | input$selectKWH == "KWH_SEP" | input$selectKWH == "KWH_OCT" | input$selectKWH == "KWH_NOV" |
       input$selectKWH == "KWH_DEC"){
      
      updateSelectInput(session,'selectGas',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo',
                        selected = 'NONE')
      
    }
    
  })
  
  observe({
    
    
    if(input$selectKWH2 == "TOTAL_KWH" | input$selectKWH2 == "KWH_JAN" | input$selectKWH2 == "KWH_FEB" | input$selectKWH2 == "KWH_MAR" |  
       input$selectKWH2 == "KWH_APR" | input$selectKWH2 == "KWH_MAY" |input$selectKWH2 == "KWH_JUN" | input$selectKWH2 == "KWH_JUL" |
       input$selectKWH2 == "KWH_AUG" | input$selectKWH2 == "KWH_SEP" | input$selectKWH2 == "KWH_OCT" | input$selectKWH2 == "KWH_NOV" |
       input$selectKWH2 == "KWH_DEC"){
      
      updateSelectInput(session,'selectGas2',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo2',
                        selected = 'NONE')
      
    }
    
  })
  
  observe({
    
    
    if(input$selectKWH3 == "TOTAL_KWH" | input$selectKWH3 == "KWH_JAN" | input$selectKWH3 == "KWH_FEB" | input$selectKWH3 == "KWH_MAR" |  
       input$selectKWH3 == "KWH_APR" | input$selectKWH3 == "KWH_MAY" |input$selectKWH3 == "KWH_JUN" | input$selectKWH3 == "KWH_JUL" |
       input$selectKWH3 == "KWH_AUG" | input$selectKWH3 == "KWH_SEP" | input$selectKWH3 == "KWH_OCT" | input$selectKWH3 == "KWH_NOV" |
       input$selectKWH3 == "KWH_DEC"){
      
      updateSelectInput(session,'selectGas3',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo3',
                        selected = 'NONE')
      
    }
    
  })
  observe({
    
    
    if(input$selectKWH4 == "TOTAL_KWH" | input$selectKWH4 == "KWH_JAN" | input$selectKWH4 == "KWH_FEB" | input$selectKWH4 == "KWH_MAR" |  
       input$selectKWH4 == "KWH_APR" | input$selectKWH4 == "KWH_MAY" |input$selectKWH4 == "KWH_JUN" | input$selectKWH4 == "KWH_JUL" |
       input$selectKWH4 == "KWH_AUG" | input$selectKWH4 == "KWH_SEP" | input$selectKWH4 == "KWH_OCT" | input$selectKWH4 == "KWH_NOV" |
       input$selectKWH4 == "KWH_DEC"){
      
      updateSelectInput(session,'selectGas4',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo4',
                        selected = 'NONE')
      
    }
    
  })
  
  
  observe({
    
    
    if(input$selectGas == "TOTAL_THERMS" | input$selectGas == "THERM_JAN" | input$selectGas == "THERM_FEB" | input$selectGas == "THERM_MAR" |  
            input$selectGas == "THERM_APR" | input$selectGas == "THERM_MAY" |input$selectGas == "THERM_JUN" | input$selectGas == "THERM_JUL" |
            input$selectGas == "THERM_AUG" | input$selectGas == "THERM_SEP" | input$selectGas == "THERM_OCT" | input$selectGas == "THERM_NOV" |
            input$selectGas == "THERM_DEC"){
      
      updateSelectInput(session,'selectKWH',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo',
                        selected = 'NONE')
      
    }
  
  })
  
  observe({
    
    
    if(input$selectGas2 == "TOTAL_THERMS" | input$selectGas2 == "THERM_JAN" | input$selectGas2 == "THERM_FEB" | input$selectGas2 == "THERM_MAR" |  
       input$selectGas2 == "THERM_APR" | input$selectGas2 == "THERM_MAY" |input$selectGas2 == "THERM_JUN" | input$selectGas2 == "THERM_JUL" |
       input$selectGas2 == "THERM_AUG" | input$selectGas2 == "THERM_SEP" | input$selectGas2 == "THERM_OCT" | input$selectGas2 == "THERM_NOV" |
       input$selectGas2 == "THERM_DEC"){
      
      updateSelectInput(session,'selectKWH2',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo2',
                        selected = 'NONE')
      
    }
    
  })
  
  observe({
    
    
    if(input$selectGas3 == "TOTAL_THERMS" | input$selectGas3 == "THERM_JAN" | input$selectGas3 == "THERM_FEB" | input$selectGas3 == "THERM_MAR" |  
       input$selectGas3 == "THERM_APR" | input$selectGas3 == "THERM_MAY" |input$selectGas3 == "THERM_JUN" | input$selectGas3 == "THERM_JUL" |
       input$selectGas3 == "THERM_AUG" | input$selectGas3 == "THERM_SEP" | input$selectGas3 == "THERM_OCT" | input$selectGas3 == "THERM_NOV" |
       input$selectGas3 == "THERM_DEC"){
      
      updateSelectInput(session,'selectKWH3',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo3',
                        selected = 'NONE')
      
    }
    
  })
  
  observe({
    
    
    if(input$selectGas4 == "TOTAL_THERMS" | input$selectGas4 == "THERM_JAN" | input$selectGas4 == "THERM_FEB" | input$selectGas4 == "THERM_MAR" |  
       input$selectGas4 == "THERM_APR" | input$selectGas4 == "THERM_MAY" |input$selectGas4 == "THERM_JUN" | input$selectGas4 == "THERM_JUL" |
       input$selectGas4 == "THERM_AUG" | input$selectGas4 == "THERM_SEP" | input$selectGas4 == "THERM_OCT" | input$selectGas4 == "THERM_NOV" |
       input$selectGas4 == "THERM_DEC"){
      
      updateSelectInput(session,'selectKWH4',
                        selected = 'NONE')
      updateSelectInput(session,'buildingInfo4',
                        selected = 'NONE')
      
    }
    
  })
  
  
  observe({
    
    
    if(input$buildingInfo == "AVG_BUILDING_AGE" | input$buildingInfo == "TYPE_BUILDING" | 
            input$buildingInfo == "TOTAL_POPU" | input$buildingInfo == "AVG_STORIES" ){
      
      updateSelectInput(session,'selectKWH',
                        selected = 'NONE')
      updateSelectInput(session,'selectGas',
                        selected = 'NONE')
    }
    
  })
  
  observe({
    
    
    if(input$buildingInfo2 == "AVG_BUILDING_AGE" | input$buildingInfo2 == "TYPE_BUILDING" | 
       input$buildingInfo2 == "TOTAL_POPU" | input$buildingInfo2 == "AVG_STORIES" ){
      
      updateSelectInput(session,'selectKWH2',
                        selected = 'NONE')
      updateSelectInput(session,'selectGas2',
                        selected = 'NONE')
    }
    
  })
  observe({
    
    
    if(input$buildingInfo3 == "AVG_BUILDING_AGE" | input$buildingInfo3 == "TYPE_BUILDING" | 
       input$buildingInfo3 == "TOTAL_POPU" | input$buildingInfo3 == "AVG_STORIES" ){
      
      updateSelectInput(session,'selectKWH3',
                        selected = 'NONE')
      updateSelectInput(session,'selectGas3',
                        selected = 'NONE')
    }
    
  })
  observe({
    
    
    if(input$buildingInfo4 == "AVG_BUILDING_AGE" | input$buildingInfo4 == "TYPE_BUILDING" | 
       input$buildingInfo4 == "TOTAL_POPU" | input$buildingInfo4 == "AVG_STORIES" ){
      
      updateSelectInput(session,'selectKWH4',
                        selected = 'NONE')
      updateSelectInput(session,'selectGas4',
                        selected = 'NONE')
    }
    
  })
  
  
  ###########################################################################################################################################
  ###########################################################     UPDATE INPUTS     #########################################################
  ###########################################################################################################################################
  
  
  
  
  nwsReactive <- reactive ({
    
    
    ###################################################################################################################################################
    #################################################################    ALL   ########################################################################
    
    if(input$buildingType == "ALL"){
      
      
      if(input$selectKWH == "TOTAL_KWH" | input$selectKWH == "KWH_JAN" | input$selectKWH == "KWH_FEB" | input$selectKWH == "KWH_MAR" |  
         input$selectKWH == "KWH_APR" | input$selectKWH == "KWH_MAY" |input$selectKWH == "KWH_JUN" | input$selectKWH == "KWH_JUL" |
         input$selectKWH == "KWH_AUG" | input$selectKWH == "KWH_SEP" | input$selectKWH == "KWH_OCT" | input$selectKWH == "KWH_NOV" |
         input$selectKWH == "KWH_DEC"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$selectGas == "TOTAL_THERMS" | input$selectGas == "THERM_JAN" | input$selectGas == "THERM_FEB" | input$selectGas == "THERM_MAR" |  
              input$selectGas == "THERM_APR" | input$selectGas == "THERM_MAY" |input$selectGas == "THERM_JUN" | input$selectGas == "THERM_JUL" |
              input$selectGas == "THERM_AUG" | input$selectGas == "THERM_SEP" | input$selectGas == "THERM_OCT" | input$selectGas == "THERM_NOV" |
              input$selectGas == "THERM_DEC"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol=input$selectGas, layer.name ="Gas Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$buildingInfo == "AVG_BUILDING_AGE"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TYPE_BUILDING"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TOTAL_POPU"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "AVG_STORIES"){
        
        mapview(chi[chi$AREA == "Near West Side", ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "NONE" | input$selectKWH == "NONE" | input$selectGas =="NONE"){
        p("SELECT A INPUT!")
      }
      
    }# END OF ALL
    
    
    ###################################################################################################################################################
    #################################################################    COMMERCIAL   #################################################################
    
    else if(input$buildingType == "COMMERCIAL"){
      
      if(input$selectKWH == "TOTAL_KWH" | input$selectKWH == "KWH_JAN" | input$selectKWH == "KWH_FEB" | input$selectKWH == "KWH_MAR" |  
         input$selectKWH == "KWH_APR" | input$selectKWH == "KWH_MAY" |input$selectKWH == "KWH_JUN" | input$selectKWH == "KWH_JUL" |
         input$selectKWH == "KWH_AUG" | input$selectKWH == "KWH_SEP" | input$selectKWH == "KWH_OCT" | input$selectKWH == "KWH_NOV" |
         input$selectKWH == "KWH_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$selectGas == "TOTAL_THERMS" | input$selectGas == "THERM_JAN" | input$selectGas == "THERM_FEB" | input$selectGas == "THERM_MAR" |  
              input$selectGas == "THERM_APR" | input$selectGas == "THERM_MAY" |input$selectGas == "THERM_JUN" | input$selectGas == "THERM_JUL" |
              input$selectGas == "THERM_AUG" | input$selectGas == "THERM_SEP" | input$selectGas == "THERM_OCT" | input$selectGas == "THERM_NOV" |
              input$selectGas == "THERM_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas, layer.name ="Gas Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$buildingInfo == "AVG_BUILDING_AGE"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TYPE_BUILDING"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TOTAL_POPU"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "AVG_STORIES"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
    }# END COMMERCIAL
    
    
    ###################################################################################################################################################
    #################################################################    INDUSTRIAL   #################################################################
    
    else if(input$buildingType == "INDUSTRIAL"){
      
      if(input$selectKWH == "TOTAL_KWH" | input$selectKWH == "KWH_JAN" | input$selectKWH == "KWH_FEB" | input$selectKWH == "KWH_MAR" |  
         input$selectKWH == "KWH_APR" | input$selectKWH == "KWH_MAY" |input$selectKWH == "KWH_JUN" | input$selectKWH == "KWH_JUL" |
         input$selectKWH == "KWH_AUG" | input$selectKWH == "KWH_SEP" | input$selectKWH == "KWH_OCT" | input$selectKWH == "KWH_NOV" |
         input$selectKWH == "KWH_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$selectGas == "TOTAL_THERMS" | input$selectGas == "THERM_JAN" | input$selectGas == "THERM_FEB" | input$selectGas == "THERM_MAR" |  
              input$selectGas == "THERM_APR" | input$selectGas == "THERM_MAY" |input$selectGas == "THERM_JUN" | input$selectGas == "THERM_JUL" |
              input$selectGas == "THERM_AUG" | input$selectGas == "THERM_SEP" | input$selectGas == "THERM_OCT" | input$selectGas == "THERM_NOV" |
              input$selectGas == "THERM_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas, layer.name ="Gas Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$buildingInfo == "AVG_BUILDING_AGE"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TYPE_BUILDING"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TOTAL_POPU"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "AVG_STORIES"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
    }# END INDUSTRIAL
    
    
    ###################################################################################################################################################
    #################################################################    RESIDENTIAL   #################################################################
    
    else if(input$buildingType == "RESIDENTIAL"){
      
      if(input$selectKWH == "TOTAL_KWH" | input$selectKWH == "KWH_JAN" | input$selectKWH == "KWH_FEB" | input$selectKWH == "KWH_MAR" |  
         input$selectKWH == "KWH_APR" | input$selectKWH == "KWH_MAY" |input$selectKWH == "KWH_JUN" | input$selectKWH == "KWH_JUL" |
         input$selectKWH == "KWH_AUG" | input$selectKWH == "KWH_SEP" | input$selectKWH == "KWH_OCT" | input$selectKWH == "KWH_NOV" |
         input$selectKWH == "KWH_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$selectGas == "TOTAL_THERMS" | input$selectGas == "THERM_JAN" | input$selectGas == "THERM_FEB" | input$selectGas == "THERM_MAR" |  
              input$selectGas == "THERM_APR" | input$selectGas == "THERM_MAY" |input$selectGas == "THERM_JUN" | input$selectGas == "THERM_JUL" |
              input$selectGas == "THERM_AUG" | input$selectGas == "THERM_SEP" | input$selectGas == "THERM_OCT" | input$selectGas == "THERM_NOV" |
              input$selectGas == "THERM_DEC"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas, layer.name ="Gas Used")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
      else if(input$buildingInfo == "AVG_BUILDING_AGE"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TYPE_BUILDING"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "TOTAL_POPU"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      else if(input$buildingInfo == "AVG_STORIES"){
        
        mapview(chi[(chi$AREA == "Near West Side") & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
          addResetMapButton()%>%
          setView(lng=-87.66865, lat=41.87634, zoom=13)
        
      }
      
    }# END RESIDENTIAL
    
    
    
  })
  

  #OneMap reactive for first tabpanel in navpage
  tableReactive <- reactive({


    
    if(input$selectKWH == "TOTAL_KWH" | input$selectGas == "TOTAL_THERMS" | input$buildingInfo == "TYPE_BUILDING" |
       input$buildingInfo == "AVG_BUILDING_AGE" | input$buildingInfo == "TOTAL_POPU"|
       input$buildingInfo == "AVG_STORIES" ){
      
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","TOTAL_KWH","TOTAL_THERMS")]
      names(data)<-c("Community Area","Census Block","Total KWH Used", "Total Therms Used")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_JAN" | input$selectGas == "THERM_JAN" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_JAN","THERM_JAN")]
      names(data)<-c("Community Area","Census Block","KWH Used in Jan", "Gas Used in Jan")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_FEB" | input$selectGas == "THERM_FEB" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_FEB","THERM_FEB")]
      names(data)<-c("Community Area","Census Block","KWH Used in Feb", "Gas Used in Feb")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_MAR" | input$selectGas == "THERM_MAR" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_MAR","THERM_MAR")]
      names(data)<-c("Community Area","Census Block","KWH Used in Mar", "Gas Used in Mar")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_APR" | input$selectGas == "THERM_APR" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_APR","THERM_APR")]
      names(data)<-c("Community Area","Census Block","KWH Used in Apr", "Gas Used in Apr")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_MAY" | input$selectGas == "THERM_MAY" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_MAY","THERM_MAY")]
      names(data)<-c("Community Area","Census Block","KWH Used in May", "Gas Used in May")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_JUN" | input$selectGas == "THERM_JUN" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_JUN","THERM_JUN")]
      names(data)<-c("Community Area","Census Block","KWH Used in June", "Gas Used in June")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_JUL" | input$selectGas == "THERM_JUL" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_JUL","THERM_JUL")]
      names(data)<-c("Community Area","Census Block","KWH Used in July", "Gas Used in July")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_AUG" | input$selectGas == "THERM_AUG" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_AUG","THERM_AUG")]
      names(data)<-c("Community Area","Census Block","KWH Used in Aug", "Gas Used in Aug")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_SEP" | input$selectGas == "THERM_SEP" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_SEP","THERM_SEP")]
      names(data)<-c("Community Area","Census Block","KWH Used in Sep", "Gas Used in Sep")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_OCT" | input$selectGas == "THERM_OCT" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_OCT","THERM_OCT")]
      names(data)<-c("Community Area","Census Block","KWH Used in Oct", "Gas Used in Oct")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_NOV" | input$selectGas == "THERM_NOV" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_NOV","THERM_NOV")]
      names(data)<-c("Community Area","Census Block","KWH Used in Nov", "Gas Used in Nov")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }
    else if(input$selectKWH == "KWH_DEC" | input$selectGas == "THERM_DEC" ){
      
      data <- chi[chi$AREA == "Near West Side",c("AREA","GEOID10","KWH_DEC","THERM_DEC")]
      names(data)<-c("Community Area","Census Block","KWH Used in Dec", "Gas Used in Dec")
      datatable(data, options = list(searching = FALSE,pageLength=5, lengthChange = FALSE),rownames= FALSE)
      
    }


    
    
  })### END OF ONEMAP REACTIVE
  
  
  
  
  
  
  
  
  

  
  

  
  
  
  mapOneReactive <- reactive({
    

    ###################################################################################################################################################
    #################################################################    ALL   ########################################################################
    
    if(input$buildingType2 == "ALL"){
      
      
      if(input$selectKWH2 == "TOTAL_KWH" | input$selectKWH2 == "KWH_JAN" | input$selectKWH2 == "KWH_FEB" | input$selectKWH2 == "KWH_MAR" |  
         input$selectKWH2 == "KWH_APR" | input$selectKWH2 == "KWH_MAY" |input$selectKWH2 == "KWH_JUN" | input$selectKWH2 == "KWH_JUL" |
         input$selectKWH2 == "KWH_AUG" | input$selectKWH2 == "KWH_SEP" | input$selectKWH2 == "KWH_OCT" | input$selectKWH2 == "KWH_NOV" |
         input$selectKWH2 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm), ], zcol=input$selectKWH, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR2"){
          
          
          mapview(chi[(chi$AREA == input$selectComm), ], zcol=input$selectKWH, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }

          

        
         
        
      }
      
      else if(input$selectGas2 == "TOTAL_THERMS" | input$selectGas2 == "THERM_JAN" | input$selectGas2 == "THERM_FEB" | input$selectGas2 == "THERM_MAR" |  
              input$selectGas2 == "THERM_APR" | input$selectGas2 == "THERM_MAY" |input$selectGas2 == "THERM_JUN" | input$selectGas2 == "THERM_JUL" |
              input$selectGas2 == "THERM_AUG" | input$selectGas2 == "THERM_SEP" | input$selectGas2 == "THERM_OCT" | input$selectGas2 == "THERM_NOV" |
              input$selectGas2 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol=input$selectGas2, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol=input$selectGas2, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol=input$selectGas2, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }

        
      }
      
      else if(input$buildingInfo2 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }

        
        
      }
      else if(input$buildingInfo2 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        

        
        
      }
      else if(input$buildingInfo2 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "AVG_STORIES"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm, ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }

        
      }
      
      
    }# END OF ALL
    
    
    ###################################################################################################################################################
    #################################################################    COMMERCIAL   #################################################################
    
    
    else if(input$buildingType2 == "COMMERCIAL"){
      
      
      if(input$selectKWH2 == "TOTAL_KWH" | input$selectKWH2 == "KWH_JAN" | input$selectKWH2 == "KWH_FEB" | input$selectKWH2 == "KWH_MAR" |  
         input$selectKWH2 == "KWH_APR" | input$selectKWH2 == "KWH_MAY" |input$selectKWH2 == "KWH_JUN" | input$selectKWH2 == "KWH_JUL" |
         input$selectKWH2 == "KWH_AUG" | input$selectKWH2 == "KWH_SEP" | input$selectKWH2 == "KWH_OCT" | input$selectKWH2 == "KWH_NOV" |
         input$selectKWH2 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR2"){
          
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas2 == "TOTAL_THERMS" | input$selectGas2 == "THERM_JAN" | input$selectGas2 == "THERM_FEB" | input$selectGas2 == "THERM_MAR" |  
              input$selectGas2 == "THERM_APR" | input$selectGas2 == "THERM_MAY" |input$selectGas2 == "THERM_JUN" | input$selectGas2 == "THERM_JUL" |
              input$selectGas2 == "THERM_AUG" | input$selectGas2 == "THERM_SEP" | input$selectGas2 == "THERM_OCT" | input$selectGas2 == "THERM_NOV" |
              input$selectGas2 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas2, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas2, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas2, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo2 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo2 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "AVG_STORIES"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END COMMERCIAL
    
    
    ###################################################################################################################################################
    #################################################################    INDUSTRIAL   #################################################################
    
    else if(input$buildingType2 == "INDUSTRIAL"){
      
      
      if(input$selectKWH2 == "TOTAL_KWH" | input$selectKWH2 == "KWH_JAN" | input$selectKWH2 == "KWH_FEB" | input$selectKWH2 == "KWH_MAR" |  
         input$selectKWH2 == "KWH_APR" | input$selectKWH2 == "KWH_MAY" |input$selectKWH2 == "KWH_JUN" | input$selectKWH2 == "KWH_JUL" |
         input$selectKWH2 == "KWH_AUG" | input$selectKWH2 == "KWH_SEP" | input$selectKWH2 == "KWH_OCT" | input$selectKWH2 == "KWH_NOV" |
         input$selectKWH2 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR2"){
          
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas2 == "TOTAL_THERMS" | input$selectGas2 == "THERM_JAN" | input$selectGas2 == "THERM_FEB" | input$selectGas2 == "THERM_MAR" |  
              input$selectGas2 == "THERM_APR" | input$selectGas2 == "THERM_MAY" |input$selectGas2 == "THERM_JUN" | input$selectGas2 == "THERM_JUL" |
              input$selectGas2 == "THERM_AUG" | input$selectGas2 == "THERM_SEP" | input$selectGas2 == "THERM_OCT" | input$selectGas2 == "THERM_NOV" |
              input$selectGas2 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas2, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas2, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas2, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo2 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo2 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "AVG_STORIES"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END INDUSTRIAL
    
    
    ###################################################################################################################################################
    #################################################################    RESIDENTIAL   #################################################################
    
    else if(input$buildingType2 == "RESIDENTIAL"){
      
      
      if(input$selectKWH2 == "TOTAL_KWH" | input$selectKWH2 == "KWH_JAN" | input$selectKWH2 == "KWH_FEB" | input$selectKWH2 == "KWH_MAR" |  
         input$selectKWH2 == "KWH_APR" | input$selectKWH2 == "KWH_MAY" |input$selectKWH2 == "KWH_JUN" | input$selectKWH2 == "KWH_JUL" |
         input$selectKWH2 == "KWH_AUG" | input$selectKWH2 == "KWH_SEP" | input$selectKWH2 == "KWH_OCT" | input$selectKWH2 == "KWH_NOV" |
         input$selectKWH2 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR2"){
          
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas2 == "TOTAL_THERMS" | input$selectGas2 == "THERM_JAN" | input$selectGas2 == "THERM_FEB" | input$selectGas2 == "THERM_MAR" |  
              input$selectGas2 == "THERM_APR" | input$selectGas2 == "THERM_MAY" |input$selectGas2 == "THERM_JUN" | input$selectGas2 == "THERM_JUL" |
              input$selectGas2 == "THERM_AUG" | input$selectGas2 == "THERM_SEP" | input$selectGas2 == "THERM_OCT" | input$selectGas2 == "THERM_NOV" |
              input$selectGas2 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas2, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas2, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas2, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo2 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo2 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo2 == "AVG_STORIES"){
        
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
      }
      
      

      
      
      
    }# END RESIDENTIAL
    
    
      
      

    
  })
  
  
  
  
  
  
  mapTwoReactive <- reactive({
    

    
    
    ###################################################################################################################################################
    #################################################################    ALL   ########################################################################
    
    
    if(input$buildingType3 == "ALL"){
      
      
      if(input$selectKWH3 == "TOTAL_KWH" | input$selectKWH3 == "KWH_JAN" | input$selectKWH3 == "KWH_FEB" | input$selectKWH3 == "KWH_MAR" |  
         input$selectKWH3 == "KWH_APR" | input$selectKWH3 == "KWH_MAY" |input$selectKWH3 == "KWH_JUN" | input$selectKWH3 == "KWH_JUL" |
         input$selectKWH3 == "KWH_AUG" | input$selectKWH3 == "KWH_SEP" | input$selectKWH3 == "KWH_OCT" | input$selectKWH3 == "KWH_NOV" |
         input$selectKWH3 == "KWH_DEC"){
        

        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectKWH3, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectKWH3, col.regions = plasma, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectKWH3, col.regions = rocket, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$selectGas3 == "TOTAL_THERMS" | input$selectGas3 == "THERM_JAN" | input$selectGas3 == "THERM_FEB" | input$selectGas3 == "THERM_MAR" |  
              input$selectGas3 == "THERM_APR" | input$selectGas3 == "THERM_MAY" |input$selectGas3 == "THERM_JUN" | input$selectGas3 == "THERM_JUL" |
              input$selectGas3 == "THERM_AUG" | input$selectGas3 == "THERM_SEP" | input$selectGas3 == "THERM_OCT" | input$selectGas3 == "THERM_NOV" |
              input$selectGas3 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectGas3, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectGas3, col.regions = plasma, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol=input$selectGas3, col.regions = rocket, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$buildingInfo3 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        

        
      }
      else if(input$buildingInfo3 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TYPE_BUILDING", layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if(input$buildingInfo3 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TOTAL_POPU", layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        

        
      }
      else if(input$buildingInfo3 == "AVG_STORIES"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVERAGE_STORIES", layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[chi$AREA == input$selectComm2, ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END OF ALL
    
    
    ###################################################################################################################################################
    #################################################################    COMMERCIAL   #################################################################
    
    else if(input$buildingType3 == "COMMERCIAL"){
      
      
      if(input$selectKWH3 == "TOTAL_KWH" | input$selectKWH3 == "KWH_JAN" | input$selectKWH3 == "KWH_FEB" | input$selectKWH3 == "KWH_MAR" |  
         input$selectKWH3 == "KWH_APR" | input$selectKWH3 == "KWH_MAY" |input$selectKWH3 == "KWH_JUN" | input$selectKWH3 == "KWH_JUL" |
         input$selectKWH3 == "KWH_AUG" | input$selectKWH3 == "KWH_SEP" | input$selectKWH3 == "KWH_OCT" | input$selectKWH3 == "KWH_NOV" |
         input$selectKWH3 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH3, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH3, col.regions = plasma, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH3, col.regions = rocket, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$selectGas3 == "TOTAL_THERMS" | input$selectGas3 == "THERM_JAN" | input$selectGas3 == "THERM_FEB" | input$selectGas3 == "THERM_MAR" |  
              input$selectGas3 == "THERM_APR" | input$selectGas3 == "THERM_MAY" |input$selectGas3 == "THERM_JUN" | input$selectGas3 == "THERM_JUL" |
              input$selectGas3 == "THERM_AUG" | input$selectGas3 == "THERM_SEP" | input$selectGas3 == "THERM_OCT" | input$selectGas3 == "THERM_NOV" |
              input$selectGas3 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas3, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas3, col.regions = plasma, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas3, col.regions = rocket, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$buildingInfo3 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if(input$buildingInfo3 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "AVG_STORIES"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END COMMERCIAL
    
    
    ###################################################################################################################################################
    #################################################################    INDUSTRIAL   #################################################################
    
    else if(input$buildingType3 == "INDUSTRIAL"){
      
      
      if(input$selectKWH3 == "TOTAL_KWH" | input$selectKWH3 == "KWH_JAN" | input$selectKWH3 == "KWH_FEB" | input$selectKWH3 == "KWH_MAR" |  
         input$selectKWH3 == "KWH_APR" | input$selectKWH3 == "KWH_MAY" |input$selectKWH3 == "KWH_JUN" | input$selectKWH3 == "KWH_JUL" |
         input$selectKWH3 == "KWH_AUG" | input$selectKWH3 == "KWH_SEP" | input$selectKWH3 == "KWH_OCT" | input$selectKWH3 == "KWH_NOV" |
         input$selectKWH3 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH3, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH3, col.regions = plasma, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH3, col.regions = rocket, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$selectGas3 == "TOTAL_THERMS" | input$selectGas3 == "THERM_JAN" | input$selectGas3 == "THERM_FEB" | input$selectGas3 == "THERM_MAR" |  
              input$selectGas3 == "THERM_APR" | input$selectGas3 == "THERM_MAY" |input$selectGas3 == "THERM_JUN" | input$selectGas3 == "THERM_JUL" |
              input$selectGas3 == "THERM_AUG" | input$selectGas3 == "THERM_SEP" | input$selectGas3 == "THERM_OCT" | input$selectGas3 == "THERM_NOV" |
              input$selectGas3 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas3, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas3, col.regions = plasma, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas3, col.regions = rocket, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$buildingInfo3 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if(input$buildingInfo3 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "AVG_STORIES"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END INDUSTRIAL
    
    
    ###################################################################################################################################################
    #################################################################    RESIDENTIAL   #################################################################
    
    else if(input$buildingType3 == "RESIDENTIAL"){
      
      
      if(input$selectKWH3 == "TOTAL_KWH" | input$selectKWH3 == "KWH_JAN" | input$selectKWH3 == "KWH_FEB" | input$selectKWH3 == "KWH_MAR" |  
         input$selectKWH3 == "KWH_APR" | input$selectKWH3 == "KWH_MAY" |input$selectKWH3 == "KWH_JUN" | input$selectKWH3 == "KWH_JUL" |
         input$selectKWH3 == "KWH_AUG" | input$selectKWH3 == "KWH_SEP" | input$selectKWH3 == "KWH_OCT" | input$selectKWH3 == "KWH_NOV" |
         input$selectKWH3 == "KWH_DEC"){
        
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH3, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH3, col.regions = plasma, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH3, col.regions = rocket, layer.name ="KWH used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$selectGas3 == "TOTAL_THERMS" | input$selectGas3 == "THERM_JAN" | input$selectGas3 == "THERM_FEB" | input$selectGas3 == "THERM_MAR" |  
              input$selectGas3 == "THERM_APR" | input$selectGas3 == "THERM_MAY" |input$selectGas3 == "THERM_JUN" | input$selectGas3 == "THERM_JUL" |
              input$selectGas3 == "THERM_AUG" | input$selectGas3 == "THERM_SEP" | input$selectGas3 == "THERM_OCT" | input$selectGas3 == "THERM_NOV" |
              input$selectGas3 == "THERM_DEC"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas3, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas3, col.regions = plasma, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas3, col.regions = rocket, layer.name ="Gas used")@map%>%
            addResetMapButton()
          
        }
        
      }
      
      else if(input$buildingInfo3 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "TYPE_BUILDING"){
        
        if(input$legendColor == "DEFAULT"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of building")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if(input$buildingInfo3 == "TOTAL_POPU"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo3 == "AVG_STORIES"){
        
        if(input$legendColor == "DEFAULT"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR1"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor == "COLOR2"){
          mapview(chi[(chi$AREA == input$selectComm2) & (chi$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
    }# END RESIDENTIAL
    
    
    
    
    
  })
  
  
  
  
  comReactive <- reactive ({
    
    
    ###################################################################################################################################################
    #################################################################    ALL   ########################################################################
    
    if(input$buildingType4 == "ALL"){
      
      
      if(input$selectKWH4 == "TOTAL_KWH" | input$selectKWH4 == "KWH_JAN" | input$selectKWH4 == "KWH_FEB" | input$selectKWH4 == "KWH_MAR" |  
         input$selectKWH4 == "KWH_APR" | input$selectKWH4 == "KWH_MAY" |input$selectKWH4 == "KWH_JUN" | input$selectKWH4 == "KWH_JUL" |
         input$selectKWH4 == "KWH_AUG" | input$selectKWH4 == "KWH_SEP" | input$selectKWH4 == "KWH_OCT" | input$selectKWH4 == "KWH_NOV" |
         input$selectKWH4 == "KWH_DEC"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectKWH4, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectKWH4, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR2"){
          
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectKWH4, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas4 == "TOTAL_THERMS" | input$selectGas4 == "THERM_JAN" | input$selectGas4 == "THERM_FEB" | input$selectGas4 == "THERM_MAR" |  
              input$selectGas4 == "THERM_APR" | input$selectGas4 == "THERM_MAY" |input$selectGas4 == "THERM_JUN" | input$selectGas4 == "THERM_JUL" |
              input$selectGas4 == "THERM_AUG" | input$selectGas4 == "THERM_SEP" | input$selectGas4 == "THERM_OCT" | input$selectGas4 == "THERM_NOV" |
              input$selectGas4 == "THERM_DEC"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectGas4, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectGas4, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol=input$selectGas4, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo4 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "TYPE_BUILDING"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo4 == "TOTAL_POPU"){
        
        if(input$legendColor2 == "DEFAULT"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "AVG_STORIES"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
    }# END OF ALL
    
    
    ###################################################################################################################################################
    #################################################################    COMMERCIAL   #################################################################
    
    
    else if(input$buildingType4 == "COMMERCIAL"){
      
      
      if(input$selectKWH4 == "TOTAL_KWH" | input$selectKWH4 == "KWH_JAN" | input$selectKWH4 == "KWH_FEB" | input$selectKWH4 == "KWH_MAR" |  
         input$selectKWH4 == "KWH_APR" | input$selectKWH4 == "KWH_MAY" |input$selectKWH4 == "KWH_JUN" | input$selectKWH4 == "KWH_JUL" |
         input$selectKWH4 == "KWH_AUG" | input$selectKWH4 == "KWH_SEP" | input$selectKWH4 == "KWH_OCT" | input$selectKWH4 == "KWH_NOV" |
         input$selectKWH4 == "KWH_DEC"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH4, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH4, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR2"){
          
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectKWH4, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas4 == "TOTAL_THERMS" | input$selectGas4 == "THERM_JAN" | input$selectGas4 == "THERM_FEB" | input$selectGas4 == "THERM_MAR" |  
              input$selectGas4 == "THERM_APR" | input$selectGas4 == "THERM_MAY" |input$selectGas4 == "THERM_JUN" | input$selectGas4 == "THERM_JUL" |
              input$selectGas4 == "THERM_AUG" | input$selectGas4 == "THERM_SEP" | input$selectGas4 == "THERM_OCT" | input$selectGas4 == "THERM_NOV" |
              input$selectGas4 == "THERM_DEC"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas4, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas4, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol=input$selectGas4, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo4 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "TYPE_BUILDING"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo4 == "TOTAL_POPU"){
        
        if(input$legendColor2 == "DEFAULT"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "AVG_STORIES"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Commercial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END COMMERCIAL
    
    
    ###################################################################################################################################################
    #################################################################    INDUSTRIAL   #################################################################
    
    else if(input$buildingType4 == "INDUSTRIAL"){
      
      
      if(input$selectKWH4 == "TOTAL_KWH" | input$selectKWH4 == "KWH_JAN" | input$selectKWH4 == "KWH_FEB" | input$selectKWH4 == "KWH_MAR" |  
         input$selectKWH4 == "KWH_APR" | input$selectKWH4 == "KWH_MAY" |input$selectKWH4 == "KWH_JUN" | input$selectKWH4 == "KWH_JUL" |
         input$selectKWH4 == "KWH_AUG" | input$selectKWH4 == "KWH_SEP" | input$selectKWH4 == "KWH_OCT" | input$selectKWH4 == "KWH_NOV" |
         input$selectKWH4 == "KWH_DEC"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH4, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH4, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR2"){
          
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectKWH4, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas4 == "TOTAL_THERMS" | input$selectGas4 == "THERM_JAN" | input$selectGas4 == "THERM_FEB" | input$selectGas4 == "THERM_MAR" |  
              input$selectGas4 == "THERM_APR" | input$selectGas4 == "THERM_MAY" |input$selectGas4 == "THERM_JUN" | input$selectGas4 == "THERM_JUL" |
              input$selectGas4 == "THERM_AUG" | input$selectGas4 == "THERM_SEP" | input$selectGas4 == "THERM_OCT" | input$selectGas4 == "THERM_NOV" |
              input$selectGas4 == "THERM_DEC"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas4, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas4, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol=input$selectGas4, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo4 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "TYPE_BUILDING"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo4 == "TOTAL_POPU"){
        
        if(input$legendColor2 == "DEFAULT"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "AVG_STORIES"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Industrial"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      
      
    }# END INDUSTRIAL
    
    
    ###################################################################################################################################################
    #################################################################    RESIDENTIAL   #################################################################
    
    else if(input$buildingType4 == "RESIDENTIAL"){
      
      
      if(input$selectKWH4 == "TOTAL_KWH" | input$selectKWH4 == "KWH_JAN" | input$selectKWH4 == "KWH_FEB" | input$selectKWH4 == "KWH_MAR" |  
         input$selectKWH4 == "KWH_APR" | input$selectKWH4 == "KWH_MAY" |input$selectKWH4 == "KWH_JUN" | input$selectKWH4 == "KWH_JUL" |
         input$selectKWH4 == "KWH_AUG" | input$selectKWH4 == "KWH_SEP" | input$selectKWH4 == "KWH_OCT" | input$selectKWH4 == "KWH_NOV" |
         input$selectKWH4 == "KWH_DEC"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, col.regions = plasma, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR2"){
          
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectKWH, col.regions = rocket, layer.name ="KWH Used")@map%>%
            addResetMapButton()
          
          
        }
        
        
        
        
        
        
      }
      
      else if(input$selectGas4 == "TOTAL_THERMS" | input$selectGas4 == "THERM_JAN" | input$selectGas4 == "THERM_FEB" | input$selectGas4 == "THERM_MAR" |  
              input$selectGas4 == "THERM_APR" | input$selectGas4 == "THERM_MAY" |input$selectGas4 == "THERM_JUN" | input$selectGas4 == "THERM_JUL" |
              input$selectGas4 == "THERM_AUG" | input$selectGas4 == "THERM_SEP" | input$selectGas4 == "THERM_OCT" | input$selectGas4 == "THERM_NOV" |
              input$selectGas4 == "THERM_DEC"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas4, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas4, col.regions = plasma, layer.name ="Gas Used")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol=input$selectGas4, col.regions = rocket, layer.name ="Gas Used")@map%>%
            addResetMapButton()
          
        }
        
        
      }
      
      else if(input$buildingInfo4 == "AVG_BUILDING_AGE"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = plasma, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVG_BUILDING_AGE", col.regions = rocket, layer.name = "Avg Building Age")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "TYPE_BUILDING"){
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = plasma, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TYPE_BUILDING", col.regions = rocket, layer.name = "Type of Building")@map%>%
            addResetMapButton()
          
        }
        
        
        
        
      }
      else if(input$buildingInfo4 == "TOTAL_POPU"){
        
        if(input$legendColor2 == "DEFAULT"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
            addResetMapButton()
          
          
        }
        else if(input$legendColor2 == "COLOR1"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = plasma, layer.name = "Total Population")@map%>%
            addResetMapButton()
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="TOTAL_POPU", col.regions = rocket, layer.name = "Total Population")@map%>%
            addResetMapButton()
          
        }
        
        
        
      }
      else if(input$buildingInfo4 == "AVG_STORIES"){
        
        
        if(input$legendColor2 == "DEFAULT"){
          
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR1"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = plasma, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
        else if(input$legendColor2 == "COLOR2"){
          mapview(chi_tracts[(chi_tracts$AREA == input$selectComm3) & (chi_tracts$TYPE_BUILDING == "Residential"), ], zcol="AVERAGE_STORIES", col.regions = rocket, layer.name = "Average Stories")@map%>%
            addResetMapButton()
          
        }
      }
      
      
    }# END RESIDENTIAL
    
    
    
    
    
    
    
    
  })
    
    
    

  
  cookReactive <- reactive ({
    
    
    
    if(input$tractInfo == "OLD_BUILDING"){
      
      n <- 10
      data <- subset(chi_tracts, AVG_BUILDING_AGE > quantile(AVG_BUILDING_AGE, prob = 1 - n/100))
      
      mapview(data, zcol="AVG_BUILDING_AGE", layer.name = "Old Buildings Age")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "NEW_BUILDING"){
      
      n <- 90
      data <- subset(chi_tracts, AVG_BUILDING_AGE < quantile(AVG_BUILDING_AGE, prob = 1 - n/100))
      
      mapview(data, zcol="AVG_BUILDING_AGE", layer.name = "New Buildings Age")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "TALL_BUILDING"){
      
      n <- 10
      data <- subset(chi_tracts, AVERAGE_STORIES > quantile(AVERAGE_STORIES, prob = 1 - n/100))
      
      mapview(chi_tracts[chi_tracts$TENP_AVERAGE_STORIES=="TRUE", ], zcol="AVERAGE_STORIES", layer.name = "Avg Building Stories")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "MOST_ELECTIC"){
      
      mapview(chi_tracts[chi_tracts$TENP_TOTAL_KWH=="TRUE", ], zcol="TOTAL_KWH", layer.name = "KWH Used")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "MOST_GAS"){
      
      mapview(chi_tracts[chi_tracts$TENP_TOTAL_THERMS=="TRUE", ], zcol="TOTAL_THERMS", layer.name = "Gas Used")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "MOST_POPU"){
      
      mapview(chi_tracts[chi_tracts$TENP_TOTAL_POPU=="TRUE", ], zcol="TOTAL_POPU", layer.name = "Total Population")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "MOST_OCCUPIED"){
      
      
      mapview(chi_tracts[chi_tracts$TENP_OCCUPIED_UNITS_PERCENTAGE=="TRUE", ], zcol="OCCUPIED_UNITS_PERCENTAGE", layer.name = "Occupied Units %")@map%>%
        addResetMapButton()
    }
    else if(input$tractInfo == "HIGH_RENTER"){
      
      mapview(chi_tracts[chi_tracts$TENP_RENTER_PERCENTAGE=="TRUE", ], zcol="RENTER_OCCUPIED_HOUSING_PERCENTAGE", layer.name = "Renter Housing %")@map%>%
        addResetMapButton()
    }  
    
    
  })
    

  
  
  
  
  


  
  
  #################################################################    RENDER NWS    #######################################################

  output$nws <- renderLeaflet({
    
    nwsReactive()
    
    
})
  
  #################################################################    END RENDER NWS    #######################################################
  
  
  
  
  #################################################################    RENDER TABLE    #######################################################

  output$dataTable <- DT::renderDataTable({
      tableReactive()
      
      

  })

  
  #################################################################    END RENDER TABLE    #######################################################
  
  
  
  
  #################################################################    RENDER MAP1   #######################################################
  
  output$mapOne <- renderLeaflet({
    
    mapOneReactive()
    
  })
  
  #################################################################    END RENDER MAP1    #######################################################
  
  
  
  
  #################################################################    RENDER MAP2   #######################################################
  
  output$mapTwo <- renderLeaflet({
    
    mapTwoReactive()
  })
  
  #################################################################    END RENDER MAP2    #######################################################
  
  
  
  
  #################################################################    RENDER LAST TAB   #######################################################
  
  output$cook_tract <- renderLeaflet({
    
    comReactive()
     
    
  })
  
  #################################################################   END RENDER LAST TAB   #######################################################
  
  output$cook_comm <- renderLeaflet({
    
    cookReactive()
    
    
  })
}
################# ENDSERVER #################

shinyApp(ui, server)
