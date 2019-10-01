library(shiny)
library(shinyjs)
library(tidyverse)
library(rfishbase)
library(robis)
library(leaflet)

source("helpers.R")

ui <- fluidPage(
  
  tags$head(inlineCSS(css)) ,
  shinyjs::useShinyjs() ,
  tags$head(tags$style(".modal-body {
                       max-height: calc(100vh - 210px);
                       overflow-y: auto;
                       }")) ,
  
  div(class = "outer" , 
      leafletOutput("map" , width = "100%" , height = "100%")
  ) ,
  
  absolutePanel(id = "searchPanel" , class = "panel" , fixed = TRUE, 
                draggable = F, width = "auto", height = "auto" , 
                style = "background:rgba(0,0,255,0);text-align:center;border:rgba(0,0,255,0);" ,
                textInput("search", 
                          label = span(style ="color:#00ffff;font-size:30px" , "Search Fish:") ,
                          placeholder = "Search by species name or common name") ,
                withBusyIndicatorUI(actionButton("search.btn" , label = "Search")) , 
                uiOutput("search.results.list")
                
  )
  
  )

# Define server logic
server <- function(input, output) {
  
  #Create Background Map
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE ,
        minZoom = 2 , maxZoom = 2 ,
        dragging = F
      )
    ) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels 
    ) %>% 
      leaflet::setMaxBounds(lat1 = -85 ,
                            lat2 = 85 ,
                            lng1 = -180 ,
                            lng2 = 180) %>% 
      leaflet::setView(lat = 15 , lng = 0 , zoom = 2)
    
  })
  
  #Create Reactive Values for species occurance map in modal
  values <- reactiveValues(df = species.occurances)
  
  #Show welcome Modal
  showModal(
    modalDialog(
      title = "Welcome to the FishBase Shiny application" ,
      size = "m" ,
      easyClose = TRUE ,
      footer = modalButton("Enter") , 
      tags$p("Lorem Ipsum")
    )
  )
  
  #Query
  query <- reactive({
    
    input$search.btn
    
    q <- isolate(str_to_sentence(input$search))
    
    if (!is.na(validate_names(q))) {
      species(q) %>% mutate(Language = "English") %>% 
        select(Species , 
               ComName = FBname ,
               Language ,
               SpecCode
        ) %>% 
        mutate_at(c("Species" , "ComName") , str_to_title) %>%
        distinct(Species, ComName , .keep_all = T) %>% 
        mutate(choices = paste(ComName , " (<em>" , Species ,  "</em>)" , sep = ""))
    } else {
      
      rfishbase::common_to_sci(q) %>% 
        mutate_at(c("Species" , "ComName") , str_to_title) %>%
        distinct(Species, ComName , .keep_all = T) %>% 
        mutate(choices = paste(ComName , " (<em>" , Species ,  "</em>)" , sep = ""))
      
    }
    
  })
  
  observeEvent(input$search.btn , {
    
    withBusyIndicatorServer("search.btn" , {
      
      if (length(unique(query()$Species)) == 1 ){
        
        select.species <- query()$Species %>% unique() %>% str_to_sentence()
        
        species.table <- rfishbase::species(select.species)
        
        species.occurances <- robis::occurrence(select.species) 
        
        if (is_empty(species.occurances)) {
          
          species.occurances <- bind_cols("year" = NA ,
                                          "species" = NA , 
                                          "lat" = 0 , 
                                          "lng" = 0)
        } else {
          
          species.occurances <- species.occurances %>% 
            select(year = date_year ,
                   species = scientificName , 
                   lat = decimalLatitude , 
                   lng = decimalLongitude)
          
        }
        
        values$df <<- species.occurances
        
        leafletProxy("modal.map")
        
        showModal(
          createModal(select.species = select.species , species.table = species.table)
        )
        
      } 
    })
    
  })
  
  #Render UI when a species is searched
  output$search.results.list <- renderUI({
    
    #Don't render anything until a user clicks the search button
    input$search.btn
    
    withBusyIndicatorServer("search.btn" , {
      
      q <- isolate(rfishbase::validate_names(str_to_sentence(input$search)))
      
      label <- paste("There were" , length(unique(query()$Species)) , "search results. <br> Please select one below.")
      
      if (input$search.btn == 0 ) {
        # Don't render anything if nothing was clicked
      } else if (input$search.btn != 0 & !is.na(q) ) {
        # Don't render anything if the name is validated
      } else if (length(unique(query()$Species)) == 1) {
        # Don't render anything if only one species is returned
      } else if (nrow(query()) == 0) {
        tags$p(style ="color:#00ffff;font-size:14px" , 
               br() ,
               paste0("I'm sorry, no results found for " , input$search , "!") , br() ,
               "Please check your spelling and search again.")
      } else {
        tagList(
          br() , 
          selectizeInput("search.results" ,  label = span(style ="color:#00ffff;font-size:14px" , HTML(label)) ,
                         choices =  split(query()$Species , query()$choices) , 
                         selected = NULL , multiple = TRUE ,
                         options = list(placeholder = "Select Species" , 
                                        maxItems = 1 ,
                                        render = I(
                                          '{
                                          item: function(item, escape) {
                                          return "<div>" + item.label + "</div>"
                                          },
                                          option: function(item, escape) {
                                          return "<div>" + item.label + "</div>"
                                          }}'))
        ) , 
        withBusyIndicatorUI(actionButton("go.btn" , "Go!"))
                                        )
        
  } 
})
})
  
  output$modal.map <- renderLeaflet({
    
    leaflet(values$df) %>% 
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels
      ) %>% 
      addCircleMarkers(lat = values$df$lat ,
                       lng = values$df$lng ,
                       color = "#F5D300" , #"#FCF340" ,
                       stroke = T ,
                       radius = 2 ,
                       opacity = .5 ,
                       group = "species.select"
      ) 
  })
  
  observeEvent(input$go.btn , {
    
    withBusyIndicatorServer("go.btn" , {
      
      select.species <- input$search.results %>% 
        str_to_sentence()
      
      species.table <- rfishbase::species(select.species)
      
      showModal(
        createModal(select.species = select.species , species.table = species.table)
      )
      
      species.occurances <- robis::occurrence(select.species) 
      
      if (is_empty(species.occurances)) {
        
        species.occurances <- bind_cols("year" = NA ,
                                        "species" = NA , 
                                        "lat" = 0 , 
                                        "lng" = 0)
      } else {
        
        species.occurances <- species.occurances %>% 
          select(year = date_year ,
                 species = scientificName , 
                 lat = decimalLatitude , 
                 lng = decimalLongitude)
        
      }
      
      values$df <<- species.occurances
      
      leafletProxy("modal.map")
    })
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
