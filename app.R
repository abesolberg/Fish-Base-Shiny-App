# remotes::install_github("ropensci/rfishbase")
# devtools::install_github("iobis/robis")
# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("mapview")
# install.packages("tinytex")
# install.packages("httr)

library(shiny)
library(shinyjs)
library(tidyverse)
library(rfishbase)
library(robis)
library(leaflet)
library(mapview)
library(tinytex)
library(httr)
library(rmarkdown)
library(lubridate)

source("helpers.R")

ui <- fluidPage(
  
  tags$head(inlineCSS(css)) ,
  shinyjs::useShinyjs() ,
  tags$head(tags$style(".modal-body {
                       max-height: calc(100vh - 210px);
                       overflow-y: auto;
                       }") ,
            tags$script(HTML(jscode))) ,
  
  div(class = "outer" , 
      leafletOutput("map" , width = "100%" , height = "100%")
  ) ,
  
  absolutePanel(id = "searchPanel" , class = "panel" , fixed = TRUE, 
                draggable = F, width = "auto", height = "auto" , 
                style = "background:rgba(0,0,255,0);text-align:center;border:rgba(0,0,255,0);" ,
                tagAppendAttributes(
                  textInput("search", 
                            label = span(style ="color:#00ffff;font-size:30px" , "Search Fish:") ,
                            placeholder = "Search by species name or common name") , 
                  `data-proxy-click` = "searchButton"),
                withBusyIndicatorUI(actionButton("searchButton" , label = "Search")) , 
                uiOutput("search.results.list")
                
  ) ,
  
  div(
    tags$a(href = "https://github.com/abesolberg/Fish-Base-Shiny-App" , img(src = "GitHub-Mark-Light-32px.png" , width = 32)), 
    tags$a(href = "https://www.fishbase.de/home.htm" , img(src = "fishbase-logo.svg" , width = 32)) ,
    style = "position:fixed; bottom:0; right:0; left:0; text-align:left; background:rgba(0,0,255, 0); padding:10px; box-sizing:border-box;") 
  
  
  )

# Define server logic
server <- function(input, output) {
  
  # Create reactive timer to cycle background map every 5 seconds (5000 milliseconds)
  autoInvalidate <- reactiveTimer(5000)
  
  # Cycle through background map
  data <- reactive({
    autoInvalidate() 
    
    x <<- x + 1
    
    if (x > 6) {
      x <<- 1
    }
    
    df.map <- df %>% filter(order == x)
    
  })
  
  # Create Leaflet Proxy to rotate background map
  observe({
    
    leafletProxy("map" , data = data()) %>% 
      clearGroup("df") %>% #You need to add the clearMarkers function so points do not build on each other
      addCircleMarkers(lat = ~lat ,
                       lng = ~lng ,
                       color = ~colors ,
                       stroke = T ,
                       weight = 2 ,
                       radius = 2.5 , 
                       opacity = .5 ,
                       group = "df")
    
  })
  
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
      title = "Welcome to the Shiny FishBase Data Portal" ,
      size = "m" ,
      easyClose = TRUE ,
      footer = modalButton("Enter") , 
      tags$p("Thank you for using the Shiny FishBase Data Portal!" , 
             br() , br(), 
             "This app was build off of the", 
             tags$a(href = "https://www.fishbase.de/home.htm" , "FishBase") ,
             "database and the" , tags$a(href = "https://github.com/ropensci/rfishbase" , "rfishbase") , 
             "R package, created by" , tags$a(href = "https://ropensci.org/" , "rOpenSci.") , 
             "All observation data come from" , tags$a(href = "https://obis.org" , "OBIS") , 
             "and the" , tags$a(href = "https://github.com/iobis/robis" , "robis") , "package in R." ,
             br() , br() , 
             "This application is still in development, and all the code is open source, and available" , 
             tags$a(href = "https://github.com/abesolberg/Fish-Base-Shiny-App" , "here.") ,
             "If you have any questions or comments on the application, or would like to propose a suggestion, please feel free to" , 
             tags$a(href="mailto:charles.solberg@mail.mcgill.ca" , "email me") , "or file a request on" ,
             tags$a(href = "https://github.com/abesolberg/Fish-Base-Shiny-App" , "GitHub.") ,
             br() , br() , "Thank you again!" , br() , "-Abe")
    )
  )
  
  #Query
  query <- reactive({
    
    input$searchButton
    
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
  
  observeEvent(input$searchButton , {
    
    withProgress( message = "Searching..." , {
      
      withBusyIndicatorServer("searchButton" , {
        
        if (length(unique(query()$Species)) == 1 ){
          
          select.species <- query()$Species %>% unique() %>% str_to_sentence()
          
          species.table <- rfishbase::species(select.species)
          
          incProgress(.25 , detail = "Finding Distribution")
          
          species.occurances <- robis::occurrence(select.species) 
          
          incProgress(.75 , detail = "Loading Data")
          
          if (is_empty(species.occurances)) {
            
            species.occurances <- bind_cols("year" = NA ,
                                            "species" = NA , 
                                            "lat" = 0 , 
                                            "lng" = 0 ,
                                            "label" = "No data available" ,
                                            "fillOpacity" = 0 ,
                                            "opacity" = 0)
          } else {
            
            species.occurances <- species.occurances %>% 
              select(year = date_year ,
                     species = scientificName , 
                     lat = decimalLatitude , 
                     lng = decimalLongitude) %>% 
              mutate(label = NULL ,
                     fillOpacity = .2 ,
                     opacity = .5)
            
          }
          
          values$df <<- species.occurances
          
          leafletProxy("modal.map")
          
          incProgress(.9)
          
          showModal(
            createModal(select.species = select.species , species.table = species.table)
          )
          
        } 
      })
      
    })
  })
  
  #Render UI when a species is searched
  output$search.results.list <- renderUI({
    
    #Don't render anything until a user clicks the search button
    input$searchButton
    
    withBusyIndicatorServer("searchButton" , {
      
      q <- isolate(rfishbase::validate_names(str_to_sentence(input$search)))
      
      label <- paste("There were" , length(unique(query()$Species)) , "search results. <br> Please select one below.")
      
      if (input$searchButton == 0 ) {
        # Don't render anything if nothing was clicked
      } else if (input$searchButton != 0 & !is.na(q) ) {
        # Don't render anything if the name is validated
      } else if (length(unique(query()$Species)) == 1) {
        # Don't render anything if only one species is returned
      } else if (nrow(query()) == 0) {
        tags$p(style ="color:#00ffff;font-size:14px" , 
               br() ,
               paste0("I'm sorry, no results found for " , isolate(input$search) , "!") , br() ,
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
      addProviderTiles(providers$CartoDB.DarkMatter
      ) %>% 
      addCircleMarkers(lat = values$df$lat ,
                       lng = values$df$lng ,
                       color = "#F5D300" , 
                       stroke = T ,
                       radius = 2 ,
                       opacity = values$df$opacity ,
                       fillOpacity = values$df$fillOpacity ,
                       label = values$df$label ,
                       labelOptions = labelOptions(
                         noHide = T , textOnly = T ,
                         direction = "top" ,
                         style = list(
                           'text-align' = 'center' ,
                           'font-size' = '30px' ,
                           'color' = "#FFFFFF"
                         )) ,
                       group = "species.select"
      ) 
  })
  
  observeEvent(input$go.btn , {
    withProgress(message = "Searching" , {
      withBusyIndicatorServer("go.btn" , {
        
        select.species <- input$search.results %>% 
          str_to_sentence()
        
        species.table <- rfishbase::species(select.species)
        
        incProgress(.25 , detail = "Finding Distribution")
        
        species.occurances <- robis::occurrence(select.species) 
        
        incProgress(.75 , detail = "Loading Data")
        
        if (is_empty(species.occurances)) {
          
          species.occurances <- bind_cols("year" = NA ,
                                          "species" = NA , 
                                          "lat" = 0 , 
                                          "lng" = 0 ,
                                          "label" = "No data available" ,
                                          "fillOpacity" = 0 ,
                                          "opacity" = 0)
        } else {
          
          species.occurances <- species.occurances %>% 
            select(year = date_year ,
                   species = scientificName , 
                   lat = decimalLatitude , 
                   lng = decimalLongitude) %>% 
            mutate(label = NULL ,
                   fillOpacity = .2 ,
                   opacity = .5)
          
        }
        
        values$df <<- species.occurances
        
        leafletProxy("modal.map")
        
        incProgress(.9)
        
        showModal(
          createModal(select.species = select.species , species.table = species.table)
        )
      })
    })
  })
  
  # Create Download Handler
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      
      q <- input$search
      
      if (!is.na(validate_names(q))) {
        
        select.species <- input$search 
        
      } else { 
        
        select.species <- rfishbase::common_to_sci(q) %>% 
          mutate_at(c("ComName") , str_to_title) %>%
          distinct(Species, ComName , .keep_all = T) %>% 
          pull(Species) %>% unique()
        
      }
      
      if (length(select.species) > 1) {
        
        select.species <- input$search.results %>% 
          str_to_sentence()
        
      }
      
      paste(select.species, sep = '.', "pdf"
      )
    },
    
    content = function(file) {
      
      withProgress(message = "Downloading..." , {
        
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        #owd <- setwd(tempdir())
        #on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        incProgress(.1)
        
        q <- input$search
        
        if (!is.na(validate_names(q))) {
          
          select.species <- q 
          
        } else { 
          
          select.species <- rfishbase::common_to_sci(q) %>% 
            mutate_at(c("ComName") , str_to_title) %>%
            distinct(Species, ComName , .keep_all = T) %>% 
            pull(Species) %>% unique()
          
        }
        
        if (length(select.species) > 1) {
          
          select.species <- input$search.results %>% 
            str_to_sentence()
          
        }
        
        incProgress(.2 , detail = "Getting species description")
        
        # Find Species Table
        species.table <- rfishbase::species(select.species)
        
        url <- paste0("http://www.fishbase.org/images/species/" , species.table$PicPreferredName)
        
        incProgress(.3 , detail = "Getting species distribution")
        # Find Species Occurances
        species.occurances <- robis::occurrence(select.species)
        
        if (!is_empty(species.occurances)) {
          
          species.occurances <- species.occurances %>% 
            select(year = date_year ,
                   species = scientificName , 
                   lat = decimalLatitude , 
                   lng = decimalLongitude) 
          
          m <- ggplot() +
            annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
            geom_point(data = species.occurances , aes(x= lng, y= lat), color = "#FFD200", 
                       alpha= .3, size= 1) + 
            geom_point(data = bounds , aes(x = x , y = y) , alpha = 0) +
            theme_void() +
            theme(
              panel.spacing = unit(c(0 , 0 , 0 , 0) , "null") ,
              plot.margin = grid::unit(c(0 , 0 , 0 , 0) , "cm")
            ) +
            xlim(-180 , 180) +
            ylim(-90 , 90) +
            coord_equal()
          
        } else {
          m <- NULL
        }
        
        incProgress(.5 , detail = "Compiling PDF")
        
        GET(url = url, 
            write_disk("fish.jpg" , overwrite = T))
        
        incProgress(.7 , detail = "Compiling PDF")
        
        library(rmarkdown)
        out <- render('fishbase-printout.Rmd', pdf_document())
        file.rename(out, file)
      })
      
    }
  )
  
  }

# Run the application 
shinyApp(ui = ui, server = server ,  options = list(launch.browser = TRUE))
