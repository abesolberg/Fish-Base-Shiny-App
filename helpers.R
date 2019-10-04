# Helpers.R

## Helpful Links ##

#https://renkun-ken.github.io/rlist-tutorial/Features/Searching.html
#http://ropensci.github.io/rfishbase/
#https://github.com/ropensci/rfishbase
#https://www.fishbase.in/manual/english/contents.htm
#https://kateto.net/network-visualization
#http://curleylab.psych.columbia.edu/netviz/netviz1.html#/
#https://www.fishbase.se/manual/English/fishbasedistribution.htm
#https://www.fishwatch.gov/resources
#https://www.programmableweb.com/api/fishwatch
#https://obis.org/manual/accessr/
#https://www.shutterstock.com/blog/neon-color-palettes
#https://shiny.rstudio.com/articles/selectize.html
#https://rmarkdown.rstudio.com/articles_intro.html
#https://rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf
#https://gist.github.com/yihui/6091942

## For photos 
# Can use just thumbnails by pasting PicPreferred to this url "http://www.fishbase.org/images/thumbnails/jpg/tn_"
# XLM pics here http://www.fishbase.org/webservice/photos/FishPicsList.php?Genus=Salmo&Species=salar&type=


## Things to do still
# Opening page pop-up
# Work Cited
# Stylize PDF downloads -- create conditional chunks
# Add more, relevant informations information per fish
# Try to find way to load/download information faster
# Stylize modal to fit with rest of app


#### CSS ####

css <- 'div.outer {
position: fixed;
top: 0px;
left: 0;
right: 0;
bottom: 0;
overflow: hidden;
padding: 0;
} 

.panel {
position: absolute;
text-align: center;
top: 15%;
left: 50%;
transform: translateX(-50%) ; }' 

#Busy Indicators
withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

## Busy Indicator Functions - https://github.com/daattali/advanced-shiny/tree/master/busy-indicator

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

# Create df for reactive values in modal map
species.occurances <- bind_cols("year" = NA ,
                                "species" = NA , 
                                "lat" = 0 , 
                                "lng" = 0 ,
                                "label" = NULL ,
                                "fillOpacity" = 0 ,
                                "opacity" = 0)

## Create Modal Function
createModal <- function(select.species , species.table ) {
  
  if( is.na(species.table$FBname) ) {
    title <- paste0("<em>" , select.species , "</em>")
  } else {
    title <- paste(species.table$FBname , tags$em(select.species) , sep = ", ")
  }
  
  modalDialog(
    #title = HTML(paste(species.table$FBname , tags$em(select.species) , sep = ", ")) ,
    title = HTML(title) ,
    size = "m" ,
    fade = F ,
    footer = HTML('<button type="button" id = "close.modal" class="btn btn-default" data-dismiss="modal">Close</button>') ,
    #Image
    tags$img(src = paste0("http://www.fishbase.org/images/species/" , 
                          species.table$PicPreferredName) , width = "100%" 
    ) ,
    tags$p(tags$strong("Description:") , br() , 
           HTML(rfishbase::morphology(select.species)$AddChars)
    ) ,
    tags$p(tags$strong("Life Cycle/Mating:") , br() , 
           HTML(rfishbase::reproduction(select.species)$AddInfos)) ,
    tags$p(tags$strong("Distribution:") , br() ,
    leafletOutput("modal.map")) , hr() ,
    tags$span(style = "text-align:center;" , downloadButton('downloadReport'))
  )
}

## Background Map Data

x <- 1

df <- read_rds("background-locations.RDS")

colors <- bind_cols(
  colors = c(
    "#08F7FE" ,
    "#F5D300" ,
    "#FF9472" ,
    "#FE53BB" ,
    "#7122FA" ,
    "#011FFD"
  ) ,
  order = c(1:6)
)

