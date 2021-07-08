# R Shiny app to show internet maps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: May 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Import libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(rgdal)

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

# Load ggplot-friendly font using show_text
font_add("gtam", "www/fonts/gtamerica/GT-America-Standard-Regular.ttf",
         bold = "www/fonts/gtamerica/GT-America-Standard-Bold.ttf")
showtext_auto()

# Load shapefiles created in mapping_internet.Rmd
intcomp_spdf <- readOGR("data", "intcomp_by_tract")
broadband_spdf <- readOGR("data", "broadband_by_tract")
income_spdf <- readOGR("data", "income_by_tract")
lang_spdf <- readOGR("data", "lang_by_tract")

# UI --------------------------------------------------------------------------
ui <- fluidPage(theme = "covid19_internet.css",
                
  # Add custom CSS for web page       
  tags$head(
    tags$style(HTML("
      
      body {
        width: 90%;
        margin: auto;
      }
      
      #maps-caption {
        margin: auto;
        max-width: 800px;
        padding-top: 2rem;
      }

    "))
  ),
                
  titlePanel("Mapping Internet Access in Massachusetts"),
  
  em("Read about the full analysis on the", 
     a("Data for Justice blog.", 
       href="http://data.aclum.org/2020/05/13/internet-deserts…-during-covid-19/")),
  
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
  div(id="maps-caption",
      tabsetPanel(type = "tabs",
                  tabPanel("Computer & Internet", 
                           withSpinner(leafletOutput("intcomp_map"), type=4, 
                                       color="#b5b5b5", size=0.5)),
                  tabPanel("Broadband", 
                           withSpinner(leafletOutput("broadband_map"), type=4, 
                                       color="#b5b5b5", size=0.5)),
                  tabPanel("Income", 
                           withSpinner(leafletOutput("income_map"), type=4, 
                                       color="#b5b5b5", size=0.5)),
                  tabPanel("Primary Language", 
                           withSpinner(leafletOutput("lang_map"), type=4, 
                                       color="#b5b5b5", size=0.5))
      ),
      
      em("DATA SOURCE: U.S. Census Bureau, American Community Survey, 2018 5-yr estimates (2014-2018); Table",
         textOutput("tab_num", inline=T)),
      br(),
      em("DATA ANALYSIS: ACLU of Massachusetts")
  ),
  
  div(id="footer",
      hr(),
      div(align="center",
          a(href="https://www.aclum.org/", target="_blank",
            img(src="Logo_CMYK_Massachusetts_Massachusetts.png", height="50px", 
                style="display: inline; margin: 10px;")),
          a(href="https://www.data.aclum.org/",  target="_blank",
            img(src="D4J-logo.png", height="50px", 
                style="display: inline; margin: 10px;"))),
      p("Please contact data4justice@aclum.org with questions.", align="center", style="opacity: 0.6;")
  )
)

# Helper Functions ------------------------------------------------------------

# Do math to figure out percentage of estimate variables
get_percents <- function(df, save=F) {
  
  # Get the input data frame's name as a string
  original_df_name <- deparse(substitute(df))
  
  # Sum together the various "estimate" values and calculate percent of the whole
  df <- df %>%
    group_by(GEOID) %>%
    mutate(sum_value = sum(estimate),
           sum_moe = sum(moe),
           percent = sum_value / summary_est * 100) %>%
    select(-estimate, -moe, -variable) %>%
    unique() %>%
    # Exclude census tracts with no land
    filter(ALAND > 0)
  
  # If desired, save resulting dataframe & spatial data to file
  if (save) {
    writeOGR(obj=df, dsn=paste0("data/", original_df_name), 
             layer=original_df_name, driver="ESRI Shapefile")
  }
  
  return(df)
}

# Replace legend labels with custom format
myLabelFormat = function(..., custom_percent=F, income=F){ 
  # Add "≥" in front of last legend percentage to denote lower limit
  if(custom_percent){ 
    function(type = "numeric", cuts){ 
      cuts <- paste0(cuts, "%")
      cuts[[length(cuts)]] <- paste0("≥", cuts[[length(cuts)]])
      cuts
    } 
    
    # Add "≥" in front of last legend value to denote lower limit & add 
    # commas for legibility in big numbers
  } else if(income){ 
    function(type = "numeric", cuts){ 
      cuts <- paste0("$",format(cuts ,big.mark=",",scientific=FALSE))
      cuts[[length(cuts)]] <- paste0("≥", cuts[[length(cuts)]])
      cuts
    } 
    
    # Use default label formatter  
  } else{
    labelFormat(...)
  }
}

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Internet & Computer Access ------------------------------------------------
  
  # Show partial range (numeric)
  pal_intcomp <- colorNumeric(
    palette = "viridis",
    domain = 0:30,
    na.color = "#FDEF23" #scales::viridis_pal()(10) %>% tail(1)
  )
  
  # Plot map
  output$intcomp_map <- renderLeaflet({
    
    output$tab_num <- renderText({"B28008"})
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = intcomp_spdf,
                  fillColor = ~pal_intcomp(percent),
                  color = "none", # you need to use hex colors
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 0.7,
                  label = ~lapply(paste0(NAME_y, " </br>", round(percent, 2), 
                                         "% without computer or internet"), 
                                  htmltools::HTML),
                  group="circle_marks") %>%
      addLegend(pal = pal_intcomp, 
                values = 0:30,
                position = "topright", 
                title = "<a style='font-family:GT America; color: dimgrey'>
                            Percent of <br>residents without<br>computer or internet<br>by census tract
                        </a>",
                labFormat = myLabelFormat(custom_percent=T)
      )  %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
      addControl("<img src='Logo_White_CMYK_Massachusetts.png'>", 
                 "bottomright", className="logo-control")
    
  })
  
  # Broadband Internet Access ------------------------------------------------
  
  # Show partial range (numeric)
  pal_broadband <- colorNumeric(
    palette = "viridis",
    domain = 0:30,
    na.color = "#FDEF23" #scales::viridis_pal()(10) %>% tail(1)
  )
  
  # Plot map
  output$broadband_map <- renderLeaflet({
    
    output$tab_num <- renderText({"B28008"})
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = broadband_spdf, 
                  fillColor = ~pal_broadband(percent),
                  color = "none", # you need to use hex colors
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 0.7,
                  label = ~lapply(paste0(NAME_y, " </br>", round(percent, 2), "% without broadband internet"), 
                                  htmltools::HTML),
                  group="circle_marks") %>%
      addLegend(pal = pal_broadband, 
                values = 0:30,
                position = "topright", 
                title = "<a style='font-family:GT America; color: dimgrey'>
                        Percent of residents<br>without broadband<br>by census tract
                    </a>",
                labFormat = myLabelFormat(custom_percent=T)
      )  %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
      addControl("<img src='Logo_White_CMYK_Massachusetts.png'>", 
                 "bottomright", className="logo-control")
    
  })
  
  # Income --------------------------------------------------------------------
  
  # Show partial range (numeric)
  pal_income <- colorNumeric(
    palette = "viridis",
    na.color = "#3D0C51",
    reverse=T,
    domain = 20000:150000
  )
  
  # Plot map
  output$income_map <- renderLeaflet({
    
    output$tab_num <- renderText({"S1902"})
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = income_spdf, 
                  fillColor = ~pal_income(percent),
                  color = "none", # you need to use hex colors
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 0.7,
                  label = ~lapply(paste0(NAME_y, " </br>Mean Income: $", 
                                         format(percent, big.mark=",")), 
                                  htmltools::HTML),
                  group="circle_marks") %>%
      addLegend(pal = pal_income, 
                values = 20000:150000,
                position = "topright", 
                title = "<a style='font-family:GT America; color: dimgrey'>
                        Mean household income<br>by census tract
                    </a>",
                labFormat = myLabelFormat(income=T)
      )  %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
      addControl("<img src='Logo_White_CMYK_Massachusetts.png'>", 
                 "bottomright", className="logo-control")
    
  })
  
  # Language Spoken at Home ---------------------------------------------------
  
  # Show partial range (numeric)
  pal_lang <- colorNumeric(
    palette = "viridis",
    domain = 0:70,
    na.color = "#FDEF23" #scales::viridis_pal()(10) %>% tail(1)
  )
  
  # Plot map
  output$lang_map <- renderLeaflet({
    
    output$tab_num <- renderText({"S1601"})
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = lang_spdf, 
                  fillColor = ~pal_lang(percent),
                  color = "none", # you need to use hex colors
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 0.7,
                  label = ~lapply(paste0(NAME_y, " </br>", round(percent, 2), 
                                         "% speak language other than English at home"), 
                                  htmltools::HTML),
                  group="circle_marks") %>%
      addLegend(pal = pal_lang, 
                values = 0:70,
                position = "topright", 
                title = "<a style='font-family:GT America; color: dimgrey'>
                        Percent of residents<br>speaking language other<br>than English at home,<br>by census tract
                    </a>",
                labFormat = myLabelFormat(custom_percent =T)
      )  %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
      addControl("<img src='Logo_White_CMYK_Massachusetts.png'>", 
                 "bottomright", className="logo-control")
    
  })
  
}

shinyApp(ui = ui, server = server)
