# --- --- --- --- --- --- --- --- --- ---
# To do: 
# --- --- --- --- --- --- --- --- --- ---

# Add space use dbmm to visualize: # Add DBMM 95% or 99% 
# Password

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Shiny App for 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# conflict_prefer("box", "shinydashboard")
# conflict_prefer("dplyr::mutate", "dplyr")
# conflict_prefer("dplyr::arrange", "dplyr")
# conflict_prefer("dplyr::summarize", "dplyr")
require(plyr)
library(tidyverse)
library(ggthemes)
require(gridExtra)
library(patchwork)
library(brms)
library(grid)
library(cowplot)
require(lubridate)
library(dplyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(viridis)
require(sf)
require(shinythemes)
require(geohashTools)
library(rsconnect)
require(plyr)
col <- "#D3D3D3"
# source(here::here('debugging', 'utils.R'))
# Load model summaries
# setwd('/Users/diegoellis/projects/Anthropause/Shiny_no_dbmm_covid/')

area_mod_summary = read.csv('area_mod_summary_2022-10-05.csv')
niche_mod_summary = read.csv('niche_mod_summary_2022-10-24.csv')

# Load path to models
indir_space_models = 'area_additive/'
indir_niche_models = 'niche_controlled/'
indir_space_interaction_models = 'area_interaction/'

# dbbmm_files = 'dbbmms_burst_stack/'
# dbbmm_files = '/Users/diegoellis/projects/Anthropause/Shiny_no_dbmm_covid/dbbmms_burst_stack_raster/'
gcloud_pwd <- read_csv("study_id_path_to_google_drive.csv")  %>% 
  dplyr::mutate(study_id = xStudy_id) %>% dplyr::select(-xStudy_id)

passwords <- read.csv('anhtropause_studies.csv')  %>% 
  left_join(gcloud_pwd, by = 'study_id')

# vector of species 
species = unique(area_mod_summary$species)
# Need to fix niche plot: Maybe if else skip statement render plot of niche figure 
species = unique(niche_mod_summary$species)

# Load one point pe day mosie DB # Load and format data
# locs = read.csv('/Users/diegoellis/projects/Anthropause/one_point_per_day_20221118.csv') %>%
locs = read.csv('one_point_per_day_20221118_anno.csv') %>%
  dplyr::mutate(
    geohash_6 = geohashTools::gh_encode(lat, lon, precision = 6L),
    geohash_6_neighbor_north = gh_neighbors(geohash_6, self=FALSE)$north,
    col_level = as.factor(year)
  )

# folder_dbmm <- list.files('dbbmms_burst_stack/', full.names = T, pattern = 'dbbmm_')

dbbmm_size = read.csv('dbbmm_size.csv') %>% dplyr::mutate(    col_level = as.factor(year))
niche_det = read.csv('niche_determinant_anthropause.csv') %>% dplyr::mutate(    col_level = as.factor(year))

# Changed #####
# dbmm_burst_dir = '/Users/diegoellis/projects/Anthropause/dbbmms_burst_stack/'
dbmm_burst_dir = NULL




ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # to enable boxes from shinyDashboard
  useShinydashboard(),
  
  # Row with selectors ----
  fluidRow(
    column(3,
           # App title ----
           titlePanel("Data owner App"),
           sidebarLayout(
             sidebarPanel(HTML('<b>Sharing a crowded planet: COVID-19 reveals  the footprint of human activity on wildlife<b>'),
                          h6(HTML('Team Leads: Ruth Oliver 1,2, Scott Yanco 1, Diego Ellis Soto 1, Walter Jetz 1 <br/> 1. Yale University, 2. University of California Santa Barbara <br>
                                  Shiny App developed by Diego Ellis Soto')),
                          width = 12),
             mainPanel(
               # 'Please select your study species and studyID. You will be able to display individual tagged animals, and the amount of human infrastructure and movement of humans they were exposed to. Below, you will also find species level predictions of space and habitat use '
               img(id ='bls_logo', src = "bls_logo.png", height = 100, width = 300, position = "right"),
               # tags$img(src = 'bls_logo.png'),
               # includeHTML("https://www.bio-logging.net"),
               position = "right")
             , position = "right"
           )
    )
    
    # ,column(10,
    #          # App title ----
    #          titlePanel(h6(HTML('Team Leads: <br/> Ruth Oliver 1,2, Scott Yanco 1, Diego Ellis Soto 1, Walter Jetz 1 <br/> 1. Yale University, 2. University of California Santa Barbara')))# ,
    # )
  ),
  
  # Can remove this
  fluidRow(
    column(3,
           titlePanel(""),
           sidebarLayout(
             sidebarPanel(HTML(''),
                          h6(HTML('')),
                          width = 12),
             mainPanel(
               img(id ='univ_logos', src = "univ_logos.png", height = 100, width = 300, position = "right"),
               position = "right")
             , position = "right"
           )
    )
    
    # ,column(10,
    #          # App title ----
    #          titlePanel(h6(HTML('Team Leads: <br/> Ruth Oliver 1,2, Scott Yanco 1, Diego Ellis Soto 1, Walter Jetz 1 <br/> 1. Yale University, 2. University of California Santa Barbara')))# ,
    # )
  ),
  
  fluidRow(
    
    column(12,
           # App title ----
           titlePanel(h4("Modeling wildlife responses")),
           sidebarLayout(
             
             sidebarPanel(      (HTML(
               '</b>We modelled the response of 40 species of terrestrial birds and mammals to human mobility and infrastructure across the United States.</b> <br> 
                <br> 
            All our analyses are based on individual animal trajectories from 2019-2020 which are displayed below for each individual, separately. For each individual animal, we estimated weekly geographic use area and environmental niche breadth. For each species we fit responses in the context of </b>human modification (static) and mobility (dynamic)</b>.  <br> 
             <br> 
            A diagram of our methodology is detailed below.
             <br> 
'))),
             mainPanel(
               img(src = "species_responses_new.png", height = 500, width = 800, position = "right")
             )
           )
    )
  ),
  fluidRow(
    
    titlePanel(h4("Requested feedback:")),
    sidebarLayout(
      sidebarPanel(      HTML(
        '<b>-We kindly ask you to fill out our Google Survey embedded in the link below.<b><br>
      -<b>Please add your contact information, animal handling permits, desired data sharing options, and funding acquisition <b><br>
      -<b>Please visually inspect the tracking data of individual animals</b><br>
      -<b>Please provide feedback on the modeled Use Area and Environmental Niche Breadth respones of your study species. Do these effects seem plausible?</b><br>
      -<b>You can provide feedback by writing us to: diego.ellissoto@yale.edu</b><br>
      -<b>Note that data for a single species model may be derived from multiple studies</b><br>'
      )),
      mainPanel()
    )
  ), # End of fluid row
  
  fluidRow(
    column(10,
           titlePanel(""),
           sidebarLayout(
             sidebarPanel(
               h5(textInput(inputId = "user_password", label = strong("Password"), "Password selected")), verbatimTextOutput("value")
             ),
             mainPanel()
           )
           
    )
  ),
  fluidRow(
    column(2, 
           # Input: Dropbox for the species ----
           selectInput(inputId = "Species", label = strong("Select a species:"), choices = "", selected = "", multiple = FALSE)),
    column(2, 
           # Input: Dropbox for the individual ----
           selectInput(inputId = "Individual", label = strong("Select an individual:"), choices = "", selected = "", multiple = FALSE)),
    column(2, 
           # Input: Dropbox for the individual ----
           selectInput(inputId = "StudyID", label = strong("Select a studyID:"), choices = "", selected = "", multiple = FALSE))
  ), # End of fluid row
  
  
  fluidRow(
    column(12,
           # App title ----
           titlePanel(h4("Results for Individual animal")),
    )
  ),
  
  
  
  fluidRow(
    shinydashboard::box(
      column(2,
             titlePanel(
               h5("Tracking data for individual animal", align = 'left'))),  
      
      column(2,
             
             title = "Choose data aggregation products",  status = 'primary', solidHeader = TRUE,
             collapsible = FALSE, collapsed = FALSE,
             checkboxGroupInput('Data_for_map', h6('Select aggregation:'), # Data for map gets rendered:
                                #  c('All data','Geohash 5', 'Geohash 3', 'Geohash 6'), # , 'Use area'),
                                c('All data','Geohash 6'), # , 'Use area'),
                                # c('All data','Geohash 6','Geohash 5', 'Geohash 3', 'Space use'),
                                selected = 'All data'),
             #      c('Geohash', 'Space use'))
      ),
      column(8,
             # Output: map ----
             leafletOutput("mymap")),
    )
    
  ), # End of fluid Row 
  # fluidRow(
  #   shinydashboard::box(
  #     column(2,
  #            titlePanel(
  #              h5("Dynamic brownian bridge movement models", align = 'left'))),  
  #     column(2,
  #            
  #            title = "Choose use area estimats",  status = 'primary', solidHeader = TRUE,
  #            collapsible = FALSE, collapsed = FALSE,
  #            checkboxGroupInput('Data_for_map_dbbmm', h6('Select aggregation:'), # Data for map gets rendered:
  #                               c('Use area (2019)','Use area (2020)'),
  #            ),
  #            column(6,
  #                   # Output: map ----
  #                   leafletOutput("mymap_dbbmm")),
  #     ) 
  #   )
  # ),
  # 
  
  fluidRow(
    shinydashboard::box(
      shiny::sidebarPanel(
        
        column(4, 
               tableOutput("google_drive_paths_table")# , width = 6)
        )
      )
    )
  ), # End of fluidRow
  
  
  # # App title ----
  # titlePanel(h4("Species level results")),
  # sidebarLayout(
  #   
  #   # strong("is fun") 
  #   
  #   sidebarPanel(      (HTML(
  #     '</b>-Use area models:</b> We estimated weekly use area for each individual using dynamic Brownian Bridge Movement models. This allowed making predictions of weekly use area under human modification and human mobility for our study speceis. We calculated these models using both an interactive effect, as well as additive effects of human mobility and modification separately. In this app, interactive models will be displayed if a significant interactive effect is shown. Otherwise, additive use area models are displayed. <br/>
  #            </b>-Niche breadth models:</b> We estimated weekly niche breadth after controlling for individual animal use area
  #          For all species, we considered the following variables as components of an individual’s realized niche: NDVI, LST and elevation. <br/>'
  #     
  #   ))),
  #   mainPanel(
  #   )
  # )
  
  
  
  # 
  # 
  # fluidRow(
  #   column(10,
  #          titlePanel(""),
  #          sidebarLayout(
  #            sidebarPanel(
  #              h5(tableOutput("google_drive_paths_table")), verbatimTextOutput("value")
  #            ),
  #            mainPanel()
  #          )
  #          
  #   )
  # ),
  
  
  
  
  fluidRow(
    
    column(6,
           # Output: plotly safegraph
           plotOutput(outputId = "indiv_scatterplot_ghm_sg"))
    ,
    
    column(6,
           # Output: plotly safegraph
           plotOutput(outputId = "dbbmm_niche_week_size_id"))# ,
    # column(2,
    #        # Output: plotly safegraph
    #        plotOutput(outputId = "niche_week_id"))
  ),
  
  
  column(12,
         # App title ----
         titlePanel(h4("Species level results")),
         sidebarLayout(
           
           # strong("is fun") 
           
           sidebarPanel(      (HTML(
             '</b>-Use area models:</b> We estimated weekly use area for each individual using dynamic Brownian Bridge Movement models. This allowed making predictions of weekly use area under human modification and human mobility for our study speceis. We calculated these models using both an interactive effect, as well as additive effects of human mobility and modification separately. In this app, interactive models will be displayed if a significant interactive effect is shown. Otherwise, additive use area models are displayed. <br/>
             </b>-Niche breadth models:</b> We estimated weekly niche breadth after controlling for individual animal use area
           For all species, we considered the following variables as components of an individual’s realized niche: NDVI, LST and elevation. <br/>'
             
           ))),
           mainPanel(
           )
         )
  ),
  fluidRow(
    column(12,
           titlePanel(
             # h4(paste0("Model output for study species ", output$species_id ), align = 'center')
             h4(textOutput(outputId = 'titulo'), align = 'center')
           )
           # In the server part.
           
    )
  ),
  
  fluidRow(
    column(6,
           # Output: Additive space model ----
           plotOutput(outputId = "space_model")),
    column(6, 
           # Output: Niche model ----
           plotOutput(outputId = "niche_model"))
  ),
  
  fluidRow(
    shinydashboard::box(
      column(4,
             titlePanel(
               # h5(paste0("Tracking data for species ", input$Species), align = 'left'))),  
               h4(textOutput(outputId = 'tracking_data_species_titulo'), align = 'center')
               
             )),
      column(8,
             # Output: map ----
             leafletOutput("mymap_species")),
    )
  ),
  
  
  # fluidRow(
  #   
  #   shinydashboard::box(
  #     title = "Data sharing options",  status = 'primary', solidHeader = TRUE, 
  #     collapsible = FALSE, collapsed = FALSE,
  #     uiOutput('tab')# )# ,
  #     
  #   ) # End of box
  #   
  # ) # End of fluid row
  # 
) # Close Fluid page


server <- function(session, input, output){
  
  # add help box with instructions
  
  output$HelpBox = renderUI({
    if (input$action %% 2){
      helpText(HTML(
        
        # "Start by typing <I>your name</I> in the <b>Identifier</b> box, then select your <b>Animal handling permits</b> and </b>data sharing permission settings</b>. <br/>
        '<b>-We kindly ask you to fill out our Google Survey embedded in the link below.<b><br>
      -<b>Please add your contact information, animal handling permits, desired data sharing options, and funding acquisition <b><br>
      -<b>Please visually inspect the tracking data of individual animals</b><br>
      -<b>Please provide feedback on the modeled Use Area and Environmental Niche Breadth respones of your study species. Do these effects seem plausible?</b><br>
      -<b>You can provide feedback by writing us to: diego.ellissoto@yale.edu</b><br>
      -<b>Note that data for a single species model may be derived from multiple studies</b><br>'
      )) } else {
        return()
      }
  })
  
  # url <- a(HTML("</b>Submit your data sharing permissions here</b>"), href="https://docs.google.com/forms/d/e/1FAIpQLSdsDfN_UAHKUV-KhO2y5VNruls3WIfGrqvS6c3v6bEyjrk_MQ/viewform")
  # output$tab <- renderUI({
  #   tagList(h4(HTML("</b>URL link:</b>"), url))
  # })
  # 
  # Password
  output$value <- renderText({ input$user_password })
  
  
  # update species list option depending on checkbox
  observe({
    tmp_species_name = locs[locs$study_id %in% passwords[passwords$password %in% paste0(input$user_password),]$study_id,]$taxon_canonical_name
    
    #browser()
    updateSelectInput(session, "Species",
                      label = "Species:",
                      sort(unique(tmp_species_name))
                      
                      
    )
  })
  
  # update species list option depending on checkbox
  observe({
    #browser()
    updateSelectInput(session, "StudyID",
                      label = "StudyID:",
                      choices = sort(unique((locs %>% dplyr::filter(study_id %in% passwords[passwords$password %in% paste0(input$user_password),]) %>% dplyr::filter(taxon_canonical_name == input$Species))$study_id))
    )                    
  })
  
  # To add:
  # Add study ID as a filter?!
  # update species list option depending on checkbox
  observe({
    #browser()
    updateSelectInput(session, "Individual",
                      label = "Individual:",
                      choices = sort(unique((locs %>% dplyr::filter(study_id %in% passwords[passwords$password %in% paste0(input$user_password),]) %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID ))$individual_id))
    )                    
  })
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Adding URL of DBMMs
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  
  
  # URL links:
  # reactive({passwords[passwords$study_id == input$StudyID, ]$URL_link_to_DBMM})
  reactive_google_drive_paths = reactive({passwords[passwords$study_id == input$StudyID, ]$URL_link_to_DBMM})
  
  
  # output$google_drive_paths_table <- renderTable({
  #   reactive_google_drive_paths()
  # })
  
  output$google_drive_paths_table <- 
    # shinydashboard::box(
    renderUI({
      tagList(HTML("</b>URL link:</b>"),
              a(HTML("</b>Explore dynamic brownian bridge models <br/> for individual animals here</b>"), href= reactive_google_drive_paths() )
      )
    })
  # )
  
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # End of Adding URL of DBMMs
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  
  species_metadata = reactive({
    map_id <- locs %>% dplyr::filter(taxon_canonical_name == input$Species) 
    ddply(map_id,'taxon_canonical_name', function(x){
      data.frame(
        studies = length(unique(x$study_id)),
        inds = length(unique(x$individual_id)),
        n_locs = nrow(x),
        time_range_begin = range(x$timestamp)[1],
        time_range_end = range(x$timestamp)[2]
      )
    })
    
    
  })
  
  # subset data based on Individual input
  dat_id <- reactive({
    map_id <- locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual) 
    map_id <- map_id %>% dplyr::arrange(as.Date(timestamp))
    map_id
  })
  
  # subset data using all individuals of a species and just display geohash lvl 3 and make the map of the USA:
  species_lvl_dat_id <- reactive({
    map_id <- locs %>% dplyr::filter(taxon_canonical_name == input$Species) 
    map_id <- map_id %>% dplyr::arrange(as.Date(timestamp))
    map_id
  })
  
  
  # dbbmm 
  dbbmm_size_id <- reactive({
    map_id <- dbbmm_size %>% dplyr::filter(species == input$Species & study_id == input$StudyID & ind_id == input$Individual)
    map_id <- map_id %>% dplyr::arrange(year, wk)
    map_id
  })
  
  niche_breadth_id <-  reactive({
    map_id <- niche_det %>% dplyr::filter(scientificname == input$Species & studyid == input$StudyID & individual == input$Individual)  %>% drop_na(total)
    map_id <- map_id %>% dplyr::arrange(year, week)
    map_id
  })
  
  
  # Create geohash sf object for level 3
  # geo_3 = reactive({
  #   dat <-  locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual)
  #   geo_3_sf =   gh_to_sf( dat$geohash_3 )
  #   geo_3_sf$geohash_3 = rownames(geo_3_sf)
  #   n_sum_g3 = dat %>% group_by(geohash_3) %>% dplyr::summarize(n_points =  n())
  #   geo_3_sf_2 = left_join(geo_3_sf, n_sum_g3, by = 'geohash_3')
  #   # paste0(str(geo_3_sf_2))
  #   return(geo_3_sf_2)
  # })
  # 
  # 
  # # Create geohash object for level 5
  # geo_5 = reactive({
  #   dat <- locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual)
  #   geo_5_sf =   gh_to_sf( dat$geohash_5 )
  #   geo_5_sf$geohash_5 = rownames(geo_5_sf)
  #   n_sum_g5 = dat %>% group_by(geohash_5) %>% dplyr::summarize(n_points =  n())
  #   geo_5_sf_2 = left_join(geo_5_sf, n_sum_g5, by = 'geohash_5')
  #   return(geo_5_sf_2)
  # 
  # })
  # 
  # Create geohash object for level 5
  # geo_5 = reactive({
  #   dat <- locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual) 
  #   geo_5_sf =   gh_to_sf( dat$geohash_5 )
  #   geo_5_sf$geohash_5 = rownames(geo_5_sf)
  #   n_sum_g5 = dat %>% group_by(geohash_5) %>% dplyr::summarize(n_points =  n())
  #   geo_5_sf_2 = left_join(geo_5_sf, n_sum_g5, by = 'geohash_5')
  #   return(geo_5_sf_2)
  #   
  # })
  # 
  
  
  # geo_3 = reactive({
  #   dat <-  locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual) 
  #   geo_3_sf =   gh_to_sf( dat$geohash_3 )
  #   geo_3_sf$geohash_3 = rownames(geo_3_sf)
  #   n_sum_g3 = dat %>% group_by(geohash_3) %>% dplyr::summarize(n_points =  n())
  #   geo_3_sf_2 = left_join(geo_3_sf, n_sum_g3, by = 'geohash_3')
  #   # paste0(str(geo_3_sf_2))  
  #   return(geo_3_sf_2)
  # })
  # 
  
  
  geo_6 =  reactive({
    dat <-  locs %>% dplyr::filter(taxon_canonical_name == input$Species & study_id == input$StudyID & individual_id == input$Individual)  %>% dplyr::arrange(as.Date(timestamp))
    geo_6_sf =   gh_to_sf( c(dat$geohash_6, dat$geohash_6_neighbor_north) )
    geo_6_sf$geohash_6 = rownames(geo_6_sf)
    n_sum_g6 = dat %>% group_by(geohash_6) %>% dplyr::summarize(n_points =  n())
    geo_6_sf_2 = left_join(geo_6_sf, n_sum_g6, by = 'geohash_6')
    # paste0(str(geo_3_sf_2))  
    return(geo_6_sf_2)
  })
  
  
  # Create a species level geohash level 3
  geo_3_species = reactive({
    dat <- locs %>% dplyr::filter(taxon_canonical_name == input$Species) 
    geo_3_sf =   gh_to_sf( dat$geohash_3 )
    geo_3_sf$geohash_3 = rownames(geo_3_sf)
    n_sum_g3 = dat %>% group_by(geohash_3) %>% dplyr::summarize(n_points =  n())
    geo_3_sf_2_species = left_join(geo_3_sf, n_sum_g3, by = 'geohash_3')
    return(geo_3_sf_2_species)
    
  })
  
  # --- --- --- --- --- --- --- ---
  # Debugging DBMMs TIF ####
  # --- --- --- --- --- --- --- ---
  # dbbmm_obj_2019 = reactive({
  #   # browser()
  #   
  #   # dbmm_files = list.files(dbmm_burst_dir, pattern = paste0(unique(input$Individual)), full.names = T)
  #   # if(!length(dbbmm_files) == 0)
  #   # if( any(str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2019.tif') ) == TRUE) ){
  #   
  #   if( any(list.files(dbbmm_files, pattern = paste0('dbbmm_',input$Individual, '_2019.tif') ) == TRUE) ){
  #     # if( any(str_detect(dbbmm_files, pattern = '_2019.tif') == TRUE) ){
  #     # dbbmm_files_2019 =  dbbmm_files[str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2019.tif') ) ][1]
  #     
  #     # dbbmm_files_2019 =  dbbmm_files[str_detect(list.files(dbbmm_files), pattern = paste0(input$Individual, '_2019.tif') ) ][1]
  #     
  #     # dbbmm_files_2019 =  dbbmm_files[list.files(dbbmm_files), pattern = paste0('dbbmm_', input$Individual, '_2019.tif')  ][1]
  #     dbbmm_files_2019 =  list.files(dbbmm_files, pattern = paste0('dbbmm_', input$Individual, '_2019.tif'), full.names = T)
  #     
  #     dbbmm_files_2019 = dbbmm_files_2019[endsWith(dbbmm_files_2019, '.tif')]
  #     dbmm_2019 = raster::raster(dbbmm_files_2019)
  #     
  #     
  #     message(print(class(dbmm_2019)))
  #     
  #     
  #     rm(dbbmm_files_2019)
  #     return(dbmm_2019)
  #   }
  #   # }else{
  #   #   dbmm_2019 <- NULL
  #   #   }
  #   
  #   # rm(dbmm_2019)
  #   # if( all(str_detect(dbbmm_files, pattern = '_2019.tif') == FALSE)  ){
  #   #   dbmm_2019 <- NULL
  #   # }
  # })
  # 
  # # message(print(class(dbbmm_obj_2019() )))
  # # warnings(print(class(dbbmm_obj_2019() )))
  # # print(class(dbbmm_obj_2019() ))  
  # 
  # dbbmm_obj_2020 = reactive({
  #   
  #   if( any(list.files(dbbmm_files, pattern = paste0('dbbmm_',input$Individual, '_2020.tif') ) == TRUE) ){
  #     # if( any(str_detect(list.files(dbbmm_files), pattern = paste0(input$Individual, '_2020.tif') ) == TRUE) ){
  #     # if( any(str_detect(dbbmm_files, pattern = '_2019.tif') == TRUE) ){
  #     # dbbmm_files_2019 =  dbbmm_files[str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2019.tif') ) ][1]
  #     
  #     # dbbmm_files_2019 =  dbbmm_files[str_detect(list.files(dbbmm_files), pattern = paste0(input$Individual, '_2019.tif') ) ][1]
  #     
  #     # dbbmm_files_2019 =  dbbmm_files[list.files(dbbmm_files), pattern = paste0('dbbmm_', input$Individual, '_2019.tif')  ][1]
  #     dbbmm_files_2020 =  list.files(dbbmm_files, pattern = paste0('dbbmm_', input$Individual, '_2020.tif'), full.names = T)
  #     
  #     dbbmm_files_2020 = dbbmm_files_2020[endsWith(dbbmm_files_2020, '.tif')]
  #     dbmm_2020 = raster::raster(dbbmm_files_2020)
  #     rm(dbbmm_files_2020)
  #     return(dbmm_2020)
  #     # }else{
  #     #   dbmm_2020 <- NULL
  #     # }
  #     
  #     rm(dbmm_2020)
  #   }
  #   # OLD STUFF I JUST REPLACED ####
  #   # if( any(str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2020.tif') ) == TRUE) ){
  #   #   dbbmm_files_2020 =  dbbmm_files[str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2020.tif') )][1]
  #   #   dbbmm_files_2020 = dbbmm_files_2020[endsWith(dbbmm_files_2020, '.tif')]
  #   #   dbmm_2020 = raster::raster(dbbmm_files_2020)
  #   #   return(dbmm_2020)
  #   # }else{
  #   #   dbmm_2020 <- NULL
  #   #   }
  #   # rm(dbbmm_files_2020)
  #   # rm(dbmm_2020)
  #   # if( all(str_detect(dbbmm_files, pattern = '_2020.tif') == FALSE)  ){
  #   #   dbmm_2020 <- NULL
  #   # }
  #   
  # })
  # --- --- --- --- --- --- --- --- --- ---
  # Make a reactive dbbmm object object for 2019 and 2020 separately
  dbbmm_obj_2019 = reactive({
    req(input$Individual)
    dbbmm_files = list.files(dbmm_burst_dir, pattern = paste0(input$Individual), full.names = T)
    message(paste0(dbbmm_files))
    if(length(dbbmm_files) == 2){
      load(dbbmm_files[1])
      dbmm_2019 = ud95
      names(dbmm_2019) <- gsub('.rdata','',basename(dbbmm_files[1]))
      rm(ud95)
    }
    # If dbbmm file contains 2019, then 
    if( length(dbbmm_files) == 1){
      if( str_detect(dbbmm_files, pattern = '_2019.rdata') == TRUE){
        load(dbbmm_files)
        dbmm_2019 = ud95
        names(dbmm_2019) <- gsub('.rdata','',basename(dbbmm_files))
        rm(ud95)
      }
      
      if( str_detect(dbbmm_files, pattern = '_2019.rdata') == FALSE){
        dbmm_2019 <- NULL
      }
      
    }
    
    return(dbmm_2019)
    
  })
  
  dbbmm_obj_2020 = reactive({
    req(input$Individual)
    dbbmm_files = list.files(dbmm_burst_dir, pattern = paste0(input$Individual), full.names = T)
    
    if(length(dbbmm_files) == 2){
      load(dbbmm_files[2])
      dbmm_2020 = ud95
      names(dbmm_2020) <- gsub('.rdata','',basename(dbbmm_files[2]))
      rm(ud95)
    }
    # If dbbmm file contains 2020, then 
    if( length(dbbmm_files) == 1){
      if( str_detect(dbbmm_files, pattern = '_2020.rdata') == TRUE){
        load(dbbmm_files)
        dbmm_2020 = ud95
        names(dbmm_2020) <- gsub('.rdata','',basename(dbbmm_files))
        rm(ud95)
      }
      
      if( str_detect(dbbmm_files, pattern = '_2020.rdata') == FALSE){
        dbmm_2020 <- NULL
      }
      
    }
    
    return(dbmm_2020)
    
  })
  
  
  
  
  
  
  space_use_interactive = reactive({
    req(input$Species)
    if(file.exists(list.files(indir_space_interaction_models, full.names = T, pattern = input$Species))){  
      int_model_space = list.files(indir_space_interaction_models, full.names = T, pattern = input$Species)
      load( int_model_space) # get the newest:
      area_int <- out$model
      rm(out)
      return(area_int)
    }
  })
  
  
  ghm_scale_sg_norm_interactive_space_use = reactive({
    req(input$Species) 
    
    if(file.exists(list.files(indir_space_interaction_models, full.names = T, pattern = input$Species))){  
      int_model_space = list.files(indir_space_interaction_models, full.names = T, pattern = input$Species)
      load( int_model_space) # get the newest:
      ghm_scale_sg_norm_interactive_space_use <- out$data %>% dplyr::select(ghm_scale, sg_norm, scientificname, studyid, individual)  %>% dplyr::filter(individual == input$Individual) 
      rm(out)
      return(ghm_scale_sg_norm_interactive_space_use)
    }
  })
  
  # browser()
  space_use_interactive_data = reactive({
    req(input$Species)
    # tmp = area_mod_summary %>% filter(species == input$Species)
    # if(tmp$inter_sig == 'Y'){
    if(file.exists(list.files(indir_space_interaction_models, full.names = T, pattern = input$Species))){  
      int_model_space = list.files(indir_space_interaction_models, full.names = T, pattern = input$Species)
      load( int_model_space) # get the newest:
      # area_int_data <- data.frame(out$data$ghm_scale)
      area_ghmq <- quantile(out$data$ghm_scale, probs = c(0.10, 0.90), na.rm = T)
      rm(out)
      return(area_ghmq)
      
    }
  })
  
  
  
  space_use = reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    # sp = species %in% input$taxa # Filter by the user defined taxa
    # Load the newest model:  
    single_sp_add_area_mod = list.files(indir_space_models, full.names = T, pattern = input$Species)
    
    # Add if else interactive or additive:
    # area_mod_summary %>% filter(inter_sig == 'Y')
    
    load(single_sp_add_area_mod[length(single_sp_add_area_mod)])  
    area_add <- out
    space_use <- area_add$model
    rm(out)
    return(space_use)
  })
  
  
  ghm_scale_sg_norm_used_space_use = reactive({
    req(input$Species) 
    single_sp_add_area_mod = list.files(indir_space_models, full.names = T, pattern = input$Species)
    load(single_sp_add_area_mod[length(single_sp_add_area_mod)])  
    ghm_scale_sg_norm_used_space_use <- out$data %>% dplyr::select(ghm_scale, sg_norm, scientificname, studyid, individual)  %>% dplyr::filter(individual == input$Individual) 
    rm(out)
    return(ghm_scale_sg_norm_used_space_use)
  })
  
  
  
  niche_breadth =  reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    # sp = species %in% input$taxa # Filter by the user defined taxa
    # Load the newest model:  
    
    # Load the newest model:  
    single_sp_cont_niche_mod = list.files(indir_niche_models, full.names = T, pattern = input$Species)
    load(single_sp_cont_niche_mod[length(single_sp_cont_niche_mod)])  
    
    niche_breadth <- out$model
  })
  
  
  
  ghm_scale_sg_norm_used_niche_breadth = reactive({
    req(input$Species) 
    single_sp_cont_niche_mod = list.files(indir_niche_models, full.names = T, pattern = input$Species)
    load(single_sp_cont_niche_mod[length(single_sp_cont_niche_mod)])  
    ghm_scale_sg_norm_used_niche_breadth <- out$data %>% dplyr::select(ghm_scale, sg_norm, scientificname, studyid, individual)  %>% dplyr::filter(individual == input$Individual) 
    rm(out)
    return(ghm_scale_sg_norm_used_niche_breadth)
  })
  
  
  
  # Add significane statements:
  signif_add_ghm =  reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    signif_tmp = area_mod_summary %>% dplyr::filter(species == input$Species) %>% dplyr::select(ghm_sig)
    
    if(signif_tmp$ghm_sig == 'Y'){
      signif <- 'Significant effect '
    }else{
      signif <- 'Non significant effect '
    }
    signif_add_ghm <- signif
  })
  
  signif_add_sg =  reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    signif_tmp = area_mod_summary %>% dplyr::filter(species == input$Species) %>% dplyr::select(sg_sig)
    
    if(signif_tmp$sg_sig == 'Y'){
      signif <- 'Significant effect '
    }else{
      signif <- 'Non significant effect '
    }
    signif_add_sg <- signif
  })
  
  niche_add_sg = reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    signif_tmp = niche_mod_summary %>% dplyr::filter(species == input$Species) %>% dplyr::select(cont_sg_sig)
    
    if(signif_tmp$cont_sg_sig == 'Y'){
      signif_niche_sg <- 'Significant effect '
    }else{
      signif_niche_sg <- 'Non significant effect '
    }
    niche_add_sg <- signif_niche_sg
  })
  
  niche_add_ghm  = reactive({
    req(input$Species) # requirement function: the input the user provides we require the user 
    signif_tmp = niche_mod_summary %>% dplyr::filter(species == input$Species) %>% dplyr::select(cont_ghm_sig)
    
    if(signif_tmp$cont_ghm_sig == 'Y'){
      signif_niche_ghm <- 'Significant effect '
    }else{
      signif_niche_ghm <- 'Non significant effect '
    }
    niche_add_ghm <- signif_niche_ghm
  })
  
  
  # Plot Space Use model:
  output$space_model <- renderPlot({
    # make_additive_area_model(wildlife(), indir_space_models, area_mod_summary)
    
    if(area_mod_summary %>% dplyr::filter(species == input$Species) %>% dplyr::select(inter_sig) == 'Y'){
      
      
      # Conditional Effects Plot for interaction
      area_ce_int <- conditional_effects(x=space_use_interactive(), 
                                         effects = "sg_norm:ghm_scale",
                                         int_conditions = list(ghm_scale = space_use_interactive_data()),
                                         re_formula = NA)
      pal <- c("#7552A3", "#CEBEDA")
      (area_int_ce_plot <-  plot(area_ce_int, plot = FALSE,
                                 line_args = list("se"=T,
                                                  "alpha" = 0.2))[[1]] +
          scale_color_manual(values = pal, name = "Modification",
                             labels = c("High", "Low")) +
          scale_fill_manual(values = pal, name = "Modification",
                            labels = c("High", "Low")) +
          xlab("Mobility") +
          ylab("Use area")+
          theme_cowplot()  +
          theme(# legend.position = "none",
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            aspect.ratio = 1) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
          labs(title = 'Significant interactive Use Area model') + labs(subtitle = 'Conditional effects of modification and mobility from \n interaction models with significant effects on species \n Use Area Low modification reflects the \n 10th pecentile and high modification \n the 90th percentile values of human modification') + theme(legend.position = 'bottom') + 
          # geom_rug(data=ghm_scale_sg_norm_interactive_space_use(), aes(x = sg_norm), inherit.aes = F)   +
          NULL
      )
      
      area_int_ce_plot
      
    }else{
      
      # If else: 
      
      # Make a variable that is significant or not significant and paste it on the header. ####
      # Add Home ranges ####
      
      
      #-- Mobility --#
      # Get conditional effects
      area_sg <- conditional_effects(x=space_use(),
                                     effects = "sg_norm",
                                     re_formula = NA)
      
      (area_sg_ce_plot <-  plot(area_sg, plot = F,
                                line_args = list("se" = T,
                                                 "color" = col,
                                                 "fill" = col))[[1]] +
          # scale_color_manual(values = palnew[3])+
          # theme_minimal() +
          xlab("Mobility") +
          ylab("Use Area")+
          # ggtitle("Puma concolor")+
          theme_cowplot()  +
          theme(legend.title = element_blank(),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12),
                aspect.ratio = 1) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
          ggtitle(paste0('Additive Use Area model \n', signif_add_sg())) +  
          theme(legend.position = 'bottom') + 
          # geom_rug(data=ghm_scale_sg_norm_used_space_use(), aes(x = sg_norm), inherit.aes = F)   +
          
          # labs(x  = "Day of year", y = "Mobility") +
          NULL
      )
      
      #-- Modification --#
      
      # Get conditional effects
      area_ghm <- conditional_effects(x=space_use(),
                                      effects = "ghm_scale",
                                      re_formula = NA)
      
      (area_ghm_ce_plot <-  plot(area_ghm, plot = F,
                                 line_args = list("se" = T,
                                                  "color" = col,
                                                  "fill" = col))[[1]] +
          # scale_color_manual(values = palnew[3])+
          # theme_minimal() +
          xlab("Modification") +
          ylab("Use Area")+
          # ggtitle("Puma concolor")+
          theme_cowplot()  +
          theme(legend.title = element_blank(),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12),
                aspect.ratio = 1) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
          ggtitle(paste0('Additive Use Area model \n', signif_add_ghm())) +
          NULL
        
      )
      
      gridExtra::grid.arrange(area_sg_ce_plot,area_ghm_ce_plot,nrow=1,top=textGrob("Additive effects of human modification and mobility \n on species Use Area"))# , face = 'bold', size = 14))
      
    } # End of else
    
  })
  
  # Plot Niche model:
  output$niche_model <- renderPlot({
    #---- Niche Plots ----#
    
    
    #-- Mobility --#
    
    # Get conditional effects
    niche_sg_cont <- conditional_effects(x=niche_breadth(),
                                         effects = "sg_norm",
                                         re_formula = NA)
    
    (niche_sg_ce_plot_cont <-  plot(niche_sg_cont, plot = F,
                                    line_args = list("se" = T,
                                                     "color" = col,
                                                     "fill" = col))[[1]] +
        # scale_color_manual(values = palnew[3])+
        # theme_minimal() +
        xlab("Mobility") +
        ylab("Niche Breadth")+
        # ggtitle("Puma concolor")+
        theme_cowplot()  +
        theme(legend.position = "none",
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              aspect.ratio = 1) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
        # ggtitle(paste0('Area controlled niche model \n')) +
        # ggtitle(paste0('Additive space use model \n', signif_add_sg())) +  
        ggtitle(paste0('Area controlled niche model \n', niche_add_sg() )) +
        geom_rug(data=ghm_scale_sg_norm_used_niche_breadth(), aes(x = sg_norm), inherit.aes = F)   +
        
        # labs(x  = "Day of year", y = "Mobility") +
        NULL
    )
    
    #-- Modification --#
    
    # Get conditional effects
    niche_ghm_cont <- conditional_effects(x=niche_breadth(),
                                          effects = "ghm_scale",
                                          re_formula = NA)
    
    (niche_ghm_ce_plot_cont <-  plot(niche_ghm_cont, plot = F,
                                     line_args = list("se" = T,
                                                      "color" = col,
                                                      "fill" = col))[[1]] +
        # scale_color_manual(values = palnew[3])+
        # theme_minimal() +
        xlab("Modification") +
        ylab("Niche Breadth")+
        # ggtitle("Puma concolor")+
        theme_cowplot()  +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              aspect.ratio = 1) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
        # ggtitle(paste0('Area controlled niche model \n' )) +
        ggtitle(paste0('Area controlled niche model \n', niche_add_ghm() )) +
        geom_rug(data=ghm_scale_sg_norm_used_niche_breadth(), aes(x = ghm_scale), inherit.aes = F)   +
        
        NULL
    )
    
    
    #  niche_sg_ce_plot_cont + niche_ghm_ce_plot_cont
    gridExtra::grid.arrange(niche_sg_ce_plot_cont,niche_ghm_ce_plot_cont,nrow=1,top=textGrob("Additive effects of human modification and mobility \n on species niche breadth")) ##,  face = 'bold', size = 14))
    
  })
  
  
  
  
  
  # browser()
  output$mymap <- renderLeaflet({
    
    #    if(input$Data_for_map %in% c('Geohash 5', 'Geohash 3', 'Geohash 6')){
    if(input$Data_for_map %in% c('Geohash 6')){
      
      pal <- colorNumeric(palette =c('#440154FF','#FDE725FF'), domain = c(2019, 2020))
      cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
      # pal_ind_geo_3 <- colorNumeric(cm.cols1(100), domain=geo_3()$n_points)
      # pal_ind_geo_5 <- colorNumeric(cm.cols1(100), domain=geo_5()$n_points)
      pal_ind_geo_6 <- colorNumeric(cm.cols1(100), domain=geo_6()$n_points)    
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.Terrain,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        
        addCircleMarkers(data = dat_id(), lng = ~lon, lat = ~lat,
                         popup = paste('Date',dat_id()$date,"<br>",
                                       'Year', dat_id()$year,"<br>",
                                       'Geohash 6',dat_id()$geohash_6,"<br>",
                                       # 'Geohash 5',dat_id()$geohash_5,"<br>",
                                       # 'Geohash 3',dat_id()$geohash_3,"<br>",
                                       'Human modification',round(dat_id()$ghm,2),"<br>",
                                       'Human device count',dat_id()$safegraph_daily_count,"<br>"), 
                         #clusterOptions = markerClusterOptions(),
                         color = ~pal(year), # fill = ~col_level,
                         radius = 6, stroke = FALSE, fillOpacity = 0.8,
                         group = 'All data') %>% 
        addScaleBar(position='topright',
                    options=scaleBarOptions(maxWidth=200,imperial=FALSE)) %>%
        # addPolygons(data = geo_6(), popup = paste('Number of locs', geo_6()$n_points, "<br>"),
        #             group = 'Geohash 6') %>%
        addPolygons(data = geo_6(), popup = paste('Number of locs', geo_6()$n_points, "<br>"),
                    group = 'Geohash 6',   fillColor = 'blue', fillOpacity = 1) # %>% 
      # addPolygons(data = geo_5(), popup = paste('Number of locs', geo_5()$n_points, "<br>"),
      #             group = 'Geohash 5',   fillColor = ~pal_ind_geo_5(n_points), fillOpacity = 0.5) %>%
      # addPolygons(data = geo_3(), popup = paste('Number of locs', geo_3()$n_points, "<br>"),
      # group = 'Geohash 3',   fillColor = ~pal_ind_geo_3(n_points), fillOpacity = 0.5)   # %>%
      
      # group = 'Daily Geohash 6 Individual',   fillColor = 'blue', fillOpacity = 1) 
      
      
    }else{
      # pal = colorFactor(palette = 'viridis', dat_id()$year)
      colors <- c("#440154FF", "#FDE725FF")
      
      pal <- colorNumeric(palette =c('#440154FF','#FDE725FF'), domain = c(2019, 2020))
      leaflet() %>%
        addProviderTiles(providers$Stamen.Terrain,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        
        addCircleMarkers(data = dat_id(), lng = ~lon, lat = ~lat,
                         popup = paste('Date',dat_id()$date,"<br>",
                                       #   'Geohash 6',dat_id()$geohash_6,"<br>",
                                       'Geohash 6',dat_id()$geohash_6,"<br>",
                                       # 'Geohash 5',dat_id()$geohash_5,"<br>",
                                       # 'Geohash 3',dat_id()$geohash_3,"<br>",
                                       'Human modification',round(dat_id()$ghm,2),"<br>",
                                       'Human device count',dat_id()$safegraph_daily_count,"<br>"), 
                         #clusterOptions = markerClusterOptions(),
                         color = ~pal(year), # fill = ~col_level,
                         radius = 6, stroke = FALSE, fillOpacity = 0.8,
                         group = 'All data') %>% 
        addScaleBar(position='topright',
                    options=scaleBarOptions(maxWidth=200,imperial=FALSE))  %>%
        addLegend(position = "bottomright",
                  colors = colors,
                  labels = c("2019", "2020"),
                  title = "Years")
      
      
      
    }
    #   }
  })
  
  
  
  observeEvent(input$Data_for_map, {
    
    
    # if('Geohash 5' %in% input$Data_for_map) {
    #   leafletProxy('mymap', data = geo_5()) %>%
    #     showGroup('Geohash 5') %>%
    #     hideGroup('Geohash 3')
    #   hideGroup('Geohash 6')
    # }else{
    #   leafletProxy('mymap', data = geo_5()) %>%
    #     hideGroup('Geohash 5')
    # }
    # if('Geohash 3' %in% input$Data_for_map) {
    #   leafletProxy('mymap', data = geo_3()) %>%
    #     showGroup('Geohash 3') %>%
    #     hideGroup('Geohash 5')
    #   hideGroup('Geohash 6')
    # }else{
    #   leafletProxy('mymap', data = geo_3()) %>%
    #     hideGroup('Geohash 3')
    # }
    if('Geohash 6' %in% input$Data_for_map) {
      leafletProxy('mymap', data = geo_6()) %>%
        showGroup('Geohash 6') #%>%
      #   hideGroup('Geohash 5')
      # hideGroup('Geohash 3')
    }else{
      leafletProxy('mymap', data = geo_6()) %>%
        hideGroup('Geohash 6')
    }
    
  }, ignoreNULL = FALSE)
  
  
  
  # output$mymap_dbbmm <- renderLeaflet({
  #   #   browser()
  #   
  #   # 
  #   # leaflet() %>%
  #   #   addProviderTiles(providers$Stamen.Terrain,
  #   #                    options = providerTileOptions(noWrap = TRUE))
  #   # 
  #   # 
  #   if(input$Data_for_map_dbbmm %in% c('Use area (2019)')){
  #     cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
  #     
  #     # browser()
  #     
  #     # dbmm_files = list.files(dbmm_burst_dir, pattern = paste0(unique(input$Individual)), full.names = T)
  #     # if(!length(dbbmm_files) == 0)
  #     # if( any(str_detect(list.files(dbbmm_files), pattern = paste0(unique(input$Individual), '_2019.tif') ) == TRUE) ){
  #     # 
  #     # 
  #     # dbbmm_obj_2019= reactive({
  #     # 
  #     #   if( any(str_detect(list.files(dbbmm_files), pattern = paste0(input$Individual, '_2019.tif') ) == TRUE) ){
  #     #   dbbmm_files_2019 =  list.files(dbbmm_files, pattern = paste0('dbbmm_', input$Individual, '_2019.tif'), full.names = T)
  #     #   dbbmm_files_2019 = dbbmm_files_2019[endsWith(dbbmm_files_2019, '.tif')]
  #     #   dbmm_2019 = raster::raster(dbbmm_files_2019)
  #     #   message(print(class(dbmm_2019)))
  #     #   rm(dbbmm_files_2019)
  #     #   return(dbmm_2019)
  #     #   }
  #     #   })
  #     
  #     # 
  #     # if(!is.null(dbbmm_obj_2019()  )){
  #     #  if(! length(dbbmm_obj_2019() == 0  )){
  #     # if(class(dbbmm_obj_2019()) == 'RasterLayer'){
  #     leaflet() %>%
  #       addProviderTiles(providers$Stamen.Terrain,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       
  #       addRasterImage(dbbmm_obj_2019(), colors = cm.cols1(100), opacity = 0.8, group = 'Space use (2019)')
  #     # }
  #     
  #   }
  #   
  #   
  #   if(input$Data_for_map_dbbmm %in% c('Use area (2020)')){
  #     cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
  #     
  #     # if(!is.null(dbbmm_obj_2019()  )){
  #     #  if(! length(dbbmm_obj_2019() == 0  )){
  #     # if(class(dbbmm_obj_2019()) == 'RasterLayer'){
  #     leaflet() %>%
  #       addProviderTiles(providers$Stamen.Terrain,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       
  #       addRasterImage(dbbmm_obj_2020(), colors = cm.cols1(100), opacity = 0.8, group = 'Space use (2019)')
  #     # }
  #     
  #   }
  #   
  #   
  #   # else{
  #   #   cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
  #   #   
  #   #   # if(!is.null(dbbmm_obj_2020()  )){
  #   #     # if(! length(dbbmm_obj_2020() == 0  )){
  #   #   if(class(dbbmm_obj_2020()) == 'RasterLayer'){
  #   #     leaflet() %>%
  #   #       addProviderTiles(providers$Stamen.Terrain,
  #   #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #   #       
  #   #       addRasterImage(dbbmm_obj_2020(), colors = cm.cols1(100), opacity = 0.8, group = 'Space use (2020)')
  #   #   }
  #   # }
  #   
  # })
  # 
  # 
  # # Replaced data input in observe event
  # observeEvent(input$Data_for_map_dbbmm, {
  #   
  #   
  #   if('Use area (2019)' %in% input$Data_for_map_dbbmm) {
  #     leafletProxy('mymap_dbbmm', data = dbbmm_obj_2019()) %>%
  #       showGroup('Use area (2019)') %>%
  #       hideGroup('Use area (2020)')
  #   }else{
  #     leafletProxy('mymap_dbbmm', data = dbbmm_obj_2019()) %>%
  #       hideGroup('Use area (2019)')
  #     
  #   }
  #   if('Use area (2020)' %in% input$Data_for_map_dbbmm) {
  #     leafletProxy('mymap_dbbmm', data = dbbmm_obj_2020()) %>%
  #       showGroup('Use area (2020)') %>%
  #       hideGroup('Use area (2019)')
  #     # hideGroup('Geohash 6')
  #   }else{
  #     leafletProxy('mymap_dbbmm', data = dbbmm_obj_2020()) %>%
  #       hideGroup('Use area (2020)')
  #   }
  #   
  # }, ignoreNULL = FALSE)
  # 
  
  output$mymap_species <- renderLeaflet({
    cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}
    pal_species_geo_3 <- colorNumeric(cm.cols1(100), domain=species_lvl_dat_id()$n_points)
    # pal = colorNumeric(palette = 'viridis',domain = geo_3()$n_points) %>%
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      
      addScaleBar(position='topright',
                  options=scaleBarOptions(maxWidth=200,imperial=FALSE)) %>%
      addPolygons(data = geo_3_species(), popup = paste('Number of locs', geo_3_species()$n_points, "<br>"),
                  group = 'Geohash 3',   fillColor = ~pal_species_geo_3(n_points), fillOpacity = 0.5 )    #  %>%
    # addLegend("bottomright", pal = ~pal(year), values = ~species_lvl_dat_id()$year,
    #           title = "Year")# %>%
  })
  
  
  cols <- c('2019' = "#440154FF", '2020' = '#FDE725FF')
  
  # DBBMM size individual
  output$dbbmm_niche_week_size_id <- renderPlot({
    dbmm_week = ggplot( data = dbbmm_size_id(), aes(x = wk, y = (area / 1000000), color = col_level, fill = col_level)) + geom_point()  +
      labs(y = 'Use Area in km2', x = 'Week of the year') +
      # scale_color_viridis(discrete = TRUE) +
      scale_colour_manual(values = cols) +
      theme_classic() + theme(axis.title = element_text(face = "bold", size = 14),
                              legend.position = c(0.90, 0.15), legend.title = element_blank())#  +
    # ggtitle('Weekly space use \n of an individual animal \n through time')
    # })
    
    # output$niche_week_id <- renderPlot({
    niche_week = ggplot( data = niche_breadth_id(), aes(x = week, y = total, color = col_level, fill = col_level)) + geom_point()  +
      labs(y = 'Total niche breadth', x = 'Week of the year') +
      # scale_color_viridis(discrete = TRUE) +
      scale_colour_manual(values = cols) +
      theme_classic() + theme(axis.title = element_text(face = "bold", size = 14),
                              legend.position="none", legend.title = element_blank()) #+
    
    
    gridExtra::grid.arrange(dbmm_week,niche_week,nrow=1,top=textGrob("Modeling responses: \n Weekly Use Area and niche breadth \n of an individual animal"))
  })
  
  
  
  
  
  # Scatterplot GHM
  output$indiv_scatterplot_ghm_sg <- renderPlot({
    
    
    indiv_ghm = ggplot( data = dat_id(), aes(x = jday, y = ghm, color = col_level, fill = col_level)) + geom_point()  +
      labs(y = 'Global Human Modification', x = 'Julian day') +
      # scale_color_viridis(discrete = TRUE) +
      scale_colour_manual(values = cols) +
      theme_classic() + theme(axis.title = element_text(face = "bold", size = 14),
                              legend.position="none", legend.title = element_blank())
    
    indiv_sg = ggplot( data = dat_id(), aes(x = jday, y = safegraph_daily_count, color = col_level, fill = col_level)) + geom_point() +
      labs(y = 'Daily cellphone count', x = 'Julian day') +
      # scale_color_viridis(discrete = TRUE) +
      scale_colour_manual(values = cols) +
      theme_classic() + theme(axis.title = element_text(face = "bold", size = 14),
                              legend.position = c(0.90, 0.15),
                              legend.title = element_blank())
    
    gridExtra::grid.arrange(indiv_sg,indiv_ghm,nrow=1,top=textGrob("Static and dynamic human activities \n experienced by an individual animal"))
    
    
    
  })
  
  output$titulo = renderText({
    # req(input$Species)
    # species = unique(input$Species)
    paste0("Model output for study species ", input$Species,'\n (', species_metadata()$studies, ' studies ', species_metadata()$inds, ' individuals)' )
    
  })
  
  output$tracking_data_species_titulo  = renderText({
    # req(input$Species)
    # species = unique(input$Species)
    paste0("Tracking data for species  ", input$Species,'\n (', species_metadata()$studies, ' studies ', species_metadata()$inds, ' individuals)' )
    
  })
  
  
} # End of server 

shinyApp(ui = ui, server = server)