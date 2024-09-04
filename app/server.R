
library(shiny)
library(leaflet)
library(bslib)
library(tidyverse)
library(googledrive)

server <- function(input, output, session) {
  
  # Adjust working directory for testing purposes
  # before publishing Shiny app to shinyapps.io
  if(!stringr::str_detect(getwd(),'/www$')){
    setwd(paste0(getwd(),'/www'))
  }
  
  # If no log-book currently, create the project 'log-book'
  if(!file.exists('project_logbook.xlsx')){
    openxlsx::write.xlsx(
      data.frame(
        Date = c('a'),
        Name = c('a'),
        Email = c('a'),
        Project_ID = c('a'),
        Focal_Species = c('a'),
        Survey_ID = c('a'),
        Privacy_Options = c('a'),
        Other_Detail = c('a'),
        Study_Area = c('a'),
        Data_File = c('a')
      ),
      'project_logbook.xlsx'
    )
  }
  
  # Setting authentication options
  options(
    # if a token is found, use it.
    garge_oauth_email = TRUE,
    garge_oauth_cache = 'credentials/.secrets'
  )
  
  googledrive::drive_auth(cache = "credentials/.secrets",
                          email = 'mesocarnivoresbc@gmail.com')
  
  # browser()
  # Read the log-book from the Google drive account.
  googledrive::drive_download('MDM_Submitted_Data_Academics/project_logbook.xlsx', overwrite = T)
  #  print('read in logbook.')
  
  # Open the project 'log-book'
  log_book = openxlsx::read.xlsx(
    'project_logbook.xlsx'
  ) |> 
    dplyr::filter(Date != 'a')
  
  # User data
  
  # user_data = reactive({
  #   list(
  #     list.files(path = './data/', pattern = '.csv', full.names = T) |> 
  #       lapply(readr::read_csv),
  #     list.files(path = './data/', pattern = '.xlsx', full.names = T) |> 
  #       lapply(openxlsx::read.xlsx)
  #   )
  # })
  
  # Read in point data and IUCN ranges for map
  # point_data = readRDS("point_data.rds")
  bc_3km_grid = readRDS("bc_3km_grid.rds")
  
  iucn_ranges = readRDS("IUCN_ranges.rds")
  
  bc_parks = readRDS("parks_in_bc.rds") |> 
    sf::st_transform(4326)
  
  unique_iucn_ranges = unique(iucn_ranges$SCI_NAME)
  
  # Add in any species that do not have IUCN ranges.
  names_in_grid = names(bc_3km_grid)[-c(1,ncol(bc_3km_grid))]
  names_to_add = names_in_grid[!names_in_grid %in% stringr::str_replace_all(unique_iucn_ranges," ","_")]
  
  species_to_map = c(unique_iucn_ranges, stringr::str_replace_all(names_to_add,"_"," "))
  species_to_map = species_to_map[order(species_to_map)]
  
  my_colours = c(
      "#FF0000", # Red
      "#00FF00", # Green
      "#0000FF", # Blue
      "#FFA500", # Orange
      "#800080", # Purple
      "#00FFFF", # Cyan
      "#FF00FF", # Magenta
      "#FFFF00", # Yellow
      "#A52A2A", # Brown
      "#FFC0CB", # Pink
      "#808000", # Olive
      "#008080", # Teal
      "#000080", # Navy
      "#BFFF00", # Lime
      "#4B0082", # Indigo
      "#800000",  # Maroon,
      "black",
      "grey"
    )
  
  # Render a leaflet map for map tab.
  l = leaflet() |> 
    addTiles(group = 'OSM') |> 
    addProviderTiles(providers$CartoDB, group = 'CartoDB') |> 
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = 'ESRI_NatGeo') |> 
    addMapPane(name = 'bc_parks', zIndex = 300) |> 
    addMapPane(name = 'species', zIndex = 550) |> 
    addLayersControl(
      position = 'bottomleft',
      baseGroups = c("OSM","CartoDB","ESRI_NatGeo"),
      overlayGroups = c("BC Parks",species_to_map),
      options = layersControlOptions(collapsed = F)
    )
  
  # Find the list of species to plot...
  
  for(i in 1:length(species_to_map)){
    
    the_species = species_to_map[i]
    the_column = stringr::str_replace_all(the_species, " ", "_")
    the_range = iucn_ranges[iucn_ranges$SCI_NAME == the_species,]
  
    # If there is a range for this species, 
    if(nrow(the_range) > 0){
      l = l |> 
        addPolygons(
          data = the_range,
          group = the_species,
          # fillColor = my_colours[i],
          fillColor = 'transparent',
          color = my_colours[i],
          # fillOpacity = 0.6,
          opacity = 0.6,
          options = pathOptions(pane = "species",
                                interactive = FALSE)
        )
    }
    
    if(the_column %in% names(bc_3km_grid)){
      
      species_grid = bc_3km_grid |> 
        dplyr::select(the_column,
                      geom = x) |> 
        dplyr::filter(!is.na(!!rlang::sym(the_column)))
      
      if(nrow(species_grid) > 0){
        l = l |> 
          addPolygons(
            data = species_grid,
            color = 'black',
            weight = 1,
            fillColor = my_colours[i],
            # color = my_colours[i],
            opacity = 1,
            fillOpacity = 0.8,
            label = ~the_species,
            group = the_species,
            options = pathOptions(pane = "species")
          )
      }
    }
    l = l |> 
      hideGroup(the_species)
  }
  
  l |> 
    addPolygons(
      data = bc_parks,
      fillColor = 'darkgreen',
      color = 'green',
      weight = 2,
      fillOpacity = 0.65,
      label = ~PROTECTED_LANDS_NAME,
      options = pathOptions(pane = "bc_parks"),
      group = 'BC Parks'
    )
    
  l = l |> 
    addScaleBar() |> 
    addLegend(colors = my_colours[1:length(species_to_map)],
              labels = species_to_map) |> 
    showGroup('Canis latrans')
  
  output$leafmap = renderLeaflet(l)
  
  coordinates = reactiveVal("Latitude: NULL; Longitude: NULL")
  
  # Grab coordinates from leafmap when clicked.
  output$coordinates_from_leafmap = renderText({
    if(is.null(input$leafmap_click$lat)){
      lat = "NULL"
      lng = "NULL"
    } else {
      lat = as.character(round(input$leafmap_click$lat,3))
      lng = as.character(round(input$leafmap_click$lng,3))
    }
    paste0("Latitude: ",lat,", Longitude: ",lng)
  })
  
  # Generate extra text for Privacy input
  
  output$privacy_info_text = renderText({
    if(input$privacy_options_input == 'proj'){
      return('\n\nRaw contributed data will only be used for the purpose 
             of distribution maps and modelling. 
             Data will be used for this objective only and will not be 
             shared outside of the Conservation Science Section of the 
             Ministry of Water Land and Resource Stewardship (WLRS). 
             Data contributors will only be contacted in case of peer-review 
             publications. All research data will be held and stored in 
             perpetuity in a password protected folder on the network drive 
             of the Ecosystems Branch of WLRS. The Carnivore Conservation Specialist 
             (currently Joanna Burgar) will remain responsible for the data.')
    }
    if(input$privacy_options_input == 'secured'){
      return('\n\nContributed data will be incorporated into provincial scale 
             data systems but managed as secure proprietary data under the Species 
             and Ecosystems Data and Information Security policy. Data secured 
             under the Species and Ecosystems Data and Information Security policy 
             may be shared by request with parties outside of government that have an 
             appropriate rationale and sign a confidentiality agreement. 
             Contributed data will not be made publicly accessible.')
    }
    if(input$privacy_options_input == 'public'){
      return('\n\nContributed data will be incorporated into provincial scale data systems and managed as open data. 
             Data will be publicly accessible. Example of publicly available database.')
    }
  })
  
  # Set up download handlers for template forms.
  
  output$download_cam_template <- downloadHandler(
    filename = function() {
      "Camera_data_MDB_share_template.xlsx"
    },
    content = function(file) {
      file.copy(from = 'Camera_data_MDB_share.xlsx',
                to = file)
    }
  )
  
  output$download_presence_only_template <- downloadHandler(
    filename = function() {
      "Presence_Only_data_MDB_share_template.xlsx"
    },
    content = function(file) {
      file.copy(from = 'Incidental_data_MDB_share.xlsx',
                to = file)
    }
  )
  
  output$download_dna_template <- downloadHandler(
    filename = function() {
      "DNA_data_MDB_share.xlsx"
    },
    content = function(file) {
      file.copy(from = 'DNA_data_MDB_share.xlsx',
                to = file)    
      }
  )
  
  output$download_information <- downloadHandler(
    filename = function() {
      "Mesocarnivore distribution project_WLRS.pdf"
    },
    content = function(file) {
      file.copy(from = 'Mesocarnivore distribution project_WLRS.pdf',
                to = file)    
    }
  )
  
  observeEvent(input$submit_form, {
    
    log_book = openxlsx::read.xlsx('project_logbook.xlsx')
    
    # Clear any shinyFeedback from prior rounds of submission attempts
    shinyFeedback::hideFeedback('name_input')
    shinyFeedback::hideFeedback('proj_id_input')
    shinyFeedback::hideFeedback('study_area_input')
    shinyFeedback::hideFeedback('email_input')
    shinyFeedback::hideFeedback('survey_id_input')
    shinyFeedback::hideFeedback('data_submission_input')
    
    # Check that all fields have something entered in them.
    
    if(input$survey_id_input == "" | 
       input$email_input == "" | 
       input$study_area_input == "" | 
       input$proj_id_input == "" | 
       input$name_input == "" | 
       is.null(input$data_submission_input)){
      
      if(input$survey_id_input == ''){
        shinyFeedback::showFeedback('survey_id_input',
                                    color = '#d9534f',
                                    text = 'Please fill in this field.')
      }
      if(input$email_input == ''){
        shinyFeedback::showFeedback('email_input',
                                    color = '#d9534f',
                                    text = 'Please fill in this field.')
      }
      if(input$study_area_input == ''){
        shinyFeedback::showFeedback('study_area_input',
                                    color = '#d9534f',
                                    text = 'Please fill in this field.')
      }
      if(input$proj_id_input == ''){
        shinyFeedback::showFeedback('proj_id_input',
                                    color = '#d9534f',
                                    text = 'Please fill in this field.')
      }
      if(input$name_input == ''){
        shinyFeedback::showFeedback('name_input',
                                    color = '#d9534f',
                                    text = 'Please fill in this field.')
      }
      if(is.null(input$data_submission_input)){
        shinyFeedback::showFeedback('data_submission_input',
                                    color = '#d9534f',
                                    text = 'Please upload one file.')
      }
      
    } else {
      # Double check that the submitted file is either .csv or .xlsx format.
      if(stringr::str_detect(input$data_submission_input$datapath,'.(csv|xlsx)')){
      
    # Add details to an excel 'log-book' of projects.
    log_book = log_book |> 
      dplyr::bind_rows(
        data.frame(
              Date = as.character(Sys.Date()),
              Name = input$name_input,
              Email = input$email_input,
              Project_ID = input$proj_id_input,
              Focal_Species = input$focal_species_input,
              Survey_ID = input$survey_id_input,
              Privacy_Options = input$privacy_options_input,
              Other_Detail = input$other_security_input,
              Study_Area = input$study_area_input,
              Data_File = paste0(input$proj_id_input, '-',Sys.Date(),'.csv')
        )
      )
    
    # Update the log-book form in the www/ folder with this
    # new entry.
    openxlsx::write.xlsx(log_book, 'project_logbook.xlsx', overwrite = T)
    
    # Update the project XLSX file on google drive.
    drive_update(
      'MDM_Submitted_Data_Academics/project_logbook.xlsx',
      media = 'project_logbook.xlsx'
    )
    
    cat(paste0("\nJust updated excel logbook with row for ",input$proj_id_input))
    
    if(stringr::str_detect(input$data_submission_input$datapath,'.csv')){
      
      # Read in the file that the user has uploaded.
      content = readr::read_csv(input$data_submission_input$datapath)
      
      # And write that file to our data folder, inside the app's www/ folder.
      readr::write_csv(content, paste0('data/',input$proj_id_input, '-',Sys.Date(),'.csv'))
      
      googledrive::drive_upload(
        media = paste0('data/',input$proj_id_input, '-',Sys.Date(),'.csv'),
        path = 'MDM_Submitted_Data_Academics', 
        name = paste0(input$proj_id_input, '-',Sys.Date(),'.csv'),
        overwrite = T
      )
    }
    
    if(stringr::str_detect(input$data_submission_input$datapath,'.xls(x)?')){
      
      file.copy(
        from = input$data_submission_input$datapath,
        to = paste0('data/',input$proj_id_input,'-',Sys.Date(),stringr::str_extract(input$data_submission_input$name,'.xls(x)?$'))
      )
      
      googledrive::drive_upload(
        media = paste0('data/',input$proj_id_input,'-',Sys.Date(),stringr::str_extract(input$data_submission_input$name,'.xls(x)?$')),
        path = 'MDM_Submitted_Data_Academics', 
        name = paste0('data/',input$proj_id_input,'-',Sys.Date(),stringr::str_extract(input$data_submission_input$name,'.xls(x)?$')),
        overwrite = T
      )
      
    }
    
    showModal(
      modalDialog(
        title = 'Submission Successful!',
        h5('Thank you!'),
        easyClose = T
      )
    )
    
    shiny::updateTextInput(session = session, 'name_input', value = '')
    shiny::updateTextInput(session = session, 'email_input', value = '')
    shiny::updateTextInput(session = session, 'survey_id_input', value = '')
    shiny::updateTextInput(session = session, 'proj_id_input', value = '')
    shiny::updateTextInput(session = session, 'study_area_input', value = '')
    shiny::updateSelectInput(session = session, 'focal_species_input', selected = 'Multispecies')
    shiny::updateSelectInput(session = session, 'privacy_options_input', selected = 'proj')
    
      } else {
        showModal(
          modalDialog(
            title = 'Submission Unsuccessful!',
            h5('File Format not .csv or .xlsx'),
            easyClose = T
          )
        )
      }
    }
  })
  
  # observeEvent(input$download_users_data, {
  #   showModal(
  #     modalDialog(
  #     title = 'Log-in Credentials',
  #     fluidRow(
  #       column(width = 6,
  #              textInput('username_input',
  #                        'Username')
  #       ),
  #       column(width = 6,
  #              textInput('password_input',
  #                        'Password'))
  #     ),
  #     fluidRow(
  #       downloadButton('submit_login_creds',
  #                    'Submit Log-in Credentials')
  #     ))
  #     )
  # })
  
  # Create a 'downloadHandler' - this allows the user to download
  # things from the Shiny app to their computer's download folder, 
  # via their internet browser.
  output$submit_login_creds <- downloadHandler(
    
    filename = function() {
      paste0("user_data_download-", Sys.Date(), ".zip")
      # paste0("user_data_download-", Sys.Date(), ".Rdata")
    },
    content = function(file) {
      if(input$username_input == 'mesocarnivoresBC' & input$password_input == 'Lynxcanadensis'){
        
        # Read in most up-to-date log book.
        log_book = openxlsx::read.xlsx('project_logbook.xlsx') |> 
          dplyr::filter(Date != 'a')
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        dir.create(paste0(temp_directory,'/csv'))
        dir.create(paste0(temp_directory,'/excel'))
        
        # Write the excel 'log-book' file out to download folder.
        openxlsx::write.xlsx(log_book, file.path(temp_directory, "log_book.xlsx"))
        
        # Write the user-submitted excel documents to the excel folder.
        
        list.files(path = 'data/', pattern = '.xls(x)?', full.names = T) |> 
          lapply(\(x) file.copy(from = x,
                                to = paste0(temp_directory, "/excel/",stringr::str_remove(x,'data/'))))
                 
        # Write the user-submitted csv documents to the csv folder.
        list.files(path = 'data/', pattern = '.csv', full.names = T) |> 
          lapply(\(x) file.copy(from = x,
                                to = paste0(temp_directory, "/csv/",stringr::str_remove(x,'data/'))))
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        
      }
    }
  )
}