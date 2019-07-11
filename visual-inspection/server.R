# Drag and drop images code from Dr. Geovany A. Ramirez, April 28, 2019
# Modified by XXX 06 May 2019

server <- function(input, output, session) {
  
  #################### Authentication ###################
  
  # XXX modified a multi-page app from https://github.com/jaehyeon-kim/shiny-multipage/tree/master/utils
  
  ############ Observers and functions for authentication #################
  user_info <- reactiveValues(is_logged = is_logged)
  
  # observers for user authentication in ui_login
  observe({
    if(!is.null(input$login_login)) {
      username <- input$login_username
      # email <- input$email
      password <- input$login_password
      # experience <- input$experience
      
      if(username != "") output$login_username_msg <- renderText("")
      if(password != "") output$login_password_msg <- renderText("")
      # if(experience != "") output$experience_msg<- renderText("")
      # if(email != "") output$email_msg<- renderText("")
    }
  })
  
  observeEvent(input$login_login, {
    username <- isolate(input$login_username)
    # email <- isolate(input$email)
    password <- isolate(input$login_password)
    # experience <- isolate(input$experience)
    
    if(username == "") output$login_username_msg <- renderText("Please enter name")
    # if(email == "") output$email_msg <- renderText("Please enter email address")
    if(password == "") output$login_password_msg <- renderText("Please enter password")
    # if(experience == "") output$experience_msg <- renderText("Please enter experience")
    
    # if(!any(username == "", password == "", experience == "", email == "")) {
    if(!any(username == "", password == "")) {
      is_valid_credentials <- check_login_credentials(username = username, password = password)
      if(is_valid_credentials) {
        user_info$is_logged <- TRUE
        user_info$username <- username
        
        output$login_fail <- renderText("")
        
      } else {
        output$login_fail <- renderText("Login failed, try again or contact admin (XXX")
      }
    }
  })
  
  # add application button when logged in
  observe({
    if(user_info$is_logged) {
      shinyjs::disable("login_username")
      # shinyjs::disable("email")
      shinyjs::disable("login_password")
      # shinyjs::disable("experience")
      output$login_more <- renderUI({
        list(
          hr(),
          actionButton("login_application", "App", icon = icon("bar-chart"), width = "100px")
        )
      })
    }
  })
  
  
  ###### A function to save data #####
  
  # make a function to save data when the Next button is clicked
  saveData <- function(num) {
    
    csvs <- list.files(pattern = ".csv$")[-grep(paste(do_not_remove, collapse = "|"), list.files(pattern = ".csv$"))]
    # rw <- ifelse(length(csvs) > 0, nrow(read.csv(csvs[1])) + 1, 1)
    
    class_A_res <- ifelse(length(input$class_A) > 0, paste(input$class_A, collapse = ";"), NA)
    class_B_res <- ifelse(length(input$class_B) > 0, paste(input$class_B, collapse = ";"), NA)
    class_C_res <- ifelse(length(input$class_C) > 0, paste(input$class_C, collapse = ";"), NA)
    class_D_res <- ifelse(length(input$class_D) > 0, paste(input$class_D, collapse = ";"), NA)
    
    data <- data.frame(login_username = input$login_username, date_time = Sys.time(), social_scale = social_scales[num], class_A = class_A_res, class_B = class_B_res, class_C = class_C_res, class_D = class_D_res) 
    
    if(length(csvs) > 0){
      
      prev.data <- do.call(rbind, lapply(csvs, read.csv, stringsAsFactors = FALSE, colClasses = c( "character", "POSIXct", "character", "character", "character", "character", "character")))
      
      data <- rbind(prev.data, data)
      file.name <- csvs[1]
      
    } else file.name <- paste0(data$login_username[1], "-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    
    write.csv(x = data, file = file.name, row.names = FALSE)
    
  }
  
  ######## Geovany's code to render drap and drop images ##########
  
  # location of images
  imagesDir <- "./images"
  # web directory
  imagesDirWeb <- "imgs"
  
  # make the directory web accesible
  addResourcePath(imagesDirWeb, imagesDir)
  
  # name of file with list of images (a data frame with one column of images)
  imgList <- "images.csv"
  
  # display a set of images
  showImages <- function(fileName, num) {
    
    # read in data frame with an images column
    allImgs <- read.csv(fileName)
    
    # subset the images by a single social scale
    # this social scale is indexed by the reactive page value that should be updated every time the user clicks the Next button
    allImgs <- allImgs[grep(social_scales[num], allImgs$scale), ]
    
    # randomly sample the spectrogram names to the ensure they are presented randomly
    spectros <- sample(as.character(allImgs$images), replace = FALSE)
    
    lapply(1:nrow(allImgs), function(i) {
      srcName <- paste0(imagesDirWeb, '/', spectros[i])
      img(src = srcName, data = spectros[i], class = "grid-square")
    })
  }
  
  # create a container of draggable elements in a grid 
  createGridDrag <- function(id, ...) {
    # to call the renderValue function in drag_drop_binding.js
    output[[id]] <- function(){}
    div(id = id, class = "grid_drag_drop group_border", ... )
  }
  
  
  ################## Text output to be used in UI pages ##################
  
  # Make text output with some basic instructions for login_UI
  output$login_info <- renderText({
    "To get started, enter your name and the password, then click the Log In button. If you successfully logged in, you will see an App button appear. Click the App button to begin classifying calls."
  })
  
  # Make text output with some basic instructions for main_UI
  output$info_header <- renderText({
    "How to classify calls by acoustic similarity: "
  })
  
  output$info1 <- renderText({
    "Spectrograms are visual representations of acoustic signals that account for the 3 domains of sound (time, frequency, amplitude). In the spectrogram below, the x-axis represents time and the y-axis represents frequency. The z-axis coming out of the screen represents amplitude. Darker parts of the spectrogram are therefore louder. You can use visible patterns of changes in frequency over time to assess the similarity of different spectrograms."
  })
  
  output$info2 <- renderText({
    "Place spectrograms into classes based on visible patterns of similarity (maximum of 4 classes). The total number of spectrograms should be the same across classes. Hit the Next Button when you've finished to move onto the next set of spectrograms."
  })

  # Text output to thank and inform user for thanks_UI
  output$thanks1 <- renderText({
    "Thank you for your time classifying these calls!"
  })
  
  output$thanks2 <- renderText({
    "You've contributed to our ongoing research with monk parakeets. This research is challenging our expectations that, as vocal learners, parrots should converge on shared calls within social group(s)."
  })
  
  output$photo_cred <- renderText({
    "@ Tania Molina"
  })
  
  output$close_window <- renderText({
    "Your classifications have been saved to this machine. You can close the app by clicking the Close Window button and then exit out of the browser window."
  })
  
  
  ################################ UI pages ###############################
  # Here, set up the server over multiple pages, in which each page is a different social scale
  
  # login page as server output
  login_UI <- function(){
    
    fluidPage(
      
      # add custom CSS and javascript code
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(type = "text/javascript", src = "Sortable.js"),
        tags$script(type = "text/javascript", src = "drag_drop_binding.js")
      ),
      
      div(class = "title-panel",
          panel(
            
            div(h1(class = "titleG",
                   "Classify contact calls"
            )),
            
            div(class = "sub-title-1",
                "Patterns of acoustic similarity in monk parakeet contact calls"
            )
          )),
      
      div(class = "sub-title-2",
          "App author:",
          a(href = "http://XXX.weebly.com", "XXX")
          ),
      
      div(class = "sub-title-2",
          "E-mail: XXX"
      ),
      
      br(),
      
      div(class = "sub-title-2",
          "Modified from original drag and drop code provided by",
          a(href = "http://geoabi.com/", "Dr. Geovany Ramirez"),
          "and multi-page code provided by", a(href = "http://XXX.weebly.com", "Dr. XXX,"), "originally from", a(href = "https://github.com/jaehyeon-kim/shiny-multipage", "Jaehyeon Kim's"), "multi-page Shiny app example.", a(href = "https://XXX.github.io/website", "XXX"), "provided CSS support."
      ),
      
      br(),
      
      fluidRow(
        column(12, h4(textOutput("login_info"), align = "left"))
      ),
      
      br(),
      
      fluidRow(
        
        useShinyjs(),
        
        # css hex color - http://www.w3schools.com/cssref/css_colors.asp
        tags$head(tags$style(HTML(".container-fluid {margin: 50px;} #login_link {float:right;} #login_login {color: #006600;} #next {color: #006600;} #login_application {color: #0000ff;} #submit {color: #006600;}"))),
        tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} "))),
        tags$script('$("#login_username").focus();'),
        column(6, offset = 3,
               wellPanel(
                 br(),
                 h4("LOGIN"),
                 textInput("login_username", "Name"),
                 div(class = "input_msg", textOutput("login_username_msg")),
                 # textInput("email", "email address"),
                 # div(class = "input_msg", textOutput("email_msg")),
                 # selectInput(inputId = "experience",label =  "Experience working with spectrograms", c("", "None", "Little", "Fair", "Good", "Expert") ),
                 # div(class = "input_msg", textOutput("experience_msg")),
                 passwordInput("login_password", "Password"),
                 div(class = "input_msg", textOutput("login_password_msg")),
                 actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px", style="color: #2e6da4"),
                 # actionButton("login_register", "Register", icon = icon("user-plus"), width = "100px"),
                 br(),
                 div(class = "input_fail", textOutput("login_fail")),
                 uiOutput("login_more")
               )
        )
      )
      
    )
  }
  
  # make a page with an example spectrogram
  spectro_intro_UI <- function(){
    
    fluidPage(
      
      # add custom CSS and javascript code
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(type = "text/javascript", src = "Sortable.js"),
        tags$script(type = "text/javascript", src = "drag_drop_binding.js")
      ),
      
      div(class = "title-panel",
          panel(
            
            div(h1(class = "titleG",
                   "Classify contact calls"
            )),
            
            div(class = "sub-title-1",
                "Patterns of acoustic similarity in monk parakeet contact calls"
            )
          )),
      
      br(),
      br(),
    
      fluidRow(
        column(12, h4(textOutput("info_header"), align = "left"))
      ),
      
      fluidRow(class = "sub-title-2",
        column(12, h4(textOutput("info1"), align = "left"))
      ),
      
      br(),
      
      # insert an image of a spectrogram with axes
      fluidRow(
        column(12, img(src = "2017_10_17_PAVO_1506.WAV_3-1.jpeg", width = "750px", height = "600px"), align = "center")
      ),
      
      fluidRow(
        column(3, 
               actionButton("next", "Next", width = "120px", icon = icon("arrow-right"), style = "color: #006400; background-color: #CDECD4; border-color: #006400")),
        column(9)
      )
      
    )
    
  }
  
    # main UI that presents blinded spectrograms over social scales (social scales and calls within them are presented at random with every iteration of the app)
    main_UI <- function(num){
      
     # create output page number for conditional panel JS expression
      # if(num < 6) {
        output$continue <- reactive({ n$page })
      # } else if(num == 6){
        # output$continue <- reactive({ n$page })
      # }
        
        fluidPage(
          
          # add custom CSS and javascript code
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            tags$script(type = "text/javascript", src = "Sortable.js"),
            tags$script(type = "text/javascript", src = "drag_drop_binding.js")
          ),
          
          fluidRow(
            column(12, h4(textOutput("info2"), align = "center"))
          ),
          
          br(),
          br(),
          
          div(
            fluidRow(
              column(offset = 1, 10, 
                     createGridDrag(id = "class_0",
                                    showImages(imgList, num = num)
                     )
              )
            ),
            
            hr(),
            fluidRow(
              column(3, h3("Class A", style = "text-align: center;"),
                     createGridDrag(id = "class_A")
              ),
              column(3, h3("Class B", style = "text-align: center;"),
                     createGridDrag(id = "class_B")
              ),
              column(3, h3("Class C", style = "text-align: center;"),
                     createGridDrag(id = "class_C")
              ),
              column(3, h3("Class D", style = "text-align: center;"),
                     createGridDrag(id = "class_D")
              )
            ),
            
            br(),
            br(),
            
            fluidRow(
              column(3, 
                     # couldn't JS expressions workin for conditional panels
                     if(n$page < (length(social_scales) + 2)){
                       actionButton("next", "Next", width = "120px", icon = icon("arrow-right"), style = "color: #006400; background-color: #CDECD4; border-color: #006400")
                     } else if(n$page == (length(social_scales) + 2)){
                       actionButton("finish", "Finish", width = "120px", icon = icon("arrow-right"), style = "color: #006400; background-color: #CDECD4; border-color: #006400")
                     }
                     ),
                     
                    
              column(9)
            )
          )
        )
    }

  
  # thanks_UI to thank and inform observer once all the social scales have been processed
  thanks_UI <- function(){
    
    fluidPage(
      
      div(class = "title-panel",
          panel(
            fluidRow(
              column(12, h3(textOutput("thanks1"), align = "center"))
              )
          )
      ),
      
      div(class = "sub-title-2",
          fluidRow(
            column(12, h3(textOutput("thanks2"), align = "center"))
          )
      ),
      
      div(class = "sub-title-2",
          fluidRow(
            column(12, h3(textOutput("close_window"), align = "center"))
          )
      ),
      
      br(),
      br(),
      
      # insert a photo of the monk parakeets
      fluidRow(
        
        column(12, img(src = 'TAN_1990.JPG', width = "800px", height = "500px"), align = "center"),
        
        fluidRow(
          column(8),
          column(4, textOutput("photo_cred"), align = "center")
        )
        
      ),
      
      # add an action button to close app
      fluidRow(
        column(5, 
              actionButton("close", "Close Window", width = "150px", icon = icon("window-close"), style = "color: #006400; background-color: #CDECD4; border-color: #006400")
        ),
        column(7)
      )
      
    )
      
  }
  
  ### Functions, reactives, observers to navigate through pages and save data ###
  
  # make a reactive page value to move through social scales, which starts at 1 (will need to subtract from the reactive page value)
  # 1 will be the intro page with the example spectrogram
  n <- reactiveValues(page = 1)
  
  # create a function to help navigate forwards, by adding on to the reactive page value initialized above
  navPage <- function(direction) {
    n$page <- n$page + direction
  }
  
  # make a function to render the UI pages
  render_page <- function(..., f) {
    ui_page <- f(...)
    renderUI({
      ui_page
    })
  }
  
  # Render the login page first
  output$page <- render_page(f = login_UI)
  
  # Make an observer to render the introduction page when the App button is clicked
  observeEvent(input$'login_application', {
    
    output$page <- render_page(f = spectro_intro_UI)
    navPage(1)
    
  })
  
  # Make an observer to remove any unfinished .csv files from the working directory, relies on the app developer initializing which .csv files should not be removed
  observe({
    
    if(n$page == 1){
  
      csvs <- list.files(pattern = ".csv$")[-grep(paste(do_not_remove, collapse = "|"), list.files(pattern = ".csv$"))]
      
      if(length(csvs) > 0){
        file.remove(file.path(path = in_path, csvs))
      }
      
    }
    
  })
  
  # Make an observer to update the reactive page number, then render the main_UI page with the correct social scale when the Next button is clicked, and also save user inputs to a csv
  # If this is the last social scale to be presented, the thanks_UI will be rendered instead
  observeEvent(input$'next', {
    
    # if the reactive page number is 0, render the main_UI with the first social scale
    if(n$page == 2){

      navPage(1)
      output$page <- render_page(f = main_UI, num = isolate(n$page) - 2)
      
    # if on subsequent pages, the reactive value has already been updated
    # render the subsequent page, save data and move the reactive page value forwards again
  } else if(n$page >= 3 & n$page <= (length(social_scales) + 1)){
        
      saveData(num = isolate(n$page) - 2)
      navPage(1)
      output$page <- render_page(f = main_UI, num = isolate(n$page) - 2)
      
    # once at the number of social scales, render the thanks_UI prompting the user to close the window
      # also move the final csv into the classifications folder
    } else if(n$page == (length(social_scales) + 2)){
      
      saveData(num = isolate(n$page) - 2)
      
    }
  
  })
    
  observeEvent(input$'finish', {
    
      saveData(num = isolate(n$page) - 2)
      output$page <- render_page(f = thanks_UI)
      
      csvs <- list.files(pattern = ".csv$")[-grep(paste(do_not_remove, collapse = "|"), list.files(pattern = ".csv$"))]
      
      file.rename(from = file.path(path = in_path, csvs), to = file.path(path = out_path, csvs))
    
  })
  
  # An observer to print the page number when clicking Next (testing)
  # observe({
  #     num <- isolate(n$page) - 2
  #     cat(paste("\n", paste(n$page, num, social_scales[num], sep = " - "), "\n"))
  # })
  
  # Make an observer to close the app when appropriate
  observeEvent(input$close, {

     stopApp()
  })
  
}
