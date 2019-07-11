# Drag and drop images code from Dr. Geovany A. Ramirez, April 28, 2019
# Modified by XXX 06 May 2019

ui <- fluidPage(
  title = "Classify monk parakeet contact calls",

  # add custom CSS and javascript code
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(type = "text/javascript", src = "Sortable.js"),
    tags$script(type = "text/javascript", src = "drag_drop_binding.js")
  ),

  uiOutput("page")

)


