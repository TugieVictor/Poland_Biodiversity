ui <- fluidPage(
  titlePanel(
  navbarPage(title = span( "Poland Biodiversity Dashboard", style = "color: #026352" )
  )),
  
  tags$style(HTML("
  body {
    background-color: #EAEAEA;
  }")),
  
  module_ui("search")
  
)