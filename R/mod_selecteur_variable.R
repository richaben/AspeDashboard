#' selecteur_variable UI Function
#'
#' @description Module UI pour la sélection de la variable d'intérêt.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectInput
mod_selecteur_variable_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectInput(
          inputId = ns("variable"),
          label = "Variable d'intérêt",
          choices = c(
              "Composition taxonomique" = "especes",
              "Indice Poisson Rivière" = "ipr",
              "Distribution" = "distribution"
          ),
          multiple = FALSE
      )
  )
}
    
#' selecteur_variable Server Functions
#'
#' @description Module serveur pour la gestion du sélecteur de variable.
#'
#' @param id Identifiant du module.
#'
#' @noRd 
#' @importFrom shiny moduleServer reactive
mod_selecteur_variable_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    reactive({
        input$variable
        })
  })
}
    
## To be copied in the UI
# mod_selecteur_variable_ui("selecteur_variable_1")
    
## To be copied in the server
# mod_selecteur_variable_server("selecteur_variable_1")
