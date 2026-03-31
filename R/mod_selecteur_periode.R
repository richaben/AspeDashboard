#' selecteur_periode UI Function
#'
#' @description Module UI pour la sélection de la période temporelle.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList sliderInput uiOutput
mod_selecteur_periode_ui <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(ns("slider_periode"))
  )
}
    
#' selecteur_periode Server Functions
#'
#' @description Module serveur pour la gestion du sélecteur de période.
#'
#' @param id Identifiant du module.
#' @param bassin Réactif contenant les bassins.
#' @param departement Réactif contenant les départements.
#'
#' @noRd 
#' @importFrom dplyr filter select collect
#' @importFrom shiny moduleServer observe req updateSliderInput reactive renderUI sliderInput
mod_selecteur_periode_server <- function(id, bassin, departement){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Calcul des bornes initiales de manière sécurisée (une seule fois)
    LimitesInitiales <- reactive({
        captures |> 
            dplyr::select(annee) |> 
            dplyr::summarise(
                min_an = min(annee, na.rm = TRUE),
                max_an = max(annee, na.rm = TRUE)
            ) 
    })
    
    output$slider_periode <- renderUI({
        lim <- LimitesInitiales()
        
        sliderInput(
            inputId = ns("periode"),
            label = "Choisir la période",
            min = lim$min_an,
            max = lim$max_an,
            value = c(lim$min_an, lim$max_an),
            round = TRUE,
            sep = "",
            ticks = FALSE,
            animate = TRUE
        )
    })
    
    observe({
        req(bassin, departement, input$periode)
        
        sel_bassin <- bassin()
        sel_dept <- departement()
        
        DataPeriode <- captures |> 
            dplyr::filter(
                dh_libelle %in% sel_bassin,
                dept_id %in% sel_dept
            ) |> 
            dplyr::select(annee) 
        

        if (nrow(DataPeriode) > 0) {
            updateSliderInput(
                session = session,
                inputId = "periode",
                label = "Choisir la période",
                value = range(DataPeriode$annee),
                min = min(DataPeriode$annee),
                max = max(DataPeriode$annee)
            )
        }
        
        })
    
    reactive({input$periode})
 
  })
}
    
## To be copied in the UI
# mod_selecteur_periode_ui("selecteur_periode_1")
    
## To be copied in the server
# mod_selecteur_periode_server("selecteur_periode_1")
