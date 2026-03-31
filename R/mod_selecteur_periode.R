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
      sliderInput(
          inputId = ns("periode"),
          label = "Choisir la période",
          min = captures |> 
            dplyr::distinct(annee) |> 
            dplyr::pull(annee, as_vector = TRUE) |> 
            min(),
          max = captures |> 
            dplyr::distinct(annee) |> 
            dplyr::pull(annee, as_vector = TRUE) |> 
            max(),
          value = captures |> 
            dplyr::distinct(annee) |> 
            dplyr::pull(annee, as_vector = TRUE) |> 
            range(),
          round = TRUE,
          sep = "",
          ticks = FALSE,
          animate = TRUE
      )
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
    
    observe({
        req(bassin, departement, input$periode)
        
        sel_bassin <- bassin()
        sel_dept <- departement()
        
        DataPeriode <- captures |> 
            dplyr::filter(
                dh_libelle %in% sel_bassin,
                dept_id %in% sel_dept
            ) |> 
            dplyr::select(annee) |> 
          dplyr::collect()
        

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
