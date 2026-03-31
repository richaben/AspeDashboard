#' panneau_droit UI Function
#'
#' @description Module UI pour le panneau latéral droit affichant les graphiques et indicateurs.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput
mod_panneau_droit_ui <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(ns("panneau"))
  )
}
    
#' panneau_droit Server Functions
#'
#' @description Module serveur pour la coordination de l'affichage dans le panneau droit.
#'
#' @param id Identifiant du module.
#' @param variable Réactif contenant la variable d'intérêt.
#' @param departement Réactif contenant les départements.
#' @param bassin Réactif contenant les bassins.
#' @param periode Réactif contenant la période.
#' @param point Réactif contenant le point cliqué.
#' @param espece Réactif contenant l'espèce sélectionnée.
#'
#' @noRd 
#' @importFrom shiny moduleServer renderUI tagList br
mod_panneau_droit_server <- function(id, variable, departement, bassin, periode, point, espece){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_generer_chiffres_cles_server(
        id = "chiffres_cles",
        variable = variable,
        departement = departement,
        bassin = bassin,
        periode = periode
    )
    
    mod_graphes_metriques_server(
        id = "graphe_metrique",
        variable = variable,
        point = point,
        departement = departement,
        periode = periode,
        bassin = bassin,
        espece = espece
    )
    
    mod_graphe_ipr_server(
        id = "graphe_ipr",
        departement = departement,
        bassin = bassin,
        periode = periode
    )

    output$panneau <- renderUI({
        
        if (variable() == "especes") {
            mod_generer_chiffres_cles_ui(id = ns("chiffres_cles"))
        } else {
            if (variable() == "ipr") {
                if (is.null(point())) {
                    tagList(
                        mod_generer_chiffres_cles_ui(id = ns("chiffres_cles")),
                        br(),
                        mod_graphe_ipr_ui(id = ns("graphe_ipr"))
                    )
                    
                } else {
                    tagList(
                        mod_graphes_metriques_ui(id = ns("graphe_metrique"))
                    )
                    
                }
            } else {
                if (variable() == "distribution") {
                    mod_graphes_metriques_ui(id = ns("graphe_metrique"))
                }
            }
        }
        
    })
    
  })
}
    
## To be copied in the UI
# mod_panneau_droit_ui("panneau_droit_1")
    
## To be copied in the server
# mod_panneau_droit_server("panneau_droit_1")
