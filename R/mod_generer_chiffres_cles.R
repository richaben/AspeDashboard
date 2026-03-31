#' generer_chiffres_cles UI Function
#'
#' @description Module UI pour l'affichage des chiffres clés.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS fluidPage tags HTML uiOutput
#' @importFrom shiny.semantic cards card
#' @importFrom shinydashboardPlus accordion accordionItem
mod_generer_chiffres_cles_ui <- function(id){
    ns <- NS(id)
    
    
    fluidPage(
        tags$head(
            tags$style(HTML("
            .panel {
                margin-bottom: 10px;
                border: 0;
                background-color: transparent;
            }
            .box-title {
                font-size: 14px;
            }
            .col-sm-12 {
                padding-left: 0px;
                padding-right: 0px;
            }
            "
        ))),
        uiOutput(ns("chiffres_cle"))
    )
    
}
    
#' generer_chiffres_cles Server Functions
#'
#' @description Module serveur pour le calcul et l'affichage des chiffres clés.
#'
#' @param id Identifiant du module.
#' @param variable Réactif contenant la variable d'intérêt ("ipr" ou "especes").
#' @param departement Réactif contenant le ou les départements sélectionnés.
#' @param bassin Réactif contenant le ou les bassins sélectionnés.
#' @param periode Réactif contenant la période sélectionnée (range).
#'
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom shiny moduleServer renderUI req tagList div
mod_generer_chiffres_cles_server <- function(id, variable, departement, bassin, periode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    CardStyle <- "
  min-height: 45px;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: left;
  text-align: left;
  padding-top: 5px;
  padding-bottom:5px;
  padding-left: 10px;
  margin-top: 20px;
  margin-bottom: -10px;
  background-color: #8DB6CD;
  border-radius: 5px;
  "
    
    output$chiffres_cle <- renderUI({
        req(variable(), departement)
        
        sel_var <- variable()
        sel_dept <- departement()
        sel_bassin <- bassin()
        sel_per <- periode()
        min_per <- min(sel_per)
        max_per <- max(sel_per)
        
        if (sel_var %in%  c("especes", "distribution"))
            donnees <- captures |>
                dplyr::filter(
                    dept_id %in% sel_dept,
                    dh_libelle %in% sel_bassin,
                    annee >= min_per,
                    annee <= max_per
                    ) 
        
        if (sel_var == "ipr")
            donnees <- ipr |>
                dplyr::filter(
                    dept_id %in% sel_dept,
                    dh_libelle %in% sel_bassin,
                    annee >= min_per,
                    annee <= max_per
                    ) 
        
        indicateurs <- calculer_chiffres_cles(donnees, variable())

        tagList(
            div(),
            shiny.semantic::cards(
                class = "one",
                shiny.semantic::card(
                    HTML(indicateurs$un),
                    style = CardStyle
                    ),
                shiny.semantic::card(
                    HTML(indicateurs$deux),
                    style = CardStyle
                )
            ),
            div(),
            if(variable() == "especes") {
                shiny.semantic::card(
                    shinydashboardPlus::accordion(
                        id = "accordion1",
                        shinydashboardPlus::accordionItem(
                            title = HTML(indicateurs$trois$texte),
                            collapsed = TRUE,
                            HTML(indicateurs$trois$tooltip)
                        ),
                        shinydashboardPlus::accordionItem(
                            title = HTML(indicateurs$quatre$texte),
                            collapsed = TRUE,
                            HTML(indicateurs$quatre$tooltip)
                        )
                    ),
                    style = CardStyle
                )
            }

        )
        
    })
  })
}
    
## To be copied in the UI
# mod_generer_chiffres_cles_ui("generer_chiffres_cles_1")
    
## To be copied in the server
# mod_generer_chiffres_cles_server("generer_chiffres_cles_1")
