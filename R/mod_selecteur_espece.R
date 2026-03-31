#' selecteur_espece UI Function
#'
#' @description Module UI pour la sélection d'une espèce.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS uiOutput
mod_selecteur_espece_ui <- function(id){
    ns <- NS(id)
    uiOutput(ns("espece"))
}

#' selecteur_espece Server Functions
#'
#' @description Module serveur pour la gestion dynamique du sélecteur d'espèces.
#'
#' @param id Identifiant du module.
#' @param variable Réactif contenant la variable d'intérêt.
#' @param bassin Réactif contenant les bassins.
#' @param departements Réactif contenant les départements.
#'
#' @noRd 
#' @importFrom dplyr filter distinct arrange pull
#' @importFrom tidyr drop_na
#' @importFrom shiny moduleServer observe req renderUI selectInput reactive
mod_selecteur_espece_server <- function(id, variable, bassin, departements){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        
        observe({
            req(variable, bassin, departements)
            
            sel_var <- variable()
            sel_bassin <- bassin()
            sel_dept <- departements()
            
            liste_especes <- carte_operations |> 
                dplyr::filter(dh_libelle %in% sel_bassin) |> 
                dplyr::filter(dept_id %in% sel_dept) |> 
                dplyr::distinct(esp_code_alternatif) |> 
                dplyr::collect() |> 
                tidyr::drop_na() |> 
                dplyr::arrange(esp_code_alternatif) |> 
                dplyr::pull(esp_code_alternatif)
                
                if (sel_var == "distribution") {
                    output$espece <- renderUI({
                        selectInput(
                            ns("espece"),
                            "Espèce",
                            c("Choisir une espèce: " = "", liste_especes)
                        )
                    })
                } else {
                    output$espece <- renderUI({})
                }   
            })
        
        return(reactive({input$espece}))
    })
    
    
}

## To be copied in the UI
# mod_selecteur_variable_ui("selecteur_variable_1")

## To be copied in the server
# mod_selecteur_variable_server("selecteur_variable_1")
