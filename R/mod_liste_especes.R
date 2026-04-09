#' liste_especes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_liste_especes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("liste_especes"))
  )
}
    
#' liste_especes Server Functions
#'
#' @noRd 
mod_liste_especes_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$liste_especes <- aspe::ref_espece |> 
      dplyr::inner_join(
        captures |> 
          dplyr::distinct(esp_code_alternatif) |> 
          dplyr::collect(),
        by = "esp_code_alternatif"
      ) |> 
      dplyr::select(
        `Code alternatif` = esp_code_alternatif,
        `Code Sandre` = esp_code_sandre,
        `Code TaxRef` = esp_code_taxref,
        `Nom scientifique` = esp_nom_latin,
        `Nom français` = esp_nom_commun
      ) |> 
      DT::datatable(
        filter = "top",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          serverSide = FALSE
        ),
        rownames = FALSE
      ) |> 
      DT::renderDataTable()
  })
}
    
## To be copied in the UI
# mod_liste_especes_ui("liste_especes_1")
    
## To be copied in the server
# mod_liste_especes_server("liste_especes_1")
