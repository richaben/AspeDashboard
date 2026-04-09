#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom utils untar
#' @importFrom shinylogs track_usage store_json
#' @importFrom rdrop2 drop_auth drop_upload
#' @importFrom purrr walk
app_server <- function( input, output, session ) {
    # shinylogs::track_usage(
    #     storage_mode = shinylogs::store_json("logs/")
    # )
    # 
    # session$onSessionEnded(function() {
    #     rdrop2::drop_auth(
    #         rdstoken = "dropbox_token.rds"
    #     )
    #     
    #     list.files("logs") |>
    #         purrr::walk(
    #             function(log) {
    #                 rdrop2::drop_upload(
    #                     file = file.path("logs", log),
    #                     path = "shinyapps_logs",
    #                     mode = "add"
    #                 )
    #             }
    #         )
    # })
  
    SelectionBassin <- mod_selecteur_bassin_server(
        id = "bassin"
    )
    SelectionDepartement <- mod_selecteur_admin_server(
        id = "admin",
        bassin = SelectionBassin
    )
    SelectionPeriode <- mod_selecteur_periode_server(
        id = "periode",
        bassin = SelectionBassin,
        departement = SelectionDepartement
        )
    SelectionVariable <- mod_selecteur_variable_server(id = "var")
    SelectionEspece <- mod_selecteur_espece_server(
        id = "espece",
        variable = SelectionVariable, 
        bassin = SelectionBassin, 
        departements = SelectionDepartement
    )

    SelectionPoint <- mod_carte_op_server(
      id = "carte_op",
      departement = SelectionDepartement,
      bassin = SelectionBassin,
      variable = SelectionVariable,
      espece = SelectionEspece,
      periode = SelectionPeriode,
      temp_dir = file.path("inst", "app", "www", "popups"),
      session_id = session$token
  )

    mod_panneau_droit_server(
        id = "panneau_droit",
        variable = SelectionVariable,
        departement = SelectionDepartement,
        bassin = SelectionBassin,
        periode = SelectionPeriode,
        point = SelectionPoint,
        espece = SelectionEspece
    )
    
    mod_liste_especes_server(
      id = "liste_especes"
    )
    
    output$date_export <- renderText(paste0("Export: ", as.character(date_export)))

}
