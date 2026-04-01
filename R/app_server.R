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

    # Création d'un répertoire temporaire pour les popups dynamiques
    # et ajout d'un chemin de ressource pour y accéder via le navigateur
    temp_popup_dir <- tempfile("popups")
    dir.create(temp_popup_dir, showWarnings = FALSE)
    shiny::addResourcePath("temp_popups", temp_popup_dir)
    cat(temp_popup_dir)
    cat("\n")
    
    # Nettoyage à la fin de la session
    session$onSessionEnded(function() {
        unlink(temp_popup_dir, recursive = TRUE)
    })
    
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
      temp_dir = temp_popup_dir
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
    
    output$date_export <- renderText(paste0("Export: ", as.character(date_export)))

    # onSessionEnded(fun = function() {
    #     unlink("inst/app/www/widgets/especes", recursive = TRUE)
    #     unlink("inst/app/www/widgets/ipr", recursive = TRUE)
    # })
    
}
