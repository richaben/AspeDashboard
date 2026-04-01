#' graphe_ipr UI Function
#'
#' @description Module UI pour l'affichage du graphique IPR.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput
mod_graphe_ipr_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("graphe"))
  )
}
    
#' graphe_ipr Server Functions
#'
#' @description Module serveur pour la génération du graphique IPR.
#'
#' @param id Identifiant du module.
#' @param departement Réactif contenant le ou les départements sélectionnés.
#' @param bassin Réactif contenant le ou les bassins sélectionnés.
#' @param periode Réactif contenant la période sélectionnée (range).
#'
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom ggplot2 theme element_line element_text scale_x_continuous scale_y_continuous
#' @importFrom templatesOFB theme_ofb int_breaks int_limits
#' @importFrom shiny moduleServer renderPlot
mod_graphe_ipr_server <- function(id, departement, bassin, periode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # Données de base (Filtres spatiaux - Arrow)
    DataBase_r <- reactive({
        sel_bassin <- bassin()
        sel_dept <- departement()
        
        res <- ipr |> 
            dplyr::filter(
                dh_libelle %in% sel_bassin,
                dept_id %in% sel_dept
            ) |> 
            dplyr::collect()
        gc()
        res
    })

    # Données filtrées par période (Filtre temporel - R en mémoire)
    DonneesGraphe <- reactive({
        req(DataBase_r(), periode())
        sel_per <- periode()
        min_per <- min(sel_per)
        max_per <- max(sel_per)
        
        DataBase_r() |> 
            dplyr::filter(
                annee >= min_per,
                annee <= max_per
            )
    })
    
    output$graphe <- renderPlot({
        graphe_ipr(donnees = DonneesGraphe()) +
            templatesOFB::theme_ofb() +
            ggplot2::theme(
                panel.grid.major.y = ggplot2::element_line(colour = "grey"),
                strip.text = ggplot2::element_text(size = 12, face = "bold"),
                axis.text = ggplot2::element_text(size = 10)
            ) +
            ggplot2::scale_x_continuous(
                breaks = templatesOFB::int_breaks,
                limits = templatesOFB::int_limits
            ) +
            ggplot2::scale_y_continuous(
                breaks = templatesOFB::int_breaks,
                limits = templatesOFB::int_limits
            )
    })
  })
}
    
## To be copied in the UI
# mod_graphe_ipr_ui("graphe_ipr_1")
    
## To be copied in the server
# mod_graphe_ipr_server("graphe_ipr_1")
