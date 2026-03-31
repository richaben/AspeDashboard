#' graphes_metriques UI Function
#'
#' @description Module UI pour l'affichage des graphiques de métriques détaillées.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput tags
mod_graphes_metriques_ui <- function(id){
  ns <- NS(id)
  tagList(
      tags$style(type = "text/css", paste0("#", ns("graphe"), " {height: calc(100vh - 210px) !important;}")),
      
      plotOutput(ns("graphe"))
  )
}
    
#' graphes_metriques Server Functions
#'
#' @description Module serveur pour la génération des graphiques de métriques.
#'
#' @param id Identifiant du module.
#' @param variable Réactif contenant la variable d'intérêt ("ipr", "especes", "distribution").
#' @param point Réactif contenant l'identifiant du point de prélèvement sélectionné.
#' @param departement Réactif contenant le ou les départements sélectionnés.
#' @param bassin Réactif contenant le ou les bassins sélectionnés.
#' @param periode Réactif contenant la période sélectionnée (range).
#' @param espece Réactif contenant l'espèce sélectionnée.
#'
#' @noRd 
#' @importFrom dplyr filter distinct mutate pull
#' @importFrom ggplot2 scale_x_continuous labs theme_minimal theme element_blank element_text scale_y_continuous
#' @importFrom templatesOFB int_breaks int_limits
#' @importFrom shiny moduleServer renderPlot req
mod_graphes_metriques_server <- function(id, variable, point, departement, bassin,  periode, espece){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$graphe <- renderPlot({
        req(variable, point, bassin, departement, espece)
        
        sel_var <- variable()
        sel_point <- point()
        sel_bassin <- bassin()
        sel_dept <- departement()
        sel_per <- periode()
        min_per <- min(sel_per)
        max_per <- max(sel_per)
        sel_esp <- espece()
        
        if (sel_var != "distribution") {
            if (!is.null(sel_point)) {
                SelectionMetriques <- metriques |> 
                    dplyr::filter(
                        variable == sel_var,
                        pop_id == sel_point,
                        annee >= min_per,
                        annee <= max_per
                    ) |> 
                    dplyr::collect()
                
                if (nrow(SelectionMetriques) == 0) {
                    NULL
                } else {
                    SelectionMetriques |> 
                        aspe::gg_temp_metriq_grille(
                            var_id_sta = pop_libelle,
                            var_nom_metrique = metrique,
                            var_valeur_metrique = valeur,
                            nb_colonnes = 2,
                            orientation = "v"
                        ) +
                        ggplot2::scale_x_continuous(
                            breaks = templatesOFB::int_breaks,
                            limits = templatesOFB::int_limits
                        ) +
                        ggplot2::labs(
                            title = SelectionMetriques |> 
                                dplyr::distinct(pop_libelle, ope_id) |> 
                                dplyr::mutate(
                                    titre = paste0(
                                        pop_libelle, " (", ope_id, ")"
                                    )
                                ) |> 
                                dplyr::pull(titre)
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            panel.grid.major.x = ggplot2::element_blank(),
                            panel.grid.minor.x = ggplot2::element_blank(),
                            panel.grid.minor.y = ggplot2::element_blank(),
                            plot.title = ggplot2::element_text(face = "bold")
                        )
                }
            }
        } else {
            graphe <- captures |> 
                dplyr::filter(
                    dh_libelle %in% sel_bassin,
                    dept_id %in% sel_dept,
                    annee >= min_per,
                    annee <= max_per
                ) |> 
                dplyr::collect() |> 
                graphe_synthese_espece(
                espece = sel_esp,
                station = sel_point
            ) +
                ggplot2::scale_x_continuous(
                    breaks = templatesOFB::int_breaks,
                    limits = templatesOFB::int_limits
                )  +
                ggplot2::scale_y_continuous(
                    breaks = templatesOFB::int_breaks,
                    limits = templatesOFB::int_limits
                ) +
                ggplot2::theme(
                    strip.text = ggplot2::element_text(hjust = 0, size = 12, face = "bold"),
                    axis.text = ggplot2::element_text(size = 10),
                    legend.text = ggplot2::element_text(size = 10)
                )
            
            graphe
        }

        
    })
    
  })
}
    
## To be copied in the UI
# mod_graphes_metriques_ui("graphes_metriques_1")
    
## To be copied in the server
# mod_graphes_metriques_server("graphes_metriques_1")
