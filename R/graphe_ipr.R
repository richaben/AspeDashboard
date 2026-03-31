#' Graphe de synthèse de l'IPR
#'
#' @description Génère un graphique montrant l'évolution du nombre de stations 
#' et du pourcentage par classe d'IPR au fil des années.
#'
#' @param donnees Un data.frame contenant les données d'IPR (doit inclure les 
#' colonnes `annee`, `cli_libelle`, `pop_id`).
#'
#' @return Un objet ggplot2 avec deux facettes (nombre et pourcentage).
#' @export
#'
#' @importFrom dplyr group_by summarise n_distinct mutate ungroup left_join filter distinct
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_identity facet_wrap vars labs
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
graphe_ipr <- function(donnees) {
    labels <- c(
        "nb_pop" = "Nombre de stations",
        "p_pop" = "Pourcentage de stations"
        )
        
    if (nrow(donnees) > 0)
        donnees |> 
            dplyr::group_by(annee, cli_libelle) |> 
            dplyr::summarise(nb_pop = dplyr::n_distinct(pop_id), .groups = "drop") |>
            dplyr::group_by(annee) |> 
            dplyr::mutate(p_pop = 100 * nb_pop / sum(nb_pop)) |> 
            dplyr::ungroup() |> 
            tidyr::pivot_longer(
                cols = c(nb_pop, p_pop)
            ) |> 
            dplyr::left_join(
                carte_operations |> 
                    dplyr::filter(variable == "ipr") |> 
                    dplyr::distinct(valeur, couleur) |> 
                    dplyr::collect(),
                by = c("cli_libelle" = "valeur")
            ) |> 
            dplyr::mutate(
                couleur = stringr::str_replace_na(couleur, "#CCCCCC") # Sécurité pour les NA
            ) |> 
            dplyr::mutate(
                name = unname(labels[name])
            ) |> 
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = annee, y = value, fill = couleur
                )
            ) +
            ggplot2::geom_col() +
            ggplot2::scale_fill_identity() +
            ggplot2::facet_wrap(ggplot2::vars(name), nrow = 1, scales = "free_y") +
            ggplot2::labs(x = "", y = "")
}
