#' Sélectionner les départements filtrés
#'
#' @description Retourne une liste nommée de départements filtrée par région 
#' et/ou bassin versant pour alimenter un sélecteur Shiny.
#'
#' @param region Vecteur des codes régions sélectionnés.
#' @param bassin Vecteur des libellés de bassins sélectionnés.
#'
#' @return Un vecteur nommé de codes départements.
#' @export
#'
#' @importFrom dplyr distinct pull filter mutate arrange
#' @importFrom purrr set_names
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr drop_na
selectionner_departement <- function(region, bassin) {
    if (length(region) == 0 & length(bassin) == 0) {
        c(
            "Choisir un département" = "",
            purrr::set_names(
                administratif |> 
                    dplyr::arrange(INSEE_DEP) |> 
                    dplyr::pull(INSEE_DEP) |> 
                    as.character(),
                administratif |> 
                    dplyr::arrange(INSEE_DEP) |> 
                    dplyr::pull(departement) |> 
                    as.character()
            )
        )
    } else {
        if (length(region) == 0)
            region <- unique(administratif$INSEE_REG)
        if (length(bassin) == 0)
            bassin <- pop_geo_df |> 
                dplyr::distinct(dh_libelle) |> 
                dplyr::collect() |> 
                tidyr::drop_na() |> 
                dplyr::pull(dh_libelle)
                
        SelectionPop <-  pop_geo_df |> 
            dplyr::distinct(dept_id, dept_libelle, reg_id, dh_libelle) |> 
            dplyr::collect() |> 
            dplyr::arrange(dept_id) |> 
            dplyr::filter(
                reg_id %in% region,
                dh_libelle %in% bassin
            ) |>
            dplyr::mutate(
                departement = paste0(dept_libelle, " (", dept_id, ")")
            )
        
        c(
            "Choisir un département" = "",
            purrr::set_names(
                SelectionPop |> 
                    dplyr::pull(dept_id) |> 
                    as.character(),
                SelectionPop |> 
                    dplyr::pull(departement)
            )
        )
    }
}

