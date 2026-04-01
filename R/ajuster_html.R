
#' Title
#'
#' @param texte 
#' @param codes_especes_a_traiter Optionnel: vecteur de codes alternatifs à traiter. 
#'     Si NULL (défaut), tous les codes connus sont testés.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr set_names
ajuster_html <- function(texte, codes_especes_a_traiter = NULL) {
    text_out <- texte
    
    text_out <- text_out |>
        stringr::str_replace_all(
            pattern = "\\\"padding\\\":40,\\\"fill\\\":false",
            replacement = "\\\"padding\\\":0,\\\"fill\\\":false"
        ) |>
        stringr::str_replace_all(
            pattern = "\\\"padding\\\":15,\\\"fill\\\":true",
            replacement = "\\\"padding\\\":0,\\\"fill\\\":true"
        ) |>
        stringr::str_replace_all(
            pattern = "width:\\d+px;height:\\d+px;",
            replacement = "width:auto;height:auto;"
        ) |>
        stringr::str_replace_all(
            pattern = "\"browser\":\\{\"width\":\\d+,\"height\":\\d+",
            replacement = "\"browser\":{\"width\":\"auto\",\"height\":\"auto\""
        )
    
    # On récupère la table de correspondance complète
    tous_codes_especes <- aspe::data_passerelle_taxo$esp_code_taxref |>
        purrr::set_names(aspe::data_passerelle_taxo$esp_code_alternatif)
    
    # Si on a spécifié des codes à traiter, on filtre
    if (!is.null(codes_especes_a_traiter)) {
        # Suppression des NA éventuels pour éviter des erreurs dans la boucle for
        codes_especes_a_traiter <- codes_especes_a_traiter[!is.na(codes_especes_a_traiter)]
        codes_a_boucler <- tous_codes_especes[names(tous_codes_especes) %in% codes_especes_a_traiter]
    } else {
        codes_a_boucler <- tous_codes_especes
    }
    
    # On s'assure que les noms ne contiennent pas de NA
    codes_a_boucler <- codes_a_boucler[!is.na(names(codes_a_boucler))]
    
    for (code_alternatif in names(codes_a_boucler)) {
        if (
            any(
                stringr::str_detect(
                    string = text_out,
                    pattern = stringr::fixed(code_alternatif)
                ),
                na.rm = TRUE
            )
        ) {
            text_out <- stringr::str_replace_all(
                string = text_out,
                pattern = stringr::fixed(paste0(
                    ">", code_alternatif, "<\\\\/text>"
                )),
                replacement = paste0(
                    "><a href='https://inpn.mnhn.fr/espece/cd_nom/",
                    codes_a_boucler[[code_alternatif]],
                    "' target='_blank'>",
                    code_alternatif, "</a></text>"
                )
            )
        }
    }    
    text_out
}

