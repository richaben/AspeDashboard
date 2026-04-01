
#' Title
#'
#' @param texte 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr set_names
ajuster_html <- function(texte) {
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
    
    codes_especes <- aspe::data_passerelle_taxo$esp_code_taxref |>
        purrr::set_names(aspe::data_passerelle_taxo$esp_code_alternatif)
    
    
    for (code_alternatif in names(codes_especes)) {
        if (
            any(
                stringr::str_detect(
                    string = text_out,
                    pattern = code_alternatif
                )
            )
        ) {
            text_out <- stringr::str_replace_all(
                string = text_out,
                pattern = paste0(
                    ">", code_alternatif, "<\\\\/text>"
                ),
                replacement = paste0(
                    "><a href='https://inpn.mnhn.fr/espece/cd_nom/",
                    codes_especes[[code_alternatif]],
                    "' target='_blank'>",
                    code_alternatif, "</a></text>"
                )
            )
        }
    }    
    text_out
}

