
#' Ajouter un lien vers l'INPN
#'
#' @description Crée un lien HTML vers la fiche espèce sur le site de l'INPN 
#' à partir du code alternatif de l'espèce.
#'
#' @param codes_alternatif Un vecteur de codes alternatifs d'espèces (ex: "GOU").
#'
#' @return Un vecteur de chaînes de caractères contenant les liens HTML.
#' @export
#'
#' @importFrom purrr map_chr
ajouter_lien_inpn <- function(codes_alternatif){

    purrr::map_chr(
        codes_alternatif,
        function(code_alternatif) {
            # On vérifie si le code existe dans la liste de correspondance
            if (code_alternatif %in% names(codes_especes)) {
                paste0(
                    "<a href='https://inpn.mnhn.fr/espece/cd_nom/",
                    codes_especes[[code_alternatif]],
                    "' target='_blank'>", 
                    code_alternatif, "</a>"
                )
            } else {
                # Si pas de correspondance, on retourne juste le code sans lien
                code_alternatif
            }
        }
    )
}
