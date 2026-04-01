#' Générer un popup pour la composition taxonomique
#'
#' @param df_captures Un data.frame contenant les données de capture pour un point.
#' @param pop_id_sel L'identifiant du point de prélèvement.
#' @param pop_libelle_sel Le libellé du point de prélèvement.
#'
#' @return Un widget interactif (ggiraph).
#' @export
popup_peuplement <- function(df_captures, pop_id_sel, pop_libelle_sel) {
  if (nrow(df_captures) == 0) return(NULL)
  
  # Largeur et hauteur de base (inch)
  largeur_base <- 3.7
  hauteur_base <- 4.6
  
  p <- aspe::gg_temp_peuplement(
    df = df_captures,
    var_id_sta = pop_id,
    var_libelle_sta = pop_libelle,
    longueur_libelle = 30,
    interactif = FALSE,
    largeur = largeur_base,
    hauteur = hauteur_base,
    rescale = TRUE
  )
  
  # Si aspe renvoie une liste, on extrait le graphique
  if (is.list(p) && !inherits(p, "ggplot")) p <- p[[1]]
  
  # Réduction des marges (inspiration prep_sauver_popups)
  p <- p + ggplot2::theme(plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  # Configuration du redimensionnement (inspiration adjust_popups)
  # On utilise les dimensions augmentées comme dans PreparationDonnees.qmd
  p <- ggiraph::girafe(
    ggobj = p,
    width = "auto",
    height = "auto",
    width_svg = largeur_base * 1.25,
    height_svg = hauteur_base * 1.25 + 0.66,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_tooltip(css = "background-color:white; color:black; border:1px solid black; padding:5px; border-radius:5px;")
    )
  )
  
  # Ajout d'un style CSS pour supprimer les marges du body dans l'iframe
  p$dependencies <- c(
    p$dependencies,
    list(htmltools::htmlDependency(
      name = "popup-style",
      version = "1.0.0",
      src = c(href = ""),
      head = "<style>body { margin: 0; padding: 0; }</style>"
    ))
  )
  
  p
}

#' Générer un popup pour l'IPR
#'
#' @param df_ipr Un data.frame contenant les données IPR pour un point.
#' @param pop_id_sel L'identifiant du point de prélèvement.
#' @param pop_libelle_sel Le libellé du point de prélèvement.
#' @param classe_ipr Le référentiel des classes IPR.
#'
#' @return Un widget interactif (ggiraph).
#' @export
popup_ipr <- function(df_ipr, pop_id_sel, pop_libelle_sel, classe_ipr) {
  if (nrow(df_ipr) == 0) return(NULL)
  
  is_sup_500m <- unique(df_ipr$sup_500m)
  
  # Largeur et hauteur de base (inch)
  largeur_base <- 3.7
  hauteur_base <- 4.6
  
  p <- aspe::gg_temp_ipr(
    df_ipr = df_ipr,
    var_ipr = ipr,
    var_id_sta = pop_libelle,
    station_sel = pop_libelle_sel,
    sup_500m = is_sup_500m,
    max_axe_y = 50,
    interactif = FALSE,
    largeur = largeur_base,
    hauteur = hauteur_base,
    titre_graphique = "",
    titre_y = "Valeur d'IPR",
    df_classes = classe_ipr
  )
  
  # Si aspe renvoie une liste, on extrait le graphique
  if (is.list(p) && !inherits(p, "ggplot")) p <- p[[1]]
  
  # Réduction des marges
  p <- p + ggplot2::theme(plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  # Configuration du redimensionnement
  p <- ggiraph::girafe(
    ggobj = p,
    width = "auto",
    height = "auto",
    width_svg = largeur_base * 1.25,
    height_svg = hauteur_base * 1.25 + 0.66,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_tooltip(css = "background-color:white; color:black; border:1px solid black; padding:5px; border-radius:5px;")
    )
  )
  
  # Ajout d'un style CSS pour supprimer les marges du body dans l'iframe
  p$dependencies <- c(
    p$dependencies,
    list(htmltools::htmlDependency(
      name = "popup-style",
      version = "1.0.0",
      src = c(href = ""),
      head = "<style>body { margin: 0; padding: 0; }</style>"
    ))
  )
  
  p
}
