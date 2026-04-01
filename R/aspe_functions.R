#' Graphique pour représenter l'évolution des effectifs de taxons au cours du temps
#'
#' @description Copie de la fonction du package aspe pour permettre des ajustements locaux.
#'
#' @param df Dataframe contenant les données des effectifs capturés pour les
#'  taxons.  Il doit contenir des variables "effectif" et "annee" ainsi qu'une
#'  variable permettant d'identifier la station ou le point de prélèvement. Il
#'  doit également contenir une variable 'pro_libelle' correspondant aux
#'  protocoles (à ajouter avec la \code{aspe::mef_ajouter_type_protocole()}).
#' @param var_id_sta Nom de la variable servant à identifier les stations ou
#'  points. Cette variable donnera les étiquettes du graphique.
#' @param var_libelle_sta Nom de la variable servant à identifier les libellés
#'  des stations ou points. Cette variable donnera les étiquettes du graphique.
#' @param var_especes Variable indiquant l'espèce ou le code espèce.
#' @param interactif Valeur logique: statique (FALSE) produit avec `ggplot2` ou
#'  interactif (TRUE) produit avec `ggiraph`.
#' @param largeur,hauteur Numériques. Dimensions des graphiques interactifs.
#' @param taxons_ipr Caractère. Indique comment distinguer sur le graphique les
#'  noms des espèces participant à l'IPR. Peut prendre les valeurs "bold",
#'  "italic", "bold.italic", ou par défaut "plain".
#' @param longueur_libelle Numérique. longueur maximale (en nombre de caractères)
#'  du titre du graphique
#' @param ... arguments passés à la fonction \code{ggiraph::opts_sizing()}
#'
#' @return Retourne une liste de graphiques pour les stations ou points,
#'  graphiques statiques `ggplot2` ou interactifs `ggiraph`.
#' @export
#'
#' @importFrom dplyr filter rowwise mutate ungroup pull select
#' @importFrom forcats fct_rev
#' @importFrom ggiraph geom_point_interactive girafe
#' @importFrom ggplot2 ggplot aes labs scale_x_continuous xlab scale_size unit
#'  theme geom_line scale_shape_manual scale_y_continuous expansion
#'  element_blank element_text element_rect
#' @importFrom ggtext element_markdown
#' @importFrom patchwork plot_layout
#' @importFrom purrr map set_names
#' @importFrom shiny HTML
#' @importFrom stringr str_wrap
#' @importFrom rlang enquo
#' @importFrom stats na.omit
gg_temp_peuplement_local <- function(df,
                               var_id_sta = pop_id,
                               var_libelle_sta = pop_libelle,
                               var_especes = esp_code_alternatif,
                               interactif = FALSE,
                                largeur = 6,
                                hauteur = 5,
                                taxons_ipr = "plain",
                               longueur_libelle = 20,
                                ...)

{
  var_id_sta <- rlang::enquo(var_id_sta)
  var_libelle_sta <- rlang::enquo(var_libelle_sta)
  var_especes <- rlang::enquo(var_especes)

  # mise en forme des étiquettes inspirée de https://stackoverflow.com/a/57086284
  int_breaks <- function(x, n = 5){
    if (length(unique(x)) > 1) {
      pretty(x, n)[round(pretty(x, n), 1) %% 1 == 0]
    } else {
      round(unique(x)) + c(-1, 0, 1)
    }
  }

  int_limits <- function(x) {
    if (length(unique(x)) > 1) {
      range(x)
    } else {
      range(int_breaks(x))
    }
  }

  # fonction de création d'un graphique
  create_graph <- function(pop, df, interactive) {
    df_pop <- df |>
      dplyr::filter(!!var_id_sta == pop) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        hover = shiny::HTML(
          paste0("<b>", annee, "</b><br>",!!var_especes,": ", effectif, ' ind.')
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(!!var_especes := forcats::fct_rev(!!var_especes))

    libelle <-
      df_pop |>
      dplyr::pull(!!var_libelle_sta) |>
      stats::na.omit() |>
      stringr::str_wrap(longueur_libelle)


    df_protocole <-
      df_pop |>
      dplyr::select(annee, !!var_libelle_sta, pro_libelle) |>
      unique() |>
      dplyr::mutate(Protocole = stringr::str_wrap(pro_libelle, 15)) |>
      dplyr::mutate(hover2 = paste0("<b>", annee, "</b><br>", pro_libelle))


    gg_peuplement <-
      ggplot2::ggplot(
        data = df_pop,
        aes(x = annee,
            y = !!var_especes,
            size = ifelse(effectif == 0, NA, effectif))
      ) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(x = annee,
                     y = !!var_especes,
                     tooltip = hover,
                     size = ifelse(effectif == 0, NA, effectif)),
        pch = 21,
        alpha = 0.7,
        fill = "#1B9E77"
      ) +
      ggplot2::labs(
        x = "Ann\u00e9es",
        y = "Taxons",
        title = libelle
      ) +
      ggplot2::scale_x_continuous(
        breaks = int_breaks,
        limits = int_limits
      ) +
      # ggplot2::scale_y_discrete(
      #   expand = ggplot2::expansion(mult = c(0.04, 0.03))
      # ) +
      ggplot2::scale_size(name = "Effectifs",
                          range = c(0.5, 7.5),
                          breaks=c(1, 10, 50, 100, 250, 500, 1000, 2500,5000),
                          limits = c(df_pop$effectif |> min(),
                                     df_pop$effectif |> max())

      ) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        panel.background = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "grey75", linewidth = .25, linetype = "dotted"),
        #strip.text = element_text(size = 11,color="white",face = "bold"),
        strip.background = ggplot2::element_rect(color = "black", fill = "grey30"),
        legend.position = 'none',
        legend.text = ggplot2::element_text(size = 8),
        legend.title = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size=9)
      )

    esp_ipr <-
      ref_espece |>
      dplyr::filter(esp_eligible_calcul_ipr == 't') |>
      dplyr::pull(!!var_especes)

    manual_font <- ifelse(levels(df_pop |> pull(!!var_especes)) %in% esp_ipr, yes = taxons_ipr, no = "plain")

    gg_peuplement <-
      gg_peuplement +
      ggplot2::theme(axis.text.y = ggtext::element_markdown(face = manual_font,size = 8))

    gg_proto <-
      ggplot2::ggplot(
        data = df_protocole,
        aes(x = annee, fill = Protocole)
      ) +
      ggplot2::geom_line(y = 0.5,group=0,alpha=0.5,lty=1,linewidth =0.2) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(x = annee, tooltip = hover2, group=annee, fill = Protocole, shape = Protocole),
        y = 0.5,
        alpha = 0.7,
        size = 2
      ) +
      ggplot2::scale_shape_manual(values = c(22, 23, 24, 25)) +
      ggplot2::scale_x_continuous(
        position = 'top',
        breaks = int_breaks,
        limits = int_limits
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::xlab(NULL) +
      ggplot2::theme(
        panel.background = element_rect(fill = 'grey95'),
        strip.text = element_text(size = 11, color = "white", face = "bold"),
        strip.background = element_rect(color = "black", fill = "grey30"),
        legend.position = 'none',
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        axis.ticks = element_blank()
      )

    plot_comb <- ((gg_peuplement +
                     ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, -0.5, 0), "pt"),
                                    axis.text.x = element_blank(),
                                    #axis.ticks.x = element_blank(),
                                    axis.title.x = element_blank()) ) / gg_proto) +
      patchwork::plot_layout(heights = c(5, 0.2))

    if (interactive) {

      gg_proto <- gg_proto + ggplot2::theme(legend.position = 'none')

      plot_comb <- ((gg_peuplement +
                       ggplot2::theme(legend.position = 'none',
                                      plot.margin = unit(c(0, 0, -0.5, 0), "pt"),
                                      axis.text.x = ggplot2::element_blank(),
                                      #axis.ticks.x = element_blank(),
                                      axis.title.x = ggplot2::element_blank()) ) / gg_proto) +
        patchwork::plot_layout(heights = c(5, 0.2))

      ggiraph::girafe(
        ggobj = plot_comb,
        width_svg = largeur,
        height_svg = hauteur,
        options = list()
      )
    } else {
      plot_comb
    }
  }

  # application sur l'ensemble des points
  graphs <- df |>
    dplyr::pull(!!var_id_sta) |>
    unique() |>
    purrr::map(
      .f = create_graph,
      df = df,
      interactive = interactif
    ) |>
    purrr::set_names(unique(dplyr::pull(df, !!var_id_sta)))

  # sortie
  if (length(graphs) == 1) {
    graphs[[1]]
  } else {
    graphs
  }

}

#' Graphique de la série chronologique des IPR
#'
#' @description Copie de la fonction du package aspe pour permettre des ajustements locaux.
#'
#' @param df_ipr Dataframe contenant les données IPR. Il doit contenir des variables "ipr"
#'     et "annee" ainsi qu'une variable permettant d'identifier la station ou le point de
#'     prélèvement.
#' @param var_id_sta Nom de la variable servant à identifier les stations ou points.
#'     Cette variable donnera les étiquettes du graphique.
#' @param var_ipr Nom de la variable contenant les valeurs IPR.
#' @param station_sel Vecteur caractère indiquant les points ou stations à sélectionner.
#' @param sup_500m Booléen. Indique si les stations sont situées à des altitudes inférieures
#'     (sup_500m = FALSE, par défaut) ou bien supérieures à 500m car les seuils de classe
#'     se qualité varient selon l'altitude. Comme ce paramètre est unique pour l'ensemble des
#'     stations (ou points) représentées, il importe de s'assurer qu'elles sont toutes soit
#'     au-dessus, soit en-dessous de 500m.
#' @param nb_colonnes Entier. Nombre (maxi) de colonnes de graphiques s'il y a plusieurs stations.
#'     Par défaut nb_colonnes = 6.
#' @param max_axe_y Numérique. Limite supérieure de l'axe des ordonnées. Par défaut max_axe_y = 40.
#' @param inv_y Booléen. Indique l'axe des ordonnées pointe vers le bas (TRUE, par défaut) ou
#'     vers le haut.
#' @param annee_facteur Booléen. Indique si la variable annee doit être transformée en facteur.
#'     Parfois utile pour la mise en forme de l'axe des abscisses (années).
#' @param titre_graphique Texte. Titre du graphique.
#' @param titre_y Texte. Titre de l'axe des ordonnées.
#' @param df_classes Dataframe contenant les limites de classes. Exemple : classe_ipr.
#' @param interactif Booléen. Indique si le graphique produit est statique (ggplot) ou interactif (produit avec le package ggiraph).
#' @param largeur,hauteur largeur et hauteur du graphique interactif (en pouces). Cela définit les dimensions relatives du graphique.
#' @param options liste d'options utilisée pour le rendu du graphique interactif (voir \code{\link[ggiraph]{girafe}})
#'
#' @return Un graphique statique ggplot2 ou un graphique interactif girafe.
#' @export
#'
#' @importFrom dplyr filter mutate case_when pull
#' @importFrom ggiraph geom_rect_interactive geom_point_interactive girafe opts_sizing
#' @importFrom ggplot2 enquo ggplot aes scale_fill_manual scale_y_continuous expansion geom_line facet_wrap labs scale_x_continuous guides guide_legend theme element_text element_blank element_line coord_cartesian
#' @importFrom stringr str_wrap
#' @importFrom rlang enquo
gg_temp_ipr_local <- function(df_ipr,
                        var_id_sta,
                        var_ipr,
                        station_sel = NA,
                        sup_500m = FALSE,
                        nb_colonnes = 6,
                        max_axe_y = NULL,
                        inv_y = TRUE,
                        annee_facteur = FALSE,
                        titre_graphique = "Evolution de l'IPR",
                        titre_y = "Indice Poisson Rivi\u00e8re",
                        df_classes = classe_ipr,
                        interactif = FALSE,
                        largeur = 6,
                        hauteur = 5,
                        options = list()
)

{
  # mise en forme des étiquettes inspirée de https://stackoverflow.com/a/57086284
  int_breaks <- function(x, n = 5){
    if (length(unique(x)) > 1) {
      pretty(x, n)[round(pretty(x, n), 1) %% 1 == 0]
    } else {
      round(unique(x)) + c(-1, 0, 1)
    }
  }

  int_limits <- function(x) {
    if (length(unique(x)) > 1) {
      range(x)
    } else {
      range(int_breaks(x))
    }
  }

    # sélection des données
  var_id_sta <- rlang::enquo(var_id_sta)
  var_ipr <- rlang::enquo(var_ipr)

  if(!is.na(station_sel))
  {
    df_ipr <- df_ipr |>
      dplyr::filter(!!var_id_sta %in% station_sel)
  }

  if(is.null(max_axe_y))
  {
    max_axe_y <- df_ipr |> #max(df_ipr$ipr)
      dplyr::pull(!!var_ipr) |>
      max(na.rm = T)
  }

  # gestion de l'évaluation non standard en passant par création d'une variable. Pas trouvé mieux.
  # sinon bug avec le facet_wrap()
  df_ipr <- df_ipr |>
    dplyr::mutate(var_id = stringr::str_wrap(!!var_id_sta, 25), # au cas où intitulés trop longs
           var_id = as.factor(var_id),
           hover = paste0("<b>", annee, "</b><br>IPR: ", round(!!var_ipr, 2))
           )

  # année en facteur ?
  if(annee_facteur)
  {
    df_ipr <- df_ipr |>
      dplyr::mutate(annee = as.factor(annee))
  }

  df_classes <- df_classes |>
    gg_gerer_seuils_classes_ipr_int_local(metriques = FALSE,
                                    sup_500m = sup_500m) |>
    dplyr::mutate(
      hover = paste0(
        "<b>", cli_libelle, "</b><br>",
        dplyr::case_when(
          cli_id == 1 ~
            paste0("[", cli_borne_inf, "-", cli_borne_sup, "]"),
          cli_id == 7 ~
            paste0("> ", cli_borne_inf),
          TRUE ~
            paste0("]", cli_borne_inf, "-", cli_borne_sup, "]")
        )
      )

    )

  plot_ipr_station <- ggplot2::ggplot(data = df_ipr) +
    ggiraph::geom_rect_interactive(
      data = df_classes,
      mapping = ggplot2::aes(
        ymin = classe_borne_inf,
        ymax = classe_borne_sup,
        fill = classe_libelle,
        tooltip = hover
      ),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.3
    ) +
    ggplot2::scale_fill_manual(values = df_classes$classe_couleur) +
    ggplot2::scale_x_continuous(
      breaks = int_breaks,
      limits = int_limits
    ) +
    ggplot2::scale_y_continuous(
      trans = "reverse",
      expand = ggplot2::expansion(mult = c(0.05, 0.01))
    ) +
    # notes IPR
    ggplot2::geom_line(ggplot2::aes(x = annee,
                  y = !!var_ipr),
              show.legend = F,
              lty = 2) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(x = annee, y = !!var_ipr, tooltip = hover),
      size = 2.5,
      pch = 21,
      fill = "grey70"
    ) +
    # treillis
    ggplot2::facet_wrap(~var_id,
               ncol = nb_colonnes) +
    # mise en forme
    ggplot2::labs(title = titre_graphique,
         x = "",
         y = titre_y) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Classe de qualit\u00E9",
                               title.position = "top",
                               nrow = 2,
                               byrow = TRUE,
                               override.aes = list(color = df_classes$classe_couleur,
                                                   fill = df_classes$classe_couleur,
                                                   shape = 15,
                                                   alpha = 0.6))) +
    ggplot2::theme(legend.position = "none",
          strip.text = ggplot2::element_text(
            size = 12, face = "bold", hjust = 0
          ),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "lightgrey", linewidth = .25),
          panel.background = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank())
  # orientation de l'axe des IPR selon l'argument inv_y
  if(inv_y) {

   plot_ipr_station <- plot_ipr_station +
    ggplot2::coord_cartesian(ylim = c(max_axe_y, 0))
  } else {
    plot_ipr_station <- plot_ipr_station +
      ggplot2::coord_cartesian(ylim = c(0, max_axe_y))
  }

  if (interactif) {
    ggiraph::girafe(
      ggobj = plot_ipr_station,
      width_svg = largeur,
      height_svg = hauteur,
      options = options
    )
  } else {
    plot_ipr_station
  }
}

#' Gérer les limites de classes d'indices sur classe_ipr
#'
#' @description Copie de la fonction du package aspe pour permettre des ajustements locaux.
#'
#' @param df_classes Dataframe d'entrée, en général classe_ipr
#' @param metriques Booléen. Si TRUE, on considère qu'il s'agit des métriques et non de
#'     l'indice agrégé. Les seuils sont donc divisés par 7 (le nombre des métriques dans l'IPR).
#' @param sup_500m Booléen. TRUE si les stations sont au-dessus de 500m d'altitude, ce qui change
#'     un seuil de classe IPR.
#'
#' @return Le dataframe filtré selon l'altitude
#'
#' @export
#'
#' @importFrom dplyr filter mutate
gg_gerer_seuils_classes_ipr_int_local <- function(df_classes,
                                            metriques = FALSE,
                                            sup_500m)

{

  # ne pas déplacer sinon le bloc suivant ne fonctionne pas
  df_classes <- replace(df_classes, is.na(df_classes), 0)

  # suppression des lignes selon le seuil d'altitude
  if(sup_500m) {
    df_classes <- df_classes |>
      dplyr::filter(cli_altitude_max != 500)
  } else {
    df_classes <- df_classes |>
      dplyr::filter(cli_altitude_min != 500)
  }

  # si métriques, on divise les seuils par 7
  if(metriques)
  {
    df_classes <- df_classes |>
      dplyr::mutate(classe_borne_inf = cli_borne_inf / 7,
             classe_borne_sup = cli_borne_sup / 7,
             # pour éviter une bande grise après l'arrière-plan rouge
             classe_borne_sup = ifelse(cli_classe == 5,
                                       yes = Inf,
                                       no = classe_borne_sup),
             classe_libelle = cli_libelle)
  } else{
    df_classes <- df_classes |>
      dplyr::mutate(classe_borne_inf = cli_borne_inf,
             classe_borne_sup = cli_borne_sup,
             # pour éviter une bande grise après l'arrière-plan rouge
             classe_borne_sup = ifelse(cli_classe == 5,
                                       yes = Inf,
                                       no = classe_borne_sup),
             classe_libelle = cli_libelle)

  }

  df_classes

}
