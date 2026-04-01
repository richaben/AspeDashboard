#' carte_op UI Function
#'
#' @description Module UI pour l'affichage de la carte interactive des opérations de pêche.
#'
#' @param id Paramètre interne pour {shiny}.
#'
#' @noRd 
#'
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS tagList selectizeInput actionButton plotOutput tags HTML
mod_carte_op_ui <- function(id){
  ns <- NS(id)
  
  css <- HTML(
      paste0(
          paste0("#", ns("carte_op"), " {height: calc(100vh - 200px) !important;}"),
          ".search-station {
            position: absolute;
            top: 0px;
            right: 20px;
          }
          
          .reset-station {
            position: absolute;
            bottom: 10px;
            right: 0px;
          }
          
           .leaflet {
                margin-top:0px;
           }

           .leaflet-control-zoom, .leaflet-top, .leaflet-bottom {
           z-index: unset !important;
           }

           .leaflet-touch .leaflet-control-layers .leaflet-control-zoom .leaflet-touch .leaflet-bar {
           z-index: 10000000000 !important;
           }

           .custom-popup .leaflet-popup-content {
                  width: 341px !important;
                  height: 475px !important;
                  margin: 0px !important;
                  padding: 0px !important;
                  overflow: hidden !important;
             }
 
             .custom-popup .leaflet-popup-content-wrapper {
                   width: 341px !important;
                   height: 475px !important;
                   padding: 0px !important;
                   border-radius: 5px;
                   overflow: hidden !important;
              }
          "
      )
  )
  
  tagList(
      tags$head(
          tags$style(css)
      ),
    column(
        width = 12,
        tags$div(
            class = "search-station",
            selectizeInput(
                inputId = ns("station"),
                label = "",
                choices = c(
                    "Zoomer sur un point de prélèvement" = ""
                ),
                multiple = FALSE
            )
        ),
        tags$div(
            class = "reset-station",
            actionButton(
                inputId = ns("reset"),
                label = "Désélectionner station"
            )
        ),
        leaflet::leafletOutput(
            ns("carte_op"),
            width = '100%'
        ),
        plotOutput(
            ns("legende"),
            height = "100px"
        )  
    )
  )
}
    
#' carte_op Server Functions
#'
#' @description Module serveur pour la gestion de la carte interactive.
#'
#' @param id Identifiant du module.
#' @param departement Réactif contenant le ou les départements sélectionnés.
#' @param bassin Réactif contenant le ou les bassins sélectionnés.
#' @param periode Réactif contenant la période sélectionnée (range).
#' @param variable Réactif contenant la variable d'intérêt ("ipr" ou "especes").
#' @param espece Réactif contenant l'espèce sélectionnée (si applicable).
#'
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom leaflet renderLeaflet leaflet addTiles leafletProxy clearMarkers addCircleMarkers fitBounds
#' @importFrom sf st_bbox
#' @importFrom shiny HTML moduleServer reactiveValues reactive observeEvent req updateSelectizeInput observe renderPlot
#' @importFrom tidyr drop_na
#' @importFrom htmlwidgets saveWidget
mod_carte_op_server <- function(id, departement, bassin, periode, variable, espece, temp_dir){
  moduleServer(
    id, 
    function(input, output, session){
    ns <- session$ns
    
    SelectionPoint <- reactiveValues(clickedMarker=NULL, markerCoords=NULL)
    
    radius_pal <- function(x) {
        approx(x = c(1, 10), y = c(3, 10), xout = sqrt(x), yleft = 3, yright = 10)$y
    }
     
    BboxMap <- list(xmin = -5.1, ymin = 41.3, xmax = 9.6, ymax = 51.1)

    output$carte_op <- leaflet::renderLeaflet({
        leaflet::leaflet() |>  
            leaflet::addTiles() |> 
            leaflet::fitBounds(BboxMap$xmin, BboxMap$ymin, BboxMap$xmax, BboxMap$ymax)
    })

    output$legende <- renderPlot({
        req(variable())
        switch(variable(),
               especes = LegendeEspeces,
               ipr = LegendeIpr,
               distribution = LegendeDistribution)
    })

    # 1. Emprise géographique (Variables locales pour Arrow)
    MapEmprise_r <- reactive({
        req(departement(), bassin())
        sel_dept <- departement()
        sel_bassin <- bassin()
        
        res <- pop_geo_df |> 
            dplyr::filter(dept_id %in% sel_dept, dh_libelle %in% sel_bassin) |> 
            dplyr::collect() |> 
            sf::st_as_sf(coords = c("x", "y"), crs = 4326)
        gc() 
        res
    })

    # 2. Données de base (Filtres spatiaux, variable & espèce - Arrow)
    DataBase_r <- reactive({
        req(departement(), bassin(), variable())
        
        # Extraction des réactifs en variables locales
        sel_dept <- departement()
        sel_bassin <- bassin()
        sel_var <- variable()
        sel_esp <- espece()

        # Si variable "distribution", l'espèce est requise
        # On ne met pas de req() ici car cela bloquerait toute la chaîne réactive
        # (et empêcherait d'effacer la carte si l'espèce est vide)
        if (sel_var == "distribution" && (is.null(sel_esp) || sel_esp == "")) return(NULL)

        # Pipeline Arrow optimisé (Filtres spatiaux et thématiques)
        query <- carte_operations |> 
            dplyr::filter(
                dept_id %in% sel_dept, 
                dh_libelle %in% sel_bassin,
                variable == sel_var
            )
        
        # Filtrage spécifique à la distribution (Species filter)
        if (sel_var == "distribution" && !is.null(sel_esp)) {
            query <- query |> dplyr::filter(esp_code_alternatif == sel_esp)
        }

        res <- query |> dplyr::collect()
        gc() 
        res
    })

    # 2b. Données de la carte (Filtre temporel - R en mémoire)
    DataMap_r <- reactive({
        # On ne met pas de req() sur DataBase_r() ici pour permettre 
        # le déclenchement de l'observeur même si DataBase_r() est NULL/bloqué
        req(MapEmprise_r(), periode(), variable())
        
        # Extraction des réactifs en variables locales
        data_base <- DataBase_r()
        sel_per <- periode()
        sel_var <- variable()

        if (is.null(data_base) || nrow(data_base) == 0) return(NULL)

        # Calcul des bornes temporelles
        min_p <- as.integer(min(sel_per))
        max_p <- as.integer(max(sel_per))

        # Filtrage par période sur les données en mémoire
        data_filtered <- data_base |> 
            dplyr::filter(
                annee >= min_p, 
                annee <= max_p
            )
            
        if (is.null(data_filtered) || nrow(data_filtered) == 0) return(NULL)

        # Traitement post-collect (R standard)
        data_processed <- data_filtered |>
            dplyr::group_by(pop_id) |> 
            dplyr::mutate(afficher = dplyr::case_when(
                sel_var != "distribution" ~ TRUE,
                sel_var == "distribution" & sum(effectif, na.rm = TRUE) > 0 ~ TRUE,
                sel_var == "distribution" & sum(effectif, na.rm = TRUE) == 0 ~ FALSE
            )) |> 
            dplyr::filter(afficher, annee == max(annee[afficher])) |> 
            dplyr::ungroup()

        if (nrow(data_processed) == 0) return(NULL)

        # Jointure finale avec les géométries
        dplyr::inner_join(
            MapEmprise_r() |> dplyr::select(pop_id),
            data_processed,
            by = "pop_id"
        ) |>
        dplyr::mutate(hover = paste0(
            "<b>", sta_libelle_sandre, " (", sta_code_sandre, ")</b><br>",
            "<em>", dept_libelle, " (", reg_libelle, ")</em><br>",
            nb_annees, " année", ifelse(as.numeric(nb_annees) > 1 , "s", ""),
            ifelse(sel_var == "distribution", " de détection", " de suivi"),
            "<br>",
            dplyr::case_when(
                sel_var == "especes" ~ paste0(valeur, " espèce", ifelse(as.numeric(valeur) > 1, "s", "")),
                sel_var == "ipr" ~ paste0(valeur, " état"),
                sel_var == "distribution" ~ paste0("Densité moyenne: ", valeur)
            ),
            ifelse(sel_var == "distribution", "", paste0(" (", annee, ")"))
        ))
    })

    # Mise à jour de la carte (Markers)
    observe({
        # On récupère le proxy et on vide la carte IMMÉDIATEMENT
        # avant tout appel à un réactif qui pourrait bloquer l'exécution via req()
        proxy <- leaflet::leafletProxy("carte_op")
        proxy |> leaflet::clearMarkers()

        # Dépendances explicites pour forcer le déclenchement dès qu'un filtre change
        # On les appelle APRÈS clearMarkers() pour garantir que la carte est vidée
        # même si l'un d'eux est NULL ou bloque l'exécution
        var <- variable()
        per <- periode()
        esp <- espece()
        dept <- departement()
        bass <- bassin()
        
        # Maintenant on récupère les données traitées
        data <- DataMap_r()
        
        if (!is.null(data) && nrow(data) > 0) {
            proxy |> leaflet::addCircleMarkers(
                data = data, layerId = ~pop_id, radius = ~radius_pal(nb_annees),
                fillColor = ~identity(couleur), stroke = TRUE, color = "black", weight = 2,
                opacity = ~identity(opacite), fillOpacity = .75,
                label = ~lapply(hover, shiny::HTML)
            ) 
        }
    })

    # Fonction utilitaire pour générer le contenu de la popup
    generer_contenu_popup <- function(pop_id_sel, sel_var) {
        if (is.null(pop_id_sel) || sel_var == "distribution") return(NULL)
        
        content <- NULL
        tryCatch({
             if (sel_var == "especes") {
                 # Filtrage via Arrow pour ne pas tout charger
                 data_sta <- captures |> 
                     dplyr::filter(pop_id == pop_id_sel) |> 
                     dplyr::collect()
                 
                 if (nrow(data_sta) > 0) {
                     pop_libelle_sel <- data_sta$pop_libelle[1]
                     p <- popup_peuplement(data_sta, pop_id_sel, pop_libelle_sel)
                     
                     # Sauvegarde dans un fichier temporaire accessible via addResourcePath
                     file_name <- paste0("popup_", pop_id_sel, ".html")
                     file_path <- file.path(temp_dir, file_name)
                     htmlwidgets::saveWidget(p, file_path, selfcontained = TRUE)
                     
                     # Modification directe du style CSS dans le fichier HTML (approche AspeDashboardData)
                     readLines(file_path, warn = FALSE) |> 
                         ajuster_html() |> 
                       writeLines(file_path)
                     
                     content <- paste0(
                           '<iframe src="temp_popups/', file_name, '" ',
                           'width="341px" height="475px" style="border:none; overflow:hidden;" ',
                           'scrolling="no"></iframe>'
                       )
                   }
               } else if (sel_var == "ipr") {
                   data_sta <- ipr |> 
                       dplyr::filter(pop_id == pop_id_sel) |> 
                       dplyr::collect()
                   
                   if (nrow(data_sta) > 0) {
                       pop_libelle_sel <- data_sta$pop_libelle[1]
                       # On suppose classe_ipr disponible globalement
                       p <- popup_ipr(data_sta, pop_id_sel, pop_libelle_sel, classe_ipr)
                       
                       # Sauvegarde dans un fichier temporaire accessible via addResourcePath
                       file_name <- paste0("popup_", pop_id_sel, ".html")
                       file_path <- file.path(temp_dir, file_name)
                       htmlwidgets::saveWidget(p, file_path, selfcontained = TRUE)
                       
                       # Modification directe du style CSS dans le fichier HTML (approche AspeDashboardData)
                       readLines(file_path, warn = FALSE) |> 
                           ajuster_html() |> 
                         writeLines(file_path)
                       
                       content <- paste0(
                            '<iframe src="temp_popups/', file_name, '" ',
                            'width="341px" height="475px" style="border:none; overflow:hidden;" ',
                            'scrolling="no"></iframe>'
                        )
                    }
                }
        }, error = function(e) {
            shiny::showNotification(paste("Erreur lors de la génération du popup :", e$message), type = "error")
        })
        
        return(content)
    }

    # Observeur central pour la gestion des popups
    observeEvent(list(input$carte_op_marker_click, variable()), {
        # Cas 1 : Nouveau clic sur un marqueur
        if (!is.null(input$carte_op_marker_click)) {
            marker <- input$carte_op_marker_click
            SelectionPoint$clickedMarker <- marker$id
            SelectionPoint$markerCoords <- list(lng = marker$lng, lat = marker$lat)
        }
        
        pop_id_sel <- SelectionPoint$clickedMarker
        sel_var <- variable()
        proxy <- leaflet::leafletProxy("carte_op")
        
        # Cas (i) : Fermeture si "distribution" ou pas de point sélectionné
        if (sel_var == "distribution" || is.null(pop_id_sel)) {
            proxy |> leaflet::clearPopups()
            return()
        }
        
        # Cas (ii) : Mise à jour du contenu pour "especes" ou "ipr"
        # Affichage d'un indicateur de chargement
        shiny::showNotification("Génération du graphique...", duration = 2, type = "message", id = "popup_loading")
        
        content <- generer_contenu_popup(pop_id_sel, sel_var)
        
        if (!is.null(content)) {
            coords <- SelectionPoint$markerCoords
            proxy |> 
                leaflet::clearPopups() |> 
                leaflet::addPopups(
                    lng = coords$lng, 
                    lat = coords$lat, 
                    popup = content,
                    options = leaflet::popupOptions(
                        minWidth = 341, 
                        maxWidth = 341, 
                        maxHeight = 500,
                        className = "custom-popup"
                    )
                )
        }
        
        shiny::removeNotification("popup_loading")
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


    # Zoom & Emprise
    observeEvent(MapEmprise_r(), {
        req(MapEmprise_r())
        bbox <- sf::st_bbox(MapEmprise_r())
        leaflet::leafletProxy("carte_op") |>
            leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
            
        updateSelectizeInput(session, "station", server = TRUE,
            choices = c("Localiser un point" = "", 
                        MapEmprise_r() |> sf::st_drop_geometry() |> 
                        dplyr::distinct(pop_libelle) |> dplyr::arrange(pop_libelle) |> 
                        dplyr::pull(pop_libelle)))
    })

    observeEvent(input$station, {
        req(input$station != "")
        sel_station <- input$station
        coords <- pop_geo_df |> dplyr::filter(pop_libelle == sel_station) |> 
                  dplyr::collect() |> sf::st_as_sf(coords = c("x", "y"), crs = 4326) |> 
                  sf::st_centroid() |> sf::st_coordinates()
        gc()
        leaflet::leafletProxy("carte_op") |> 
            leaflet::setView(lng = coords[,"X"], lat = coords[,"Y"], zoom = 15)
    })
    
    observeEvent(input$reset, { 
        SelectionPoint$clickedMarker <- NULL
        SelectionPoint$markerCoords <- NULL
        updateSelectizeInput(session, "station", selected = "") 
        leaflet::leafletProxy("carte_op") |> leaflet::clearPopups()
    })
    
    reactive({ SelectionPoint$clickedMarker })
  })
}

## To be copied in the UI
# mod_carte_op_ui("carte_op_ui_1")
    
## To be copied in the server
# mod_carte_op_server("carte_op_ui_1")
