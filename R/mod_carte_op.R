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
mod_carte_op_server <- function(id, departement, bassin, periode, variable, espece){
  moduleServer(
    id, 
    function(input, output, session){
    ns <- session$ns
    
    SelectionPoint <- reactiveValues(clickedMarker=NULL)
    
     radius_pal <- function(x) {
        # On utilise une valeur fixe pour la range si on veut éviter de scanner tout le parquet
        # ou on le fait une fois au démarrage
        approx(
            x = c(1, 10), # Valeurs par défaut raisonnables pour nb_annees
            y = c(3, 10),
            xout = sqrt(x),
            yleft = 3,
            yright = 10
        )$y
     }
     
   BboxMap <- list(xmin = -5.1, ymin = 41.3, xmax = 9.6, ymax = 51.1) # Bbox France approximative

   output$carte_op <- leaflet::renderLeaflet(
        leaflet::leaflet() |>  
            leaflet::addTiles() |> 
        leaflet::fitBounds(
            lng1 = BboxMap[["xmin"]],
            lat1 = BboxMap[["ymin"]],
            lng2 = BboxMap[["xmax"]],
            lat2 = BboxMap[["ymax"]]
        )
    )

    output$legende <- renderPlot({
        req(variable)
        
        switch(
            variable(),
            especes = LegendeEspeces,
            ipr = LegendeIpr,
            distribution = LegendeDistribution
        )
    }
    )
    
    observe({
        req(departement, bassin, periode, variable, espece)
        
        sel_dept <- departement()
        sel_bassin <- bassin()
        sel_per <- periode()
        min_per <- min(sel_per)
        max_per <- max(sel_per)
        sel_var <- variable()
        sel_esp <- espece()
        
        ChoixEspece <- ifelse(
            sel_var != "distribution" | is.null(sel_esp), "", sel_esp
        )

        MapEmprise <- pop_geo_df |> 
            dplyr::filter(
                dept_id %in% sel_dept,
                dh_libelle %in% sel_bassin
            ) |> 
            dplyr::collect() |> 
            sf::st_as_sf(coords = c("x", "y"), crs = 4326)
        
        DataMap <- dplyr::inner_join(
                MapEmprise |> 
                    dplyr::select(pop_id),
                carte_operations |> 
                  dplyr::filter(
                    dept_id %in% sel_dept,
                    dh_libelle %in% sel_bassin,
                    variable == sel_var,
                    annee >= min_per,
                    annee <= max_per
                  ) |> 
                  dplyr::collect() |> 
                  dplyr::mutate(
                    esp_code_alternatif = stringr::str_replace_na(
                      esp_code_alternatif, ""
                    )
                  ) |> 
                  dplyr::filter(esp_code_alternatif == ChoixEspece) |> 
                  dplyr::group_by(pop_id) |> 
                  dplyr::mutate(
                    afficher = dplyr::case_when(
                      variable != "distribution" ~ TRUE,
                      variable == "distribution" &
                        sum(effectif)>0 ~ TRUE,
                      variable == "distribution" &
                        sum(effectif) == 0 ~ FALSE
                    )
                  ) |> 
                  dplyr::filter(
                    afficher,
                    annee == max(annee[afficher])
                  ) |> 
                  dplyr::ungroup(),
                by = "pop_id"
            ) |>
            dplyr::mutate(
                hover = paste0(
                    "<b>", sta_libelle_sandre, " (", sta_code_sandre, ")</b><br>",
                    "<em>", dept_libelle, " (", reg_libelle, ")</em><br>",
                    nb_annees, " année",
                    ifelse(as.numeric(nb_annees) > 1 , "s", ""),
                    ifelse(variable == "distribution",
                           paste0(" de détection (sur ", nb_annees_tot, ")"),
                           " de suivi"
                           ),
                    "<br>",
                    dplyr::case_when(
                        variable == "especes" ~ paste0(
                            valeur, " espèce",
                            ifelse(as.numeric(valeur) > 1, "s", "")
                            ),
                        variable == "ipr" ~ paste0(valeur, " état"),
                        variable == "distribution" ~ paste0("Densité moyenne quand capturée: ", valeur)
                        ),
                    ifelse(variable == "distribution", "",
                           paste0(" (", annee, ")")
                           )
                    )
            )

        updateSelectizeInput(
            session = session,
            inputId = "station",
            choices = c(
                "Localiser un point de prélèvement" = "",
                MapEmprise |> 
                    sf::st_drop_geometry() |> 
                    dplyr::distinct(pop_libelle) |> 
                    dplyr::arrange(pop_libelle) |> 
                    dplyr::pull(pop_libelle)
            ),
            server = TRUE
        )
        
        BboxMap <- sf::st_bbox(MapEmprise)
        
        leaflet::leafletProxy("carte_op") |>
            leaflet::fitBounds(
                lng1 = BboxMap[["xmin"]],
                lat1 = BboxMap[["ymin"]],
                lng2 = BboxMap[["xmax"]],
                lat2 = BboxMap[["ymax"]]
            )

        popups <- switch(
            variable(),
            especes = paste0(
                '<iframe src="www/widgets/especes/file_', DataMap$pop_id, '.html" ',
                'width="419px" height="538px" frameborder="0"></iframe>'
            ),
            ipr = paste0(
                '<iframe src="www/widgets/ipr/file_', DataMap$pop_id, '.html" ',
                'width="419px" height="538px" frameborder="0"></iframe>'
            ),
            distribution = NULL
        )
        
        if (nrow(DataMap) == 0) {
            SelectionPoint$clickedMarker <- NULL
            
            leaflet::leafletProxy("carte_op") |>
                leaflet::clearMarkers()
        } else {
            
            leaflet::leafletProxy("carte_op") |>
            leaflet::clearMarkers() |>
            leaflet::addCircleMarkers(
                data = DataMap,
                layerId = ~pop_id,
                radius = ~radius_pal(nb_annees),
                fillColor = ~identity(couleur),
                stroke = TRUE,
                color = "black",
                weight = 2,
                opacity = ~identity(opacite),
                fillOpacity = .75,
                label = ~lapply(hover, shiny::HTML),
                popup = popups,
                popupOptions = leaflet::popupOptions(
                    maxWidth = 400,
                    minWidth = 300
                )
            ) 
        }
        
        observe({

            if (input$station != "") {
                sel_station <- input$station

                CoordsStation <- pop_geo_df |> 
                    dplyr::filter(pop_libelle == sel_station) |> 
                    dplyr::collect() |> 
                    sf::st_as_sf(coords = c("x", "y"), crs = 4326) |> 
                    sf::st_centroid() |> 
                    sf::st_coordinates()
                
                CoordsStationX <- unname(CoordsStation[,"X"])
                CoordsStationY <- unname(CoordsStation[,"Y"])
                
            leaflet::leafletProxy("carte_op") |> 
                leaflet::setView(
                    lng = CoordsStationX,
                    lat = CoordsStationY,
                    zoom = 15
                )
            } 
            
        })
    })
    
    # observe the marker click info and print to console when it is changed.
    observeEvent(input$carte_op_marker_click,{
        SelectionPoint$clickedMarker <- input$carte_op_marker_click$id
        # update
    })
    
    observeEvent(input$reset, {
        SelectionPoint$clickedMarker <- NULL
        sel_dept <- departement()
        sel_bassin <- bassin()
        
        updateSelectizeInput(
            session = session,
            inputId = "station",
            choices = c(
                "Localiser un point de prélèvement" = "",
                pop_geo_df |> 
                    dplyr::filter(
                        dept_id %in% sel_dept,
                        dh_libelle %in% sel_bassin
                    ) |> 
                    dplyr::collect() |> 
                    dplyr::distinct(pop_libelle) |> 
                    dplyr::arrange(pop_libelle) |> 
                    dplyr::pull(pop_libelle)
            ),
            server = TRUE
        )
        
    })
    
    reactive({
        SelectionPoint$clickedMarker
        })
  })
}
    
## To be copied in the UI
# mod_carte_op_ui("carte_op_ui_1")
    
## To be copied in the server
# mod_carte_op_server("carte_op_ui_1")
