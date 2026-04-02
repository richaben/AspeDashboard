# Préparation données filtre admin
administratif <- COGiter::departements |>  
    dplyr::rename(INSEE_REG = REG, INSEE_DEP = DEP) |> 
    dplyr::distinct(
        INSEE_REG, INSEE_DEP, NOM_DEP
    ) |>  
    dplyr::mutate(
        INSEE_REG = factor(
            INSEE_REG,
            levels = c("84", "27", "53", "24", "44", "32", "11", "28", "75", "76", "52", "93", "94")
        ),
        departement = paste0(NOM_DEP, " (", INSEE_DEP, ")")
    ) |>  
    dplyr::filter(!is.na(INSEE_REG)) |> 
    dplyr::arrange(INSEE_REG, NOM_DEP) |>  
    dplyr::mutate(INSEE_REG = as.character(INSEE_REG))

# # Préparation emprises pour les filtres spatiaux
# regions <- COGiter::regions_metro_geo |> 
#     dplyr::left_join(COGiter::regions |> 
#                          dplyr::select(REG, NOM_REG),
#                      by = "REG") |> 
#     sf::st_transform(crs = 4326)
# 
# departements <- COGiter::departements_metro_geo |> 
#     dplyr::left_join(COGiter::departements |> 
#                          dplyr::select(DEP, NOM_DEP),
#                      by = "DEP") |> 
#     sf::st_transform(crs = 4326)
# 
# bassins <- sf::st_read("https://services.sandre.eaufrance.fr/geo/topage2025?REQUEST=getCapabilities&service=WFS&VERSION=2.0.0", layer = "sa:BassinHydrographique_Topage2025") |> 
#     sf::st_transform(crs = 4326)
# 
# extraire_bbox <- function(geo, niveau, code, nom) {
#     geo_bbox <- geo |> 
#         sf::st_bbox()
#     
#     tibble::tibble(
#         niveau = niveau,
#         code = code,
#         nom = nom,
#         xmin = geo_bbox["xmin"],
#         xmax = geo_bbox["xmax"],
#         ymin = geo_bbox["ymin"],
#         ymax = geo_bbox["ymax"]
#     )
# }
# 
# emprises <- dplyr::bind_rows(
#     extraire_bbox(geo = regions, niveau = "france", code = "", nom = "France"),
#     purrr::map(
#         regions$REG,
#         function(reg) {
#             extraire_bbox(
#                 geo = regions |> 
#                     dplyr::filter(REG == reg),
#                 niveau = "region",
#                 code = regions |> 
#                     dplyr::filter(REG == reg) |> 
#                     dplyr::pull(REG),
#                 nom = regions |> 
#                     dplyr::filter(REG == reg) |> 
#                     dplyr::pull(NOM_REG)
#             )  
#         }
#     ) |> 
#         purrr::list_rbind(),
#     purrr::map(
#         departements$DEP,
#         function(dep) {
#             extraire_bbox(
#                 geo = departements |> 
#                     dplyr::filter(DEP == dep),
#                 niveau = "departement",
#                 code = departements |> 
#                     dplyr::filter(DEP == dep) |> 
#                     dplyr::pull(DEP),
#                 nom = departements |> 
#                     dplyr::filter(DEP == dep) |> 
#                     dplyr::pull(NOM_DEP)
#             )  
#         }
#     ) |> 
#         purrr::list_rbind(),
#     purrr::map(
#         bassins$CdBH,
#         function(bassin) {
#             extraire_bbox(
#                 geo = bassins |> 
#                     dplyr::filter(CdBH == bassin),
#                 niveau = "bassin",
#                 code = bassins |> 
#                     dplyr::filter(CdBH == bassin) |> 
#                     dplyr::pull(CdBH),
#                 nom = bassins |> 
#                     dplyr::filter(CdBH == bassin) |> 
#                     dplyr::pull(LbBH)
#             )  
#         }
#     ) |> 
#         purrr::list_rbind()
# )

ref_espece <- aspe::ref_espece
classe_ipr <- aspe::classe_ipr |>
    aspe::ip_completer_classes_couleur()

codes_especes <- aspe::ref_espece$esp_code_taxref |> 
    purrr::set_names(aspe::ref_espece$esp_code_alternatif)

usethis::use_data(
    administratif,
    # emprises, 
    classe_ipr,
    ref_espece,
    codes_especes,
    overwrite = TRUE,
    internal = TRUE
)
