pkgs <- installed.packages()[renv::dependencies()$Package,] |> 
    as.data.frame() |> 
    tibble::as_tibble() |> 
    dplyr::select(Package, Version) 

get_description <- function(p) {
    desc <- packageDescription(p)
    
    tibble::tibble(
        Package = desc$Package,
        version = desc$Version,
        Repository = desc$Repository,
        RemoteType = desc$RemoteType,
        RemotePkgRef = desc$RemotePkgRef
    )
}

purrr::map(pkgs$Package, get_description) |> 
    purrr::list_rbind() |> 
    dplyr::mutate(
        RemotePkgRef = ifelse(is.na(RemotePkgRef), Package, RemotePkgRef)
    ) |> 
    dplyr::pull(RemotePkgRef) |>
    unique() |> 
    writeLines("pak_packages.txt")
