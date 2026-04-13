# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
AspeDashboard::run_app(
    onStart = function() {
        dest_dir <- system.file("app/data", package = "AspeDashboard")
        
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/captures.parquet", 
            destfile = file.path(dest_dir, "captures.parquet"),
            mode = "wb"
            )
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/ipr.parquet", 
            destfile = file.path(dest_dir, "ipr.parquet"),
            mode = "wb"
            )
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/carte_operations.parquet", 
            destfile = file.path(dest_dir, "carte_operations.parquet"),
            mode = "wb"
            )
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/metriques.parquet", 
            destfile = file.path(dest_dir, "metriques.parquet"),
            mode = "wb"
            )
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/pop_geo.parquet", 
            destfile = file.path(dest_dir, "pop_geo.parquet"),
            mode = "wb"
            )
        download.file(
            url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/metadata.rda", 
            destfile = file.path(dest_dir, "metadata.rda"),
            mode = "wb"
            )
        
        captures <<- arrow::open_dataset(file.path(dest_dir, "captures.parquet"))
        ipr <<- arrow::open_dataset(file.path(dest_dir, "ipr.parquet"))
        carte_operations <<- arrow::open_dataset(file.path(dest_dir, "carte_operations.parquet"))
        metriques <<- arrow::open_dataset(file.path(dest_dir, "metriques.parquet"))
        pop_geo_df <<- arrow::open_dataset(file.path(dest_dir, "pop_geo.parquet"))
        
        load(file.path(dest_dir, "metadata.rda"), envir = .GlobalEnv)
        
        popups_base_dir <- file.path(system.file("app/www", package = "AspeDashboard"), "popups")  
        dir.create(popups_base_dir, showWarnings = FALSE, recursive = TRUE)
        
        onStop(
            function() {
                popups_dir <- list.dirs(path = popups_base_dir, full.names = TRUE, recursive = FALSE)
                popups_dir <- popups_dir[!stringr::str_detect(string = popups_dir, pattern = file.path(popups_base_dir, "lib"))]
                purrr::walk(
                    popups_dir,
                    unlink, force = TRUE, recursive = TRUE
                )
                purrr::walk(
                    list.files(path = popups_base_dir, pattern = ".html", full.names = TRUE),
                    unlink, force = TRUE
                )
            }
        )
    }
) # add parameters here (if any)
