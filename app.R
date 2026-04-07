# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
AspeDashboard::run_app(
    onStart = function() {
        download.file(url = "https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/captures.parquet", destfile = "inst/app/data/captures.parquet", mode = "wb")
        download.file("https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/ipr.parquet", destfile = "inst/app/data/ipr.parquet", mode = "wb")
        download.file("https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/carte_operations.parquet", destfile = "inst/app/data/carte_operations.parquet", mode = "wb")
        download.file("https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/metriques.parquet", destfile = "inst/app/data/metriques.parquet", mode = "wb")
        download.file("https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/pop_geo.parquet", destfile = "inst/app/data/pop_geo.parquet", mode = "wb")
        download.file("https://github.com/OFB-IdF/AspeDashboard/raw/refs/heads/main/inst/app/data/metadata.rda", destfile = "inst/app/data/metadata.rda", mode = "wb")
        
        captures <<- arrow::open_dataset("inst/app/data/captures.parquet")
        ipr <<- arrow::open_dataset("inst/app/data/ipr.parquet")
        carte_operations <<- arrow::open_dataset("inst/app/data/carte_operations.parquet")
        metriques <<- arrow::open_dataset("inst/app/data/metriques.parquet")
        pop_geo_df <<- arrow::open_dataset("inst/app/data/pop_geo.parquet")
        
        load("inst/app/data/metadata.rda", envir = .GlobalEnv)
        
        popups_base_dir <- file.path("inst", "app", "www", "popups")  
        dir.create(popups_base_dir, showWarnings = FALSE, recursive = TRUE)
        
        onStop(
            function() {
                unlink(popups_base_dir, recursive = TRUE, force = TRUE)
            }
        )
    }
) # add parameters here (if any)
