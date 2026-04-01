# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
AspeDashboard::run_app(
    onStart = function() {
        # Chargement des datasets Arrow (Lazy Loading, 0 RAM au démarrage)
        if (file.exists("inst/app/data/captures.parquet")) {
            captures <<- arrow::open_dataset("inst/app/data/captures.parquet")
            ipr <<- arrow::open_dataset("inst/app/data/ipr.parquet")
            carte_operations <<- arrow::open_dataset("inst/app/data/carte_operations.parquet")
            metriques <<- arrow::open_dataset("inst/app/data/metriques.parquet")
            pop_geo_df <<- arrow::open_dataset("inst/app/data/pop_geo.parquet")
        }
        
        # Chargement des métadonnées légères
        if (file.exists("inst/app/data/metadata.rda")) {
            load("inst/app/data/metadata.rda", envir = .GlobalEnv)
        } 
    }
) # add parameters here (if any)
