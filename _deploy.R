rsconnect::appDependencies(appDir = ".") |> 
    dplyr::filter(Source == "github")

if (!require("pak")) install.packages("pak")

pak::pkg_install(c(
    "OFB-IdF/AspeDashboard",
    "PascalIrz/aspe",
    "karthik/rdrop2",
    "dreamRs/shinylogs",
    "OFB-IdF/templatesOFB"
))

rsconnect::deployApp(
    appName= "AspeDashboard",
    appTitle= "AspeDashboard",
    account= "ofbidf",
    server= "shinyapps.io"
)


