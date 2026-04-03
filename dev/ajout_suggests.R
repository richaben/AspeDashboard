c("httr", "rmapshaper", "cowplot", "ows4R") %>% 
    lapply(function(x) {
        usethis::use_package(x, type = "Suggests")
    })
