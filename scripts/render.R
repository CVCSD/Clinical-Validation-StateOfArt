### Generació d'informe



rmarkdown::render("scripts/analisi_StateArt.Rmd", 
                  output_file = here::here("outputs",paste0("Informe_estadistic_",Sys.Date())))