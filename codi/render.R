### Generació d'informe



rmarkdown::render("codi/analisi_StateArt.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_estadistic_",Sys.Date())))