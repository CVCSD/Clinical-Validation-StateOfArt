#  NETEJA NOMS DE VARIABLES DE CARACTERS EXTRANYS ("/","(".....) ---------------

netejar.noms.variables<-function(dt=LIPOS_EORTEGA){
  
  paco<-names(dt) %>% 
    iconv("UTF-8","ASCII","") %>% 
    stringr::str_replace("/","") %>% 
    stringr::str_replace_all("\\(","") %>% 
    stringr::str_replace_all("\\)","") %>% 
    stringr::str_replace_all("\\/","") %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all(" ","_") %>% 
    stringr::str_replace_all("-","_") %>%
    stringr::str_replace_all("([.])\\1+","\\1") %>% 
    stringr::str_replace_all("\\*","") %>% 
    stringr::str_replace_all("\\?","") %>% 
    stringr::str_replace_all("\\<","Inf") %>% 
    stringr::str_replace_all("\\>","Sup") %>% 
    stringr::str_replace_all("\\[","") %>% 
    stringr::str_replace_all("\\]","") 
  
  names(dt)<-paco
  dt
  
}


# Funció que elimina accents dels noms de les variables
netejar.accents.variables <- function(dt=LIPOS_EORTEGA){
  paco<-names(dt) %>%
    iconv(to="ASCII//TRANSLIT")
  names(dt)<-paco
  dt
}


#  Extreure.Variables: Selector de variables TAULA DE--------
#
extreure.variables=function(taula="table1",taulavariables="variables_R.xls",variable_camp="camp",dt=NA,...) {
  
  # taula="dates_excel"
  # taulavariables = conductor_variables
  # variable_camp="camp"
  # dt=dades
  
  ####  Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(!!variable_camp,!!taula)
  variables <- read_conductor(taulavariables,...) %>% dplyr::select(!!variable_camp,!!taula)
  taula_sym<-rlang::sym(taula)
  variables<-variables %>% dplyr::filter(!is.na(!!taula_sym))
  
  # Verificar si columnes del conductor estan en dt
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value")) %>% pull("camp")
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  # filtratge 
  kk<-variables %>% dplyr::arrange(!!taula_sym) %>% dplyr::filter(!!taula_sym>0) %>% dplyr::select(!!variable_camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  purrr::set_names(kk,kk)
  
}



#' Read conductor file different formats txt o rds o xls xlsx o data_frame tibble
#' @param fitxer Character as a file name and path or data.frame
#' 
read_conductor<-function(fitxer,...) {
  # fitxer<-here::here(fitxer_cataleg)
  # Si el fitxer es un data_frame saltar
  if (any(class(fitxer) %in% c("tbl_df","tbl","data.frame"))) 
    
    dt <- tibble::as_tibble(fitxer) 
  
  else { 
    
    if (stringr::str_detect(fitxer,"\\.txt$")){
      
      dt<-data.table::fread(fitxer) %>% as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.rds$")) {
      
      dt<-readRDS(fitxer,...) %>% as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.xls$")) {
      
      dt<-readxl::read_excel(fitxer,...) %>% tidyr::as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.xlsx$")) {
      
      dt<-readxl::read_excel(fitxer,...) %>% tidyr::as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.xlS$")) {
      
      dt<-readxl::read_excel(fitxer,...) %>% tidyr::as_tibble()
      
    } else if (stringr::str_detect(fitxer,"\\.sav$")) {
      
      dt<-foreign::read.spss(fitxer,use.value.labels = T,to.data.frame = T,...) %>% tidyr::as_tibble()
    } 
    else {stop("format de dades no reconegut ")}
  }
  
}


#
#  Etiquetar les variables de les dades      #####
###
etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls",camp_descripcio="descripcio",...) {
  
  # d=dades
  # taulavariables = conductor
  # camp_descripcio="descripcio"
  
  
  # Symbol
  camp_descripcio<-sym(camp_descripcio)
  
  #  Llegir etiquetes i variables a analitzar ####
  variables<-read_conductor(taulavariables,...)
  # variables<-read_conductor(taulavariables)
  
  variables<-variables %>% 
    dplyr::filter(!is.na(camp) & !is.na(!!camp_descripcio)) %>% # elimino els que no tenen etiqueta
    dplyr::select(camp,descripcio=!!camp_descripcio) # selecciono els camps necessaris (camp i descripcio) i amb etiqueta
  
  # Els que no tenen etiqueta assignar el mateix nom del camp (Eliminats previament)
  variables<-variables %>% mutate(descripcio=as.character(descripcio))
  variables<-variables %>% mutate(descripcio=ifelse(descripcio=="0" | is.na(descripcio),camp,descripcio)) 
  
  # Etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 1:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
  d
}


#  Formula segos LLISTA DE VARIABLES  D'AJUST     #######################
#####       hi envio la columna de variables amb que vull generar la formula pel compare

#####     x= variables d'ajust / y = resposta / eliminar /  a = Avaluar 

formula_text=function(x="taula1",y="resposta",eliminar=c("IDP"), a=NULL,taulavariables='variables.xls',dt=NA,...) {
  
  # x="cov_art2"
  # y=""
  # eliminar="Prediabetes"
  # a=c("SINBAD","SINBAD.cat")
  # taulavariables=conductor
  
  
  
  # variables <- data.frame(readxl::read_excel(taulavariables))
  # variables <- read_conductor(taulavariables) %>% data.frame()
  variables <- read_conductor(taulavariables,...) %>% data.frame()
  
  # variables[is.na(variables)]<- 0
  x_sym<-rlang::sym(x)
  variables<-variables %>% dplyr::filter(!is.na(!!x_sym))
  
  variables<-variables %>% 
    dplyr::filter(!!x_sym>0) %>% 
    dplyr::arrange(!!x_sym)
  
  # Verificar si dades estan en conductor
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% anti_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value")) %>% pull("camp")
    variables<-variables %>% semi_join(names(dt) %>% as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  llistataula<-c(a,llistataula) %>% unique()
  
  y<-paste(y, paste(llistataula, collapse=" + "), sep=" ~ ")
  
  y
  
}
