
hamta_data_webbrapport <- function(
    repo_namn = NULL, 
    github_lokal_sokvag = "c:/gh/",
    uppdatera_data = FALSE, 
    spara_diagram_som_bildfiler = FALSE,               # Oftast vill vi inte spara bildfiler men det finns en möjlighet att göra det
    vald_region = "20",
    here_mapp_sokvag_figurer = "figurer/",
    mapp_environment_fil = "g:/skript/projekt/environments/"
  ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  if (is.null(repo_namn) & !is.null(rstudioapi::getActiveProject())) {
    repo_lokal_sokvag <- here()
  } else {
    repo_lokal_sokvag <- glue("{github_lokal_sokvag}{repo_namn}")
  }
  if (!str_ends(repo_lokal_sokvag, "/")) repo_lokal_sokvag <- paste0(repo_lokal_sokvag, "/")
  
  output_mapp_figur = glue("{repo_lokal_sokvag}{here_mapp_sokvag_figurer}")
  
  if(uppdatera_data == TRUE){
    
    cat("Hämtning av data påbörjad")
    start_time <- Sys.time()
  
    # Diagram 1
    fellista <- skriptrader_upprepa_om_fel({
      source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_stod_bakgrund.R")
      gg_ek_stod <- diagram_ek_stod_bakgrund_SCB (output_mapp = output_mapp_figur,
                                                  skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                  returnera_data_rmarkdown = TRUE)
      
      ek_stod_manad_ar_forsta <- first(ekonomiskt_stod_df$månad_år) %>% as.character()
      ek_stod_manad_ar_sista <- last(ekonomiskt_stod_df$månad_år) %>% as.character()
      
      ek_stod_totalt_sista <- format(ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="totalt") %>% .$antal,big.mark = " ")
      
      ek_stod_skillnad_forsta <- ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal
      ek_stod_skillnad_senaste <- ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal
      
    })     # innan funktion är i produktion, lägg till: ",max_forsok = 1" mellan "}" och ")" på denna rad
    
    # Diagram 2
    fellista <- skriptrader_upprepa_om_fel({
      source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_tidsserie_SCB.R")
      gg_arbetsloshet_tidsserie <- diagram_arbetsmarknadsstatus_tidsserie (spara_figur = spara_diagram_som_bildfiler, 
                                                                           output_mapp_figur = output_mapp_figur,
                                                                           returnera_data = TRUE,
                                                                           marginal_yaxis_facet = c(0.02,0.02),
                                                                           diagram_facet = TRUE,
                                                                           returnera_figur = TRUE)
      
      arbetsloshet_tidsserie_ar <-  unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$ar))
      arbetsloshet_tidsserie_manad <- unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$manad_long))
      
      # Totalt
      arbetsloshet_tidserie_Dalarna_totalt_max_ar <- arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$ar %>% .[1]
      arbetsloshet_tidserie_Dalarna_totalt_max_manad <- arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$manad_long %>% .[1]
      arbetsloshet_tidserie_Dalarna_totalt_max_varde <- gsub("\\.",",", arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$varde %>% .[1])
      
      arbetsloshet_tidserie_Dalarna_totalt_senaste_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% .$varde))
      
      # Inrikes/utrikes födda
      arbetsloshet_tidserie_Dalarna_inrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$varde))
      arbetsloshet_tidserie_Dalarna_utrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="utrikes född") %>% .$varde))
    })
    
    # Sparar global environment i R. Detta för att man skall slippa hämta data varje gång
    save.image(file = glue("{mapp_environment_fil}{repo_namn}.RData"))
    
    end_time <- Sys.time()
    elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
    cat(sprintf("Hämtning av data klar: Det tog %.2f minuter.", elapsed_time))
    cat("\n\n")
    
    
  } else {
    load(glue("{mapp_environment_fil}{repo_namn}.RData"))
  }


} # slut funktion
