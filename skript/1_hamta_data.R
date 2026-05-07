# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. ctrl+A följt av ctrl+enter för att köra skriptet

#Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till TRUE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har uppdaterats med nya data.

uppdatera_data = FALSE
spara_figurer = FALSE

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
    
    cat("Hämtning av data påbörjad\n")
    start_time <- Sys.time()
    

      fellista <- skriptrader_upprepa_om_fel({
      source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/refs/heads/main/Skript/diagram_skogsmark.R")
        gg_skogsmark <- diagram_skogsmark(region = "20",
                                          output_mapp = output_mapp_figur,
                                          returnera_data = TRUE,
                                          diag_areal = TRUE,
                                          diag_andel = TRUE,
                                          ggobjektfilnamn_utan_tid = TRUE,
                                          spara_figur = spara_diagram_som_bildfiler)
        
        ar_min_skogsmark <- as.character(min(skogsmark_df$år))
        ar_max_skogsmark <- as.character(max(skogsmark_df$år))
        
        areal_skogsmark_forandring_procent <-round(((skogsmark_df %>% filter(år==max(år)) %>% .$area_hektar/skogsmark_df %>% filter(år==min(år)) %>% .$area_hektar)-1)*100,0)
        areal_skogsmark_min_ar <- format(skogsmark_df %>% filter(år==min(år)) %>% .$area_hektar,big.mark=" ")
        areal_skogsmark_max_ar <- format(skogsmark_df %>% filter(år==max(år)) %>% .$area_hektar,big.mark=" ")
        
        andel_skogsmark_min_ar <- round(skogsmark_df %>% filter(år==min(år)) %>% .$area_procent,0)
        andel_skogsmark_max_ar <- round(skogsmark_df %>% filter(år==max(år)) %>% .$area_procent,0)
      
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


hamta_data_webbrapport(
  repo_namn = "uppfoljning_strategier_planer",
  uppdatera_data = uppdatera_data,
  spara_diagram_som_bildfiler = FALSE
)
