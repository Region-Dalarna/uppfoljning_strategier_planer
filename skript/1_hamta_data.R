# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. ctrl+A följt av ctrl+enter för att köra skriptet

#Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till TRUE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har uppdaterats med nya data.

uppdatera_data = FALSE
spara_figurer = FALSE

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here,
       glue)

mapp_environment_fil = "g:/skript/projekt/environments/" # OBS: Får ej ändras
repo_namn = "uppfoljning_strategier_planer" # OBS: Får ej ändras

if(uppdatera_data == TRUE){
  
    output_mapp_figur = here("figurer","/")
    
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
    
    cat("Hämtning av data påbörjad\n")
    start_time <- Sys.time()

    skriptrader_upprepa_om_fel({
    source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/refs/heads/main/Skript/diagram_skogsmark.R")
      gg_skogsmark <- diagram_skogsmark(region = "20",
                                        output_mapp = output_mapp_figur,
                                        returnera_data = TRUE,
                                        diag_areal = TRUE,
                                        diag_andel = TRUE,
                                        ggobjektfilnamn_utan_tid = TRUE,
                                        spara_figur = spara_figurer)
      
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

## Kollektivt resande
source(here("Skript","diagram_kollektivt_resande.R"))
gg_kollektivt_resande <- diagram_kollektivt_resande(region_vekt = "20",
                                                    output_mapp = output_mapp_figur,
                                                    returnera_data = TRUE,
                                                    diag_andel_per_invanare = TRUE,
                                                    diag_marknadsandel = TRUE,
                                                    ggobjektfilnamn_utan_tid = TRUE,
                                                    spara_figur = spara_figurer)

# Resor per invånare
resande_per_person_min_ar <- min(kollektivt_resande_df$ar)
resande_per_person_max_ar <- max(kollektivt_resande_df$ar)
resande_per_person_min_ar_varde <- kollektivt_resande_df %>% filter(ar == min(ar)) %>% .$varde
resande_per_person_max_ar_varde <- round(kollektivt_resande_df %>% filter(ar == max(ar)) %>% .$varde,0)

# Marknadsandel
resande_marknadsandel_min_ar <- min(resande_marknadsandel_df$year)
resande_marknadsandel_max_ar <- max(resande_marknadsandel_df$year)
resande_marknadsandel_2021 <- resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == 2021) %>% .$value
resande_marknadandel_min_ar_varde <- resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == min(year)) %>% .$value
resande_marknadsandel_max_ar_varde <- resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == max(year)) %>% .$value

okning_minskning_marknadsandel_klartext <- ifelse(resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == max(year)) %>% .$value>resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == min(year)) %>% .$value,"ökat","minskat")
forandring_marknadsandel <- abs(resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == max(year)) %>% .$value -resande_marknadsandel_df %>% filter(kpi == "Marknadsandel_procent", year == min(year)) %>% .$value)

## Avfall
source(here("Skript","diagram_avfall.R"))
gg_avfall <- diagram_avfall(region_vekt = "20",
                            output_mapp = output_mapp_figur,
                            returnera_data = TRUE,
                            ggobjektfilnamn_utan_tid = TRUE,
                            spara_figur = spara_figurer)

avfall_min_ar <- min(avfall_df %>% filter(variabel_kort== "Insamlat kommunalt avfall totalt, kg/invånare (justerat)") %>% .$ar)
avfall_max_ar <- max(avfall_df %>% filter(variabel_kort== "Insamlat kommunalt avfall totalt, kg/invånare (justerat)") %>% .$ar)

avfall_per_person_min_ar <- avfall_df %>% filter(ar == avfall_min_ar,variabel_kort == "Insamlat kommunalt avfall totalt, kg/invånare (justerat)") %>% .$varde
avfall_per_person_max_ar <- round(avfall_df %>% filter(ar == avfall_max_ar,variabel_kort == "Insamlat kommunalt avfall totalt, kg/invånare (justerat)") %>% .$varde,0)

avfall_brp_min_ar <- min(avfall_brp_df$ar)
avfall_brp_max_ar <- max(avfall_brp_df$ar)

avfall_brp_min_ar_varde <- format(plyr::round_any(avfall_brp_df %>% filter(ar == min(ar)) %>%  .$avfallbrp,10),big.mark = " ")
avfall_brp_max_ar_varde <- format(plyr::round_any(avfall_brp_df %>% filter(ar == max(ar)) %>%  .$avfallbrp,10),big.mark = " ")

## Utsläpp
source(here("Skript","diagram_vaxthusgaser.R"))
gg_utslapp <- diagram_vaxthusgaser(region_vekt = "20",
                                   output_mapp = output_mapp_figur,
                                   diag_bransch = TRUE,
                                   diag_per_invanare = TRUE,
                                   returnera_data = TRUE,
                                   ggobjektfilnamn_utan_tid = TRUE,
                                   spara_figur = spara_figurer)

utslapp_min_ar <- min(vaxthusgaser_df$ar)
utslapp_max_ar <- max(vaxthusgaser_df$ar)
utslapp_min_ar_per_person <- gsub("\\.",",",round(vaxthusgaser_df %>% filter(ar == min(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde,1))
utslapp_max_ar_per_person <- gsub("\\.",",",round(vaxthusgaser_df %>% filter(ar == max(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde,1))

# Beräknar förändringar som krävs för att uppnå mål samt hur vi förändrats sedan 2015
years <- 2045 - as.integer(max(vaxthusgaser_df$ar))
slut_ar_varde = 1.25
borjan_varde = vaxthusgaser_df %>% filter(ar == max(ar),variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde
behovd_minskning <- round(abs(((slut_ar_varde / borjan_varde)^(1 / years) - 1) * 100),0)

utslapp_2015 = vaxthusgaser_df %>% filter(ar == 2015,variabel_kort == "Utsläpp per invånare",region == "Dalarna") %>% .$varde
ar_sedan_2015 <- as.integer(max(vaxthusgaser_df$ar))-2015

forandring_sedan_2015 <- round(abs(((borjan_varde / utslapp_2015)^(1 / ar_sedan_2015) - 1) * 100),0)

## Energiproduktion
source(here("Skript","diagram_energiproduktion.R"))
gg_energiproduktion <- diagram_energiproduktion(region = "20",
                                                output_mapp = output_mapp_figur,
                                                returnera_data = TRUE,
                                                ggobjektfilnamn_utan_tid = TRUE,
                                                spara_figur = spara_figurer)

elproduktion_min_ar <- min(elproduktion_df$ar)
elproduktion_max_ar <- max(elproduktion_df$ar)

elproduktion_vind_min_ar <- format(plyr::round_any(elproduktion_df %>% filter(variabel_kort == "Vindkraft",region == "Dalarna") %>% filter(ar == min(ar)) %>%  .$varde,100),big.mark = " ")
elproduktion_vind_max_ar <- format(plyr::round_any(elproduktion_df %>% filter(variabel_kort == "Vindkraft",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde,100),big.mark = " ")

elproduktion_andel_min_ar <- round((elproduktion_df %>% filter(variabel_kort == "Vindkraft",region == "Dalarna") %>% filter(ar == min(ar)) %>%  .$varde/elproduktion_df %>% filter(variabel_kort == "Totalt",region == "Dalarna") %>% filter(ar == min(ar)) %>%  .$varde)*100,0)
elproduktion_andel_max_ar <- round((elproduktion_df %>% filter(variabel_kort == "Vindkraft",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde/elproduktion_df %>% filter(variabel_kort == "Totalt",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde)*100,0)

elproduktion_sol_2021 <- format(plyr::round_any(elproduktion_df %>% filter(variabel_kort == "Solkraft",region == "Dalarna") %>% filter(ar == 2021) %>%  .$varde,100),big.mark = " ")
elproduktion_sol_max_ar <- format(plyr::round_any(elproduktion_df %>% filter(variabel_kort == "Solkraft",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde,100),big.mark = " ")

solkraft_andel_max_ar <- gsub("\\.",",",round((elproduktion_df %>% filter(variabel_kort == "Solkraft",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde/elproduktion_df %>% filter(variabel_kort == "Totalt",region == "Dalarna") %>% filter(ar == max(ar)) %>%  .$varde)*100,1))

## Energieffektivitet
source(here("Skript","diagram_energieffektivitet.R"))
gg_energieffektivitet <- diagram_energieffektivitet(region = "20",
                                                    output_mapp = output_mapp_figur,
                                                    returnera_data = TRUE,
                                                    diag_tid = TRUE,
                                                    diag_jmf_senaste_ar = TRUE,
                                                    ggobjektfilnamn_utan_tid = TRUE,
                                                    spara_figur = spara_figurer)

energieffektivitet_min_ar <- min(energieffektivitet_df$År)
energieffektivitet_max_ar <- max(energieffektivitet_df$År)

energieffektivitet_forandring_procent <- round((energieffektivitet_df %>% filter(Region == "Dalarna", År == max(År)) %>% .$value/energieffektivitet_df %>% filter(Region == "Dalarna", År == min(År)) %>% .$value-1)*100,0)



