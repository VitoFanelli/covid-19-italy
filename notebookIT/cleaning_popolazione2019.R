# 0. setup ----------------------------------------------------------------
setwd(.rs.getProjectDirectory())
library(tidyverse)

# 1. load dati ------------------------------------------------------------
popolazione2019 <- read.csv("./notebookIT/popolazione2019.csv", h = T, 
                            sep = ",", stringsAsFactors = F)

popolazione2019 <- popolazione2019 %>% select(Territorio, Sesso, ETA1, Value)

# 2. filtro regioni -------------------------------------------------------
regioni <- c("Valle d'Aosta / Vallée d'Aoste", "Piemonte", "Liguria", "Lombardia", 
             "Trentino Alto Adige / Südtirol", "Veneto", "Friuli-Venezia Giulia", 
             "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", 
             "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", 
             "Calabria", "Sicilia", "Sardegna")

popolazione2019_regione <- popolazione2019 %>% filter(Territorio %in% regioni)

popolazione2019_regione <- popolazione2019_regione %>% 
  mutate(Territorio = case_when(
    Territorio == "Valle d'Aosta / Vallée d'Aoste" ~ "Valle d'Aosta",
    Territorio == "Trentino Alto Adige / Südtirol" ~ "Trentino Alto Adige",
    #Territorio == "Emilia-Romagna" ~ "Emilia Romagna",
    Territorio == "Friuli-Venezia Giulia" ~ "Friuli Venezia Giulia",
    TRUE ~ Territorio
  ))

table(popolazione2019_regione$Territorio)
# ok

# 3. pulizia dati ---------------------------------------------------------
popolazione2019_regione_ok <- popolazione2019_regione %>% 
  filter(Sesso == "totale", ETA1 != "TOTAL") %>% 
  group_by(Territorio, Sesso, ETA1) %>% 
  summarise(Value = max(Value)) %>% 
  ungroup() %>% 
  mutate(ETA1 = case_when(
    ETA1 == "Y_GE100" ~ "Y100",
    TRUE ~ ETA1
  ))

popolazione2019_regione_ok$ETA1 <- as.numeric(gsub("Y", "", popolazione2019_regione_ok$ETA1))

# 4. eta media -----------------------------------------------------------
popolazione2019_regione_eta <- popolazione2019_regione_ok %>% 
  mutate(ETA2 = ETA1 * Value) %>% 
  group_by(Territorio) %>% 
  summarise(
    popolazione = sum(Value),
    eta_media = round(sum(ETA2)/sum(Value),2)) %>% 
  ungroup()

colnames(popolazione2019_regione_eta)[1] <- "regione"

# 5. salvataggio dataset --------------------------------------------------
save(popolazione2019_regione_eta, file = "./notebookIT/popolazione2019_regione_eta.RData")

