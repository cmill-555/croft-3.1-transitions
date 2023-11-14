

library(pacman)
p_load(tidyverse,readxl,janitor,skimr,freqtables,tibble)

CRoFT.predom.device <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.device.xlsx")
CRoFT.predom.flavors.cat2 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.flavors.cat2.xlsx")
NicConc.2023_0501 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/NicConc.2023_0501.xlsx")

NicConc.2023_0511 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/NicConc.2023_0511.xlsx") #RECLASSIFIED MISREPORTERS


Flavors.main.column <- CRoFT.predom.flavors.cat2 %>% select(pid,visit,predom.flavor.cat2FINAL)
Device.main.column <- CRoFT.predom.device %>% select(pid,visit,device)
NicConc.main.column <- NicConc.2023_0501 %>% select(pid,visit,most.common.nc)

merge.one <- Flavors.main.column %>% left_join(Device.main.column, by = c("pid","visit"))

merge.one <- merge.one %>% rename(flavor = predom.flavor.cat2FINAL)

FDN.2023_0510 <- merge.one %>% left_join(NicConc.2023_0501, by = c("pid","visit")) %>% 
  select(pid,visit,predom.flavor.cat2FINAL,device,most.common.nc) %>% 
  arrange(pid,visit)


######################################################

#########################
### FLAVOR INDICATORS ###
#########################

Flavors.2023_0510 <- FDN.2023_0510 %>% 
  mutate(Sweet = case_when(grepl("sweets/fruit", predom.flavor.cat2FINAL) ~ 1, TRUE ~ 0),
         Hybrid = case_when(grepl("hybrid", predom.flavor.cat2FINAL) ~ 1, TRUE ~ 0),
         Menthol = case_when(grepl("menthol", predom.flavor.cat2FINAL) ~ 1, TRUE ~ 0),
         Tobacco = case_when(grepl("tobacco", predom.flavor.cat2FINAL) ~ 1, TRUE ~ 0),
         Abstain = case_when(grepl("abstain", predom.flavor.cat2FINAL) ~ 1, TRUE ~ 0))

#########################
### DEVICE INDICATORS ###
#########################

Device.2023_0510 <- FDN.2023_0510 %>% 
  mutate(device.rev = case_when(device == "Open-System Pod-Mod" ~ "Open-System P.Mod", TRUE ~ device)) #need to make distinct for grepling

Device.2023_0510 <- Device.2023_0510 %>% 
  mutate(Disposable = case_when(device.rev == "Disposable" ~ "Yes", TRUE ~ "No"),
         Closed.System.Pod = case_when(device.rev == "Closed-System Pod" ~ "Yes", TRUE ~ "No"),
         Open.System.Pod = case_when(device.rev == "Open-System Pod" ~ "Yes", TRUE ~ "No"),
         Open.System.Pod.Mod = case_when(device.rev == "Open-System P.Mod" ~ "Yes", TRUE ~ "No"),
         Open.System.Mod = case_when(device.rev == "Open-System Mod" ~ "Yes", TRUE ~ "No"))

Device.2023_0510 <- Device.2023_0510 %>% select(-device.rev)

###########################
### Nic Conc INDICATORS ###
###########################

Nic.Conc.2023_0510 <- FDN.2023_0510 %>% mutate(Low.Nicotine = case_when(most.common.nc == "<24 mg/ml" ~ 1, TRUE ~ 0))
Nic.Conc.2023_0510 <- Nic.Conc.2023_0510 %>% mutate(High.Nicotine = case_when(most.common.nc == "24+ mg/ml" ~ 1, TRUE ~ 0))




############################################################################################################
#----------------------------------------------------------------------------------------------------------#
############################################################################################################

merges.1 <- Nic.Conc.2023_0510 %>% 
  left_join(Device.2023_0510, by = c("pid","visit")) %>% 
  select(pid, visit, Low.Nicotine, High.Nicotine, 
         Disposable, Closed.System.Pod, Open.System.Pod, Open.System.Pod.Mod, Open.System.Mod)


merges.2 <- merges.1 %>% 
  left_join(Flavors.2023_0510, by = c("pid","visit")) %>% 
  select(pid, visit, Low.Nicotine, High.Nicotine,
         Disposable, Closed.System.Pod, Open.System.Pod, Open.System.Pod.Mod, Open.System.Mod,
         Sweet, Hybrid, Menthol, Tobacco, Abstain)

#----------------------------------------
#consistently-formatted dummy vars (0,1)
CRoFT.combos.dta <- merges.2 %>% 
  mutate_at(vars(Low.Nicotine, High.Nicotine,
                 Disposable, Closed.System.Pod, Open.System.Pod, Open.System.Pod.Mod, Open.System.Mod,
                 Sweet, Hybrid, Menthol, Tobacco, Abstain), ~ str_replace(., "Yes", "1"))

CRoFT.combos.dta <- CRoFT.combos.dta %>% 
  mutate_at(vars(Low.Nicotine, High.Nicotine,
                 Disposable, Closed.System.Pod, Open.System.Pod, Open.System.Pod.Mod, Open.System.Mod,
                 Sweet, Hybrid, Menthol, Tobacco, Abstain), ~ str_replace(., "No", "0"))

#change indicator to name from columns (por UpSet package functions to come below)
CRoFT.combos.dta[] <- Map(function(n, x) replace(x, x == 1, n), names(CRoFT.combos.dta), CRoFT.combos.dta)
CRoFT.combos.dta[] <- Map(function(n, x) replace(x, x == "0", n), NA, CRoFT.combos.dta)

CRoFT.combos.dta <- CRoFT.combos.dta %>% mutate(pid = as.numeric(case_when(pid %in% "pid" ~ "1", TRUE ~ pid)),
                                                visit = as.numeric(case_when(visit %in% "visit" ~ "1", TRUE ~ visit)))

CRoFT.combos.dta <- CRoFT.combos.dta %>%
  mutate(across(Low.Nicotine:Tobacco, ~ case_when(Abstain == "Abstain" ~ NA_character_, TRUE ~ .)))


#------------------------------------------------------------------------------------------------

CRoFT.combos.dta.UpSet <- CRoFT.combos.dta %>% 
  unite(col = "all_ENDS_attrbs",
        c(Low.Nicotine, High.Nicotine,
          Disposable, Closed.System.Pod, Open.System.Pod, Open.System.Pod.Mod, Open.System.Mod,
          Sweet, Hybrid, Menthol, Tobacco), 
        sep = "; ", remove = TRUE, na.rm = TRUE) %>% 
  mutate(all_ENDS_attrbs_list = as.list(strsplit(all_ENDS_attrbs, "; "))) %>% 
  select(pid,visit,all_ENDS_attrbs,all_ENDS_attrbs_list)

#CRoFT.combos.dta.UpSet <- CRoFT.combos.dta.UpSet %>% mutate(across(everything(),~ gsub(".ind","", .)))


#CRoFT.combos.dta.UpSet <- CRoFT.combos.dta.UpSet %>% mutate(sweet,~ gsub(".ind","", .)))


#------------------------------------------------------------------------------------------------
pacman::p_load(ggupset)
CRoFT.combos.dta.UpSet$all_ENDS_attrbs_list
ggplot(
  data = CRoFT.combos.dta.UpSet,
  mapping = aes(x = all_ENDS_attrbs_list)) +
  geom_bar() +
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("Low.Nicotine", "High.Nicotine",
             "Disposable", "Closed.System.Pod", "Open.System.Pod", "Open.System.Pod.Mod", "Open.System.Mod",
             "Sweet", "Hybrid", "Menthol", "Tobacco"))+
  labs(
    title = "ENDS Product Characteristics",
    subtitle = "CRoFT Study",
    caption = "",
    x = "ENDS Attributes",
    y = "No. of visits")


#------------------------------------------------------------------------------------------------





############################################################################################################
#----------------------------------------------------------------------------------------------------------#
############################################################################################################

CRoFT.combos.dta.UpSetR <- CRoFT.combos.dta %>% 
  mutate("Low Nicotine" = as.numeric(case_when(Low.Nicotine == "Low.Nicotine" ~ "1", TRUE ~ "0")),
         "High Nicotine" = as.numeric(case_when(High.Nicotine == "High.Nicotine" ~ "1", TRUE ~ "0")),
         "Disposable" = as.numeric(case_when(Disposable == "Disposable" ~ "1", TRUE ~ "0")),
         "Closed System Pod" = as.numeric(case_when(Closed.System.Pod == "Closed.System.Pod" ~ "1", TRUE ~ "0")),
         "Open System Pod" = as.numeric(case_when(Open.System.Pod == "Open.System.Pod" ~ "1", TRUE ~ "0")),
         "Open System Pod-Mod" = as.numeric(case_when(Open.System.Pod.Mod == "Open.System.Pod.Mod" ~ "1", TRUE ~ "0")),
         "Open System Mod" = as.numeric(case_when(Open.System.Mod == "Open.System.Mod" ~ "1", TRUE ~ "0")),
         "Sweet" = as.numeric(case_when(Sweet == "Sweet" ~ "1", TRUE ~ "0")),
         "Hybrid" = as.numeric(case_when(Hybrid == "Hybrid" ~ "1", TRUE ~ "0")),
         "Menthol" = as.numeric(case_when(Menthol == "Menthol" ~ "1", TRUE ~ "0")),
         "Tobacco" = as.numeric(case_when(Tobacco == "Tobacco" ~ "1", TRUE ~ "0")),
         "Abstain" = as.numeric(case_when(Abstain == "Abstain" ~ "1", TRUE ~ "0")))

#create uid for pid + visit
#CRoFT.combos.dta.UpSetR <- rowid_to_column(CRoFT.combos.dta.UpSetR, "UID") %>% select(-pid,-visit,-Abstain)

CRoFT.combos.dta.UpSetR <- as.data.frame(CRoFT.combos.dta.UpSetR) #error codes unless you do this for some odd reason


#----------------------------------------------------------------------------------------------------
set_vars <- c("Low Nicotine", "High Nicotine", "Disposable", "Closed System Pod", "Open System Pod", 
              "Open System Pod-Mod", "Open System Mod", "Sweet", "Hybrid", "Menthol", "Tobacco")
p_load(UpSetR)
upset(CRoFT.combos.dta.UpSetR, 
      sets = set_vars,
      mb.ratio = c(0.7, 0.3),
      number.angles = 0, point.size = 3, line.size = 1, 
      mainbar.y.label = "ENDS Attribute Combinations", sets.x.label = "ENDS Attribute Prevalence",
      order.by = "freq",
      keep.order = TRUE,
      text.scale = c(1.5, 1.3, 1, 1, 1.2, 0.9),#c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
      nintersects = 20)



#------------------------------------------------------------------------------------------------------

sets <- c("Low Nicotine", "High Nicotine", "Disposable", "Closed System Pod", "Open System Pod", 
          "Open System Pod-Mod", "Open System Mod", "Sweet", "Hybrid", "Menthol", "Tobacco")

product.char.class <- c("Nicotine", "Nicotine", "Device", "Device", "Device", 
                        "Device", "Device", "Flavor", "Flavor", "Flavor", "Flavor")

metadata <- as.data.frame(cbind(sets, product.char.class))
names(metadata) <- c("sets", "product.char.class")


upset(CRoFT.combos.dta.UpSetR, 
      sets = set_vars,
      mb.ratio = c(0.7, 0.3),
      number.angles = 0, point.size = 3, line.size = 1, 
      mainbar.y.label = "ENDS Attribute Combinations", sets.x.label = "ENDS Attribute Prevalence",
      order.by = "freq",
      keep.order = TRUE,
      text.scale = c(1.5, 1.3, 1, 1, 1.2, 0.9),#c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
      nintersects = 20,
      set.metadata = list(data = metadata, 
                          plots = list(list(type = "matrix_rows",
                                            column = "product.char.class", 
                                            colors = c(Nicotine = "forestgreen", Device = "dodgerblue4", Flavor = "firebrick4"),
                                            alpha = 0.5))))




#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


CRoFT.visit.dates <- read_excel("/Users/Connor/Downloads/Calculated Visits_2023-05-10_21-44-09.xlsx")

#clean up full-sample dataframes
CRoFT.visit.dates <- CRoFT.visit.dates %>% clean_names()
CRoFT.visit.dates <- CRoFT.visit.dates %>% rename(pid = participant_id)
CRoFT.visit.dates$pid <- as.numeric(CRoFT.visit.dates$pid)

CRoFT.visit.dates$visit <- gsub("Visit ", "", CRoFT.visit.dates$visit)
CRoFT.visit.dates$visit <- as.numeric(CRoFT.visit.dates$visit)


noncompleters_list <- list(4,11,12,15,18,19,20,22,25,29,30,31,33,37,38,39,40,41,46,56,57,65,67,68,
                           70,71,72,74,75,80,81,82,89,91,93,96,97,99,104,108,111,113,116,119,121,123)

CRoFT.visit.dates <-CRoFT.visit.dates %>% filter(!(pid %in% noncompleters_list))

pacman::p_load(zoo)
CRoFT.visit.dates$year.month <- as.yearmon(paste(CRoFT.visit.dates$year, CRoFT.visit.dates$month), "%Y %m")

CRoFT.combos.dta.UpSetR.date_boxplot <- CRoFT.visit.dates %>% 
  select(pid,visit,month) %>% 
  left_join(CRoFT.combos.dta.UpSetR, by = c("pid", "visit"))

CRoFT.combos.dta.UpSetR.date_boxplot <- CRoFT.combos.dta.UpSetR.date_boxplot %>% 
  select(pid, visit, month, "Low Nicotine", "High Nicotine", "Disposable", 
         "Closed System Pod", "Open System Pod", "Open System Pod-Mod", 
         "Open System Mod", "Sweet", "Hybrid", "Menthol", "Tobacco")



CRoFT.combos.dta.UpSetR.date_boxplot <- as.data.frame(CRoFT.combos.dta.UpSetR.date_boxplot)

upset(CRoFT.combos.dta.UpSetR.date_boxplot, 
      #sets = set_vars,
      #mb.ratio = c(0.7, 0.3),
      #number.angles = 0, point.size = 3, line.size = 1, 
      #mainbar.y.label = "ENDS Attribute Combinations", sets.x.label = "ENDS Attribute Prevalence",
      #order.by = "freq",
      #keep.order = TRUE,
      #text.scale = c(1.5, 1.3, 1, 1, 1.2, 0.9),#c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
      nintersects = 10,
      boxplot.summary = c("month"))
#---------------------------------------------------------------------------------------------------


library(grid)
library(plyr)
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")


upset(movies, 
      boxplot.summary = "ReleaseDate")


