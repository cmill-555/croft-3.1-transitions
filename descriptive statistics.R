
library(pacman)
p_load(tidyverse,readxl,janitor,skimr,freqtables,tibble)

CRoFT.predom.device <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.device.xlsx")
CRoFT.predom.flavors.cat2 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.flavors.cat2.xlsx")
NicConc.2023_0501 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/NicConc.2023_0501.xlsx")

Flavors.main.column <- CRoFT.predom.flavors.cat2 %>% select(pid,visit,predom.flavor.cat2FINAL) %>% rename(flavor = predom.flavor.cat2FINAL)
Device.main.column <- CRoFT.predom.device %>% select(pid,visit,device)
NicConc.main.column <- NicConc.2023_0501 %>% select(pid,visit,most.common.nc)

merge.one <- Flavors.main.column %>% left_join(Device.main.column, by = c("pid","visit"))

FDN.2023_0502 <- merge.one %>% left_join(NicConc.2023_0501, by = c("pid","visit")) %>% 
  select(pid,visit,flavor,device,most.common.nc) %>% 
  arrange(pid,visit)


######################################################

##################################
### FLAVOR PREVALENCE AT VISIT ###
##################################

Flavors.2023_0503 <- FDN.2023_0502 %>% 
  mutate(fruitsweets.ind = case_when(grepl("sweets/fruit", flavor) ~ 1, TRUE ~ 0),
         hybrid.ind = case_when(grepl("hybrid", flavor) ~ 1, TRUE ~ 0),
         menthol.ind = case_when(grepl("menthol", flavor) ~ 1, TRUE ~ 0),
         tobacco.ind = case_when(grepl("tobacco", flavor) ~ 1, TRUE ~ 0))

pacman::p_load(gmodels)
CrossTable(Flavors.2023_0503$fruitsweets.ind)
CrossTable(Flavors.2023_0503$hybrid.ind)
CrossTable(Flavors.2023_0503$menthol.ind)
CrossTable(Flavors.2023_0503$tobacco.ind)

skim(Flavors.2023_0503)

#--------------------

###################################
### FLAVOR TRANSITIONS AT VISIT ###
###################################
Flavors.2023_0503$flavor.ordered <- Flavors.2023_0503$flavor %>% 
  str_split(., '; ') %>% lapply(., 'sort') %>%  lapply(., 'paste', collapse='; ') %>% unlist(.)

Flavors.2023_0503 <- Flavors.2023_0503 %>% mutate(lag.flavor = lag(flavor.ordered), lead.flavor = lead(flavor.ordered)) %>% arrange(pid,visit)


Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  mutate(transition.ind = case_when(visit ==1 ~ NA, grepl("abstain",flavor.ordered) ~ "Abstained", 
                                    lag.flavor == flavor.ordered ~ "No",
                                    flavor.ordered !="abstain" & grepl("abstain",lag.flavor) ~ "Immediate post-abstain visit", 
                                    TRUE ~ "Yes"))

Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  unite("transition.type", c("lag.flavor", "flavor.ordered"), sep= " --> ", na.rm = TRUE, remove = FALSE)


#Remove unite var for stable visits
Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  mutate(transition.type = case_when(visit ==1 ~ "Baseline visit", 
                                     transition.ind == "No"  ~ "Stable",
                                     transition.ind == "Abstained" ~ "Abstained", 
                                     TRUE ~ transition.type))



flavor.abst_return <- Flavors.2023_0503 %>% filter(grepl("abstain -->",transition.type) & !grepl("abstain --> abstain",transition.type))


flavor.abst_return.preced_rows <- Flavors.2023_0503 %>% 
  filter(pid==9 | pid==34 | pid==47 | pid==48 | pid==122) %>% select(pid, visit, transition.type, flavor.ordered)

flavor.abst_return <- flavor.abst_return %>% 
  mutate(transition.abst_return = case_when(pid==9 & visit==11 ~ "Stable",
                                            pid==34 & visit ==5 ~ "Stable",
                                            pid==47 & visit ==5 ~ "sweets/fruit --> menthol",
                                            pid==48 & visit ==9 ~ "sweets/fruit --> hybrid; sweets/fruit",
                                            pid==122 & visit ==7 ~ "Stable")) %>% 
  select(pid,visit,transition.abst_return) %>% arrange(pid,visit)

Flavors.2023_0503 <- Flavors.2023_0503 %>% left_join(flavor.abst_return, by = c("pid","visit"))

Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  mutate(flavor.trans.type = case_when(!is.na(transition.abst_return) ~ transition.abst_return, TRUE ~ transition.type))

Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  mutate(transition.ind = case_when(transition.ind == "Immediate post-abstain visit" & transition.type == "Stable"  ~ "No",
                                    transition.ind == "Immediate post-abstain visit" & transition.type != "Stable" ~ "Yes",
                                    TRUE ~ transition.ind))

Flavor.transitions.data <- Flavors.2023_0503 %>% filter(flavor.trans.type!="Baseline visit") %>% select(pid,visit,flavor.trans.type)

#PERCENTAGE OF Non-Baseline VISITS WITH VARIOUS FLAVOR TRANSITIONS
flavor.trans.perc <- Flavor.transitions.data %>% freq_table(flavor.trans.type)

#Manually updating multi-use visit transitions (added v. subtracted any flavor types)
Flavor.transitions.complex <- flavor.trans.perc %>% filter(grepl("; ", cat)) %>% select(cat)
Flavor.transitions.complex <- rowid_to_column(Flavor.transitions.complex, "row.ID")

Flavor.transitions.complex <- Flavor.transitions.complex %>% 
  mutate(flavor.complex.trans = case_when(row.ID %in% c(1:3,9,14:17,21:24,27:29) ~ "Added Flavor",
                                          TRUE ~ "Subtracted Flavor")) %>% 
  rename(flavor.trans.type = cat)


#Flavor.transitions.one_to_one <- flavor.trans.perc %>% filter(!grepl("; ", cat)) %>% select(cat)


#merge data
Flavor.transitions.data <- Flavor.transitions.data %>% left_join(Flavor.transitions.complex, by = "flavor.trans.type")

Flavor.transitions.data <- Flavor.transitions.data %>% 
  mutate(flavor.trans.type.simple = case_when(!is.na(flavor.complex.trans) ~ flavor.complex.trans,
                                              TRUE ~ flavor.trans.type)) %>% 
  select(pid, visit, flavor.trans.type, flavor.trans.type.simple)


flavor.trans.props <- Flavor.transitions.data %>% freq_table(flavor.trans.type.simple)
flavor.trans.props <- rowid_to_column(flavor.trans.props, "row.ID")

flavor.trans.props <- flavor.trans.props %>% mutate(prop = (n / 60)*100) %>% select(cat,n,prop)

flavor.trans.props <- flavor.trans.props %>% 
  mutate(prop = case_when(cat %in% c("Abstained", "Added Flavor", "Subtracted Flavor", "Stable") ~ NA,
                          TRUE ~ prop))

flavor.trans.props$prop <- sprintf(flavor.trans.props$prop, fmt = '%#.2f')

########################################
### DOING MORE WITH COMPLEX SWITCHES ###
########################################

Flavor.transitions.complex <- Flavor.transitions.complex %>% separate(flavor.trans.type, c("lag.flavors", "current.flavors"), " --> ", remove=FALSE)

Flavor.transitions.complex <- Flavor.transitions.complex %>%
  mutate(partial.sweet.add = case_when(!grepl("sweets/fruit",lag.flavors) & grepl("sweets/fruit",current.flavors) ~ 1,
                                       TRUE ~ 0),
         partial.sweet.subtr = case_when(grepl("sweets/fruit",lag.flavors) & !grepl("sweets/fruit",current.flavors) ~ 1,
                                         TRUE ~ 0),
         
         partial.hybrid.add = case_when(!grepl("hybrid",lag.flavors) & grepl("hybrid",current.flavors) ~ 1,
                                        TRUE ~ 0),
         partial.hybrid.subtr = case_when(grepl("hybrid",lag.flavors) & !grepl("hybrid",current.flavors) ~ 1,
                                          TRUE ~ 0),
         
         partial.menthol.add = case_when(!grepl("menthol",lag.flavors) & grepl("menthol",current.flavors) ~ 1,
                                         TRUE ~ 0),
         partial.menthol.subtr = case_when(grepl("menthol",lag.flavors) & !grepl("menthol",current.flavors) ~ 1,
                                           TRUE ~ 0),
         
         partial.tobacco.add = case_when(!grepl("tobacco",lag.flavors) & grepl("tobacco",current.flavors) ~ 1,
                                         TRUE ~ 0),
         partial.tobacco.subtr = case_when(grepl("tobacco",lag.flavors) & !grepl("tobacco",current.flavors) ~ 1,
                                           TRUE ~ 0))


Flavor.transitions.data <- Flavor.transitions.data %>% 
  left_join(Flavor.transitions.complex, by = "flavor.trans.type")

Flavor.transitions.data <- Flavor.transitions.data %>% 
  select(pid,visit,flavor.trans.type,flavor.trans.type.simple,"partial.sweet.add":"partial.tobacco.subtr")

Flavor.transitions.complex.stats <- Flavor.transitions.data %>% 
  filter(flavor.trans.type.simple %in% c("Added Flavor", "Subtracted Flavor"))


partial.hybrid.subtr.props <- Flavor.transitions.complex.stats %>% freq_table(partial.hybrid.subtr) %>% select(var,n)
partial.hybrid.add.props <- Flavor.transitions.complex.stats %>% freq_table(partial.hybrid.add)%>% select(var,n)
partial.sweet.add.props <- Flavor.transitions.complex.stats %>% freq_table(partial.sweet.add) %>% select(var,n)
partial.sweet.subtr.props <- Flavor.transitions.complex.stats %>% freq_table(partial.sweet.subtr) %>% select(var,n)
partial.menthol.add.props <- Flavor.transitions.complex.stats %>% freq_table(partial.menthol.add) %>% select(var,n)
partial.menthol.subtr.props <- Flavor.transitions.complex.stats %>% freq_table(partial.menthol.subtr) %>% select(var,n)
partial.tobacco.subtr.props <- Flavor.transitions.complex.stats %>% freq_table(partial.tobacco.subtr) %>% select(var,n)
partial.tobacco.add.props <- Flavor.transitions.complex.stats %>% freq_table(partial.tobacco.add) %>% select(var,n)


#MAnual ordering of most common partial switches
partial.switch.props <- 
  bind_rows(partial.hybrid.subtr.props[2,],
            partial.hybrid.add.props[2,],
            partial.sweet.add.props[2,],
            partial.sweet.subtr.props[2,],
            partial.menthol.add.props[2,],
            partial.menthol.subtr.props[2,],
            partial.tobacco.subtr.props[2,],
            partial.tobacco.add.props[2,])

partial.switch.props <- partial.switch.props %>% mutate(prop = (n/84)*100)

partial.switch.props$prop <- sprintf(partial.switch.props$prop, fmt = '%#.2f')
View(partial.switch.props)


###################################################################
### Percent of visits where ONE predominant flavor established  ###
###################################################################

Flavors.2023_0503 <- Flavors.2023_0503 %>% 
  mutate(single.flavor.ind = case_when(!grepl("; ", flavor) ~ 1, TRUE ~ 0))

Flavors.2023_0503 %>% filter(flavor != "abstain") %>% freq_table(single.flavor.ind)



##################################################################
### How many participants had NO flavor changes across visits? ###
##################################################################

Flavors.2023_0503.no_abstain <- Flavors.2023_0503 %>% filter(flavor != "abstain") %>% select(pid,visit,flavor.ordered)


Flavors.2023_0503.no_abstain <- Flavors.2023_0503.no_abstain %>% 
  group_by(pid) %>% 
  mutate(change = case_when(flavor.ordered != lag(flavor.ordered) ~ TRUE, 
                            TRUE ~ FALSE),
         n_change = cumsum(change))


Flavors.2023_0503.no_abstain <- Flavors.2023_0503.no_abstain %>% 
  mutate(stable.flavor = case_when(visit==12 & n_change == 0 ~ 1,
                                   visit==12 & n_change > 0 ~ 0,
                                   TRUE ~ NA))

stable.flavor.wide <- Flavors.2023_0503.no_abstain %>% filter(!is.na(stable.flavor))
#NOTE: PID 092 had abstinent visit at V12; need to add them to denominator; they had ONE flavor switch early, 
#so NOT completely stable
stable.flavor.wide <- stable.flavor.wide %>% 
  select(pid,visit,n_change, stable.flavor)

stable.flavor.wide %>% freq_table(stable.flavor)


##################################################################
### Avg flavor changes over study (among any switchers (n=37)  ###
##################################################################

flavor.wide.notstable <- stable.flavor.wide %>% filter(n_change != 0)
mean(flavor.wide.notstable$n_change)
sd(flavor.wide.notstable$n_change)

#########################################################################
### Proportion of visits that were within flavor-stable participants  ###
#########################################################################

#SWEET
intermed <- Flavors.2023_0503 %>% left_join(stable.flavor.wide, by = "pid") %>% select(pid,visit.x,flavor,stable.flavor)
sweet.visits <- intermed %>% filter(grepl("sweets/fruit",flavor))
sweet.visits %>% freq_table(stable.flavor)
# n=10 NA; this is participant 92 who had abstinent v12 which I filtered out way above... add together!

#HYBRID 
intermed <- Flavors.2023_0503 %>% left_join(stable.flavor.wide, by = "pid") %>% select(pid,visit.x,flavor,stable.flavor)
hybrid.visits <- intermed %>% filter(grepl("hybrid",flavor))
hybrid.visits %>% freq_table(stable.flavor) 
# n=1 NA; this is participant 92 who had abstinent v12 which I filtered out way above... add together!

#MENTHOL
intermed <- Flavors.2023_0503 %>% left_join(stable.flavor.wide, by = "pid") %>% select(pid,visit.x,flavor,stable.flavor)
menthol.visits <- intermed %>% filter(grepl("menthol",flavor))
menthol.visits %>% freq_table(stable.flavor)

#TOBACCO
intermed <- Flavors.2023_0503 %>% left_join(stable.flavor.wide, by = "pid") %>% select(pid,visit.x,flavor,stable.flavor)
tobacco.visits <- intermed %>% filter(grepl("tobacco",flavor))
tobacco.visits %>% freq_table(stable.flavor)






######################################################################################################
######################################################################################################

##################################
### DEVICE PREVALENCE AT VISIT ###
##################################

Device.2023_0503 <- FDN.2023_0502 %>% 
  mutate(device.rev = case_when(device == "Open-System Pod-Mod" ~ "Open-System P.Mod", TRUE ~ device))

#---------------------------------
Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(disposable.ind = case_when(device.rev == "Disposable" ~ "Yes", TRUE ~ "No"),
         closed.pod.ind = case_when(device.rev == "Closed-System Pod" ~ "Yes", TRUE ~ "No"),
         open.pod.ind = case_when(device.rev == "Open-System Pod" ~ "Yes", TRUE ~ "No"),
         open.podmod.ind = case_when(device.rev == "Open-System P.Mod" ~ "Yes", TRUE ~ "No"),
         open.mod.ind = case_when(device.rev == "Open-System Mod" ~ "Yes", TRUE ~ "No"))

Device.2023_0503 <- Device.2023_0503 %>% select(-device.rev)

skim(Device.2023_0503)

#---------------------------------

Device.2023_0503 %>% count(disposable.ind) %>% mutate(prop = prop.table(n))
Device.2023_0503 %>% count(closed.pod.ind) %>% mutate(prop = prop.table(n))
Device.2023_0503 %>% count(open.pod.ind) %>% mutate(prop = prop.table(n))
Device.2023_0503 %>% count(open.podmod.ind) %>% mutate(prop = prop.table(n))
Device.2023_0503 %>% count(open.mod.ind) %>% mutate(prop = prop.table(n))

Device.2023_0503 %>% freq_table(disposable.ind)
Device.2023_0503 %>% freq_table(closed.pod.ind)
Device.2023_0503 %>% freq_table(open.pod.ind)
Device.2023_0503 %>% freq_table(open.podmod.ind)
Device.2023_0503 %>% freq_table(open.mod.ind)



###################################
### DEVICE TRANSITIONS AT VISIT ###
###################################
Device.2023_0503$device.ordered <- Device.2023_0503$device %>% 
  str_split(., '; ') %>% lapply(., 'sort') %>%  lapply(., 'paste', collapse='; ') %>% unlist(.)


Device.2023_0503 <- Device.2023_0503 %>% mutate(lag.device = lag(device.ordered), lead.device = lead(device.ordered)) %>% arrange(pid,visit)


Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(transition.ind = case_when(visit ==1 ~ NA, grepl("Abstained",device.ordered) ~ "Abstained", 
                                    lag.device == device.ordered ~ "No",
                                    device.ordered !="Abstained" & grepl("Abstained",lag.device) ~ "Immediate post-abstain visit", 
                                    TRUE ~ "Yes"))

Device.2023_0503 <- Device.2023_0503 %>% 
  unite("transition.type", c("lag.device", "device.ordered"), sep= " --> ", na.rm = TRUE, remove = FALSE)

#Remove unite var for stable visits
Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(transition.type = case_when(visit ==1 ~ "Baseline visit", 
                                     transition.ind == "No" | is.na(transition.ind)  ~ "Stable",
                                     transition.ind == "Abstained" ~ "Abstained", 
                                     TRUE ~ transition.type))


#confirming or reclassifying (manually) visits where participants are returning from abstained visit(s)
device.abst_return <- Device.2023_0503 %>% filter(grepl("Abstained -->",transition.type))

device.abst_return.preced_rows <- Device.2023_0503 %>% 
  filter(pid==9 | pid==34 | pid==47 | pid==48 | pid==122) %>% select(pid, visit, transition.type, device.ordered)

device.abst_return <- device.abst_return %>% 
  mutate(transition.abst_return = case_when(pid==9 & visit==11 ~ "Stable",
                                            pid==34 & visit ==5 ~ "Open-System Mod --> Open-System Pod-Mod",
                                            pid==47 & visit ==5 ~ "Open-System Mod --> Closed-System Pod",
                                            pid==48 & visit ==9 ~ "Open-System Mod --> Disposable",
                                            pid==122 & visit ==7 ~ "Stable")) %>% 
  select(pid,visit,transition.abst_return) %>% arrange(pid,visit)


Device.2023_0503 <- Device.2023_0503 %>% left_join(device.abst_return, by = c("pid","visit"))

Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(device.trans.type = case_when(!is.na(transition.abst_return) ~ transition.abst_return, TRUE ~ transition.type))

Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(transition.ind = case_when(transition.ind == "Immediate post-abstain visit" & transition.type == "Stable"  ~ "No",
                                    transition.ind == "Immediate post-abstain visit" & transition.type != "Stable" ~ "Yes",
                                    TRUE ~ transition.ind))

Device.transitions.data <- Device.2023_0503 %>% filter(device.trans.type!="Baseline visit") %>% select(pid,visit,device.trans.type)

#PERCENTAGE OF Non-Baseline VISITS WITH VARIOUS DEVICE TRANSITIONS
device.trans.perc <- Device.transitions.data %>% freq_table(device.trans.type)

#Manually updating multi-use visit transitions (added v. subtracted any device types)
Device.transitions.complex <- device.trans.perc %>% filter(grepl("; ", cat)) %>% select(cat)
Device.transitions.complex <- rowid_to_column(Device.transitions.complex, "row.ID")

Device.transitions.complex <- Device.transitions.complex %>% 
  mutate(device.complex.trans = case_when(row.ID %in% c(1,6,9:11,13:15,19:20) ~ "Added Device",
                                          row.ID %in% c(2:5,7:8,12,16:18) ~ "Subtracted Device")) %>% 
  rename(device.trans.type = cat)

#merge data
Device.transitions.data <- Device.transitions.data %>% left_join(Device.transitions.complex, by = "device.trans.type")

Device.transitions.data <- Device.transitions.data %>% 
  mutate(device.trans.type.simple = case_when(!is.na(device.complex.trans) ~ device.complex.trans,
                                              TRUE ~ device.trans.type)) %>% 
  select(pid, visit, device.trans.type, device.trans.type.simple)



device.trans.props <- Device.transitions.data %>% freq_table(device.trans.type.simple)
device.trans.props <- rowid_to_column(device.trans.props, "row.ID")

device.trans.props <- device.trans.props %>% mutate(prop = (n / 50)*100) %>% select(cat,n,prop)

device.trans.props <- device.trans.props %>% 
  mutate(prop = case_when(cat %in% c("Abstained", "Added Device", "Subtracted Device") ~ NA,
                          TRUE ~ prop))

device.trans.props$prop <- sprintf(device.trans.props$prop, fmt = '%#.2f')


#------------------------------------------------
#EXPLORE ADDERS & SUBTRACTERS OF DEVICES (UPDATE 2023.0503: NOT using this granularity for paper)
Device.adders.subtracters <- Device.transitions.data %>% filter(device.trans.type.simple == "Added Device" | device.trans.type.simple == "Subtracted Device")
View(Device.adders.subtracters)

Device.adders.subtracters <- Device.adders.subtracters %>% 
  mutate(device.trans.multi.patrns = case_when(pid==34 & visit==10 ~ "A --> B + C",
                                               pid==34 & visit==11 ~ "B + C --> A",
                                               
                                               pid==35 & visit==4 ~ "A --> A + B",
                                               pid==35 & visit==5 ~ "A + B --> A",
                                               pid==35 & visit==11 ~ "A --> A + B",
                                               pid==35 & visit==12 ~ "A + B --> A",
                                               
                                               pid==69 & visit==4 ~ "A --> A + B",
                                               pid==69 & visit==5 ~ "A + B --> A",
                                               
                                               pid==95 & visit==2 ~ "A --> A + B",
                                               pid==95 & visit==4 ~ "A + B --> A",
                                               
                                               pid==98 & visit==4 ~ "A --> A + B",
                                               pid==98 & visit==5 ~ "A + B --> A",
                                               pid==98 & visit==6 ~ "A --> A + B",
                                               pid==98 & visit==7 ~ "A + B --> A",
                                               pid==98 & visit==11 ~ "A --> A + B",
                                               pid==98 & visit==12 ~ "A + B --> A",
                                               
                                               pid==100 & visit==4 ~ "A --> A + B",
                                               pid==100 & visit==5 ~ "A + B --> A + B + C",
                                               pid==100 & visit==6 ~ "A + B + C --> A",
                                               
                                               pid==110 & visit==5 ~ "A --> A + B",
                                               pid==110 & visit==7 ~ "A + B --> A",
                                               pid==110 & visit==8 ~ "A --> A + B",
                                               pid==110 & visit==9 ~ "A + B --> A",
                                               pid==110 & visit==12 ~ "A --> A + B",
                                               
                                               pid==114 & visit==5 ~ "A --> A + B + C",
                                               pid==114 & visit==6 ~ "A + B + C --> A",
                                               pid==114 & visit==7 ~ "A --> A + B + C",
                                               pid==114 & visit==8 ~ "A + B + C --> A + B",
                                               pid==114 & visit==9 ~ "A + B --> B + C",
                                               pid==114 & visit==10 ~ "A + B --> A",
                                               
                                               pid==115 & visit==2 ~ "A + B --> A + B + C",
                                               pid==115 & visit==3 ~ "A + B + C --> A",
                                               pid==115 & visit==8 ~ "A --> A + B",
                                               pid==115 & visit==9 ~ "A + B --> A",
                                               pid==115 & visit==12 ~ "A --> A + B",
                                               
                                               pid==125 & visit==8 ~ "A --> A + B",
                                               pid==125 & visit==9 ~ "A + B --> A"),
         
         device.trans.multisimpled = case_when(pid==34 & visit==10 ~ "A --> B + C",
                                               pid==34 & visit==11 ~ "A + B --> C",
                                               
                                               pid==35 & visit==4 ~ "Closed-System Pod --> Disposable",
                                               pid==35 & visit==5 ~ "Disposable --> Closed-System Pod",
                                               pid==35 & visit==11 ~ "Closed-System Pod --> Disposable",
                                               pid==35 & visit==12 ~ "Disposable --> Closed-System Pod",
                                               
                                               pid==69 & visit==4 ~ "Disposable --> Closed-System Pod",
                                               pid==69 & visit==5 ~ "Closed-System Pod --> Disposable",
                                               
                                               pid==95 & visit==2 ~ "Open-System Mod --> Disposable",
                                               pid==95 & visit==4 ~ "Disposable --> Open-System Mod", 
                                               
                                               pid==98 & visit==4 ~ "Open-System Mod --> Closed-System Pod",
                                               pid==98 & visit==5 ~ "Closed-System Pod --> Open-System Mod",
                                               pid==98 & visit==6 ~ "Open-System Mod --> Closed-System Pod",
                                               pid==98 & visit==7 ~ "Closed-System Pod --> Open-System Mod",
                                               pid==98 & visit==11 ~ "Open-System Mod --> Closed-System Pod",
                                               pid==98 & visit==12 ~ "Closed-System Pod --> Open-System Mod",
                                               
                                               pid==100 & visit==4 ~ "Disposable --> Open-System Mod",
                                               pid==100 & visit==5 ~ "A + B --> A + B + C",
                                               pid==100 & visit==6 ~ "A + B + C --> A",
                                               
                                               pid==110 & visit==5 ~ "Open-System Mod --> Disposable",
                                               pid==110 & visit==7 ~ "Disposable --> Open-System Mod",
                                               pid==110 & visit==8 ~ "Open-System Mod --> Disposable",
                                               pid==110 & visit==9 ~ "Disposable --> Open-System Mod",
                                               pid==110 & visit==12 ~ "Open-System Mod --> Disposable",
                                               
                                               pid==114 & visit==5 ~ "A --> A + B + C",
                                               pid==114 & visit==6 ~ "A + B + C --> A",
                                               pid==114 & visit==7 ~ "A --> A + B + C",
                                               pid==114 & visit==8 ~ "A + B + C --> A + B",
                                               pid==114 & visit==9 ~ "A + B --> B + C",
                                               pid==114 & visit==10 ~ "Open-System Mod --> Open-System Pod-Mod",
                                               
                                               pid==115 & visit==2 ~ "A + B --> A + B + C",
                                               pid==115 & visit==3 ~ "A + B + C --> A",
                                               pid==115 & visit==8 ~ "Closed-System Pod --> Disposable",
                                               pid==115 & visit==9 ~ "Disposable --> Closed-System Pod",
                                               pid==115 & visit==12 ~ "Closed-System Pod --> Disposable",
                                               
                                               pid==125 & visit==8 ~ "Closed-System Pod --> Disposable",
                                               pid==125 & visit==9 ~ "Disposable --> Closed-System Pod"),
         
         device.trans.broad = case_when(pid==34 & visit==10 ~ "A --> B + C",
                                        pid==34 & visit==11 ~ "A + B --> C",
                                        
                                        pid==35 & visit==4 ~ "Addition",
                                        pid==35 & visit==5 ~ "Subtraction",
                                        pid==35 & visit==11 ~ "Addition",
                                        pid==35 & visit==12 ~ "Subtraction",
                                        
                                        pid==69 & visit==4 ~ "Addition",
                                        pid==69 & visit==5 ~ "Subtraction",
                                        
                                        pid==95 & visit==2 ~ "Addition",
                                        pid==95 & visit==4 ~ "Subtraction",
                                        
                                        pid==98 & visit==4 ~ "Addition",
                                        pid==98 & visit==5 ~ "Subtraction",
                                        pid==98 & visit==6 ~ "Addition",
                                        pid==98 & visit==7 ~ "Subtraction",
                                        pid==98 & visit==11 ~ "Addition",
                                        pid==98 & visit==12 ~ "Subtraction",
                                        
                                        pid==100 & visit==4 ~ "Addition",
                                        pid==100 & visit==5 ~ "A + B --> A + B + C",
                                        pid==100 & visit==6 ~ "A + B + C --> A",
                                        
                                        pid==110 & visit==5 ~ "Addition",
                                        pid==110 & visit==7 ~ "Subtraction",
                                        pid==110 & visit==8 ~ "Addition",
                                        pid==110 & visit==9 ~ "Subtraction",
                                        pid==110 & visit==12 ~ "Addition",
                                        
                                        pid==114 & visit==5 ~ "A --> A + B + C",
                                        pid==114 & visit==6 ~ "A + B + C --> A",
                                        pid==114 & visit==7 ~ "A --> A + B + C",
                                        pid==114 & visit==8 ~ "A + B + C --> A + B",
                                        pid==114 & visit==9 ~ "A + B --> B + C",
                                        pid==114 & visit==10 ~ "Subtraction",
                                        
                                        pid==115 & visit==2 ~ "A + B --> A + B + C",
                                        pid==115 & visit==3 ~ "A + B + C --> A",
                                        pid==115 & visit==8 ~ "Addition",
                                        pid==115 & visit==9 ~ "Subtraction",
                                        pid==115 & visit==12 ~ "Addition",
                                        
                                        pid==125 & visit==8 ~ "Addition",
                                        pid==125 & visit==9 ~ "Subtraction"))



Device.adders.subtracters %>% distinct(device.trans.multi.patrns)


#--------------------------------------------------------------------------------------------

########################################
### DOING MORE WITH COMPLEX SWITCHES ###
########################################
Device.transitions.complex$device.trans.type <- gsub("Pod-Mod", "P.Mod", Device.transitions.complex$device.trans.type)

Device.transitions.complex <- Device.transitions.complex %>% separate(device.trans.type, c("lag.device", "current.device"), " --> ", remove=FALSE)

Device.transitions.complex <- Device.transitions.complex %>%
  mutate(partial.disp.add = case_when(!grepl("Disposable",lag.device) & grepl("Disposable",current.device) ~ 1,
                                      TRUE ~ 0),
         partial.disp.subtr = case_when(grepl("Disposable",lag.device) & !grepl("Disposable",current.device) ~ 1,
                                        TRUE ~ 0),
         
         partial.cspod.add = case_when(!grepl("Closed-System Pod",lag.device) & grepl("Closed-System Pod",current.device) ~ 1,
                                       TRUE ~ 0),
         partial.cspod.subtr = case_when(grepl("Closed-System Pod",lag.device) & !grepl("Closed-System Pod",current.device) ~ 1,
                                         TRUE ~ 0),
         
         partial.ospod.add = case_when(!grepl("Open-System Pod",lag.device) & grepl("Open-System Pod",current.device) ~ 1,
                                       TRUE ~ 0),
         partial.ospod.subtr = case_when(grepl("Open-System Pod",lag.device) & !grepl("Open-System Pod",current.device) ~ 1,
                                         TRUE ~ 0),
         
         partial.ospodmod.add = case_when(!grepl("Open-System P.Mod",lag.device) & grepl("Open-System P.Mod",current.device) ~ 1,
                                          TRUE ~ 0),
         partial.ospodmod.subtr = case_when(grepl("Open-System P.Mod",lag.device) & !grepl("Open-System P.Mod",current.device) ~ 1,
                                            TRUE ~ 0),
         
         partial.osmod.add = case_when(!grepl("Open-System Mod",lag.device) & grepl("Open-System Mod",current.device) ~ 1,
                                       TRUE ~ 0),
         partial.osmod.subtr = case_when(grepl("Open-System Mod",lag.device) & !grepl("Open-System Mod",current.device) ~ 1,
                                         TRUE ~ 0))



Device.transitions.data <- Device.transitions.data %>% 
  left_join(Device.transitions.complex, by = "device.trans.type")

Device.transitions.data <- Device.transitions.data %>% 
  select(pid,visit,device.trans.type, device.trans.type.simple,"partial.disp.add":"partial.osmod.subtr")

Device.transitions.complex.stats <- Device.transitions.data %>% 
  filter(device.trans.type.simple %in% c("Added Device", "Subtracted Device"))

#something missed above for pid 114... manual edit!
Device.transitions.complex.stats <- Device.transitions.complex.stats %>% 
  mutate(partial.disp.add = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 0, pid==34 & visit==11 ~ 1, TRUE ~ partial.disp.add),
         partial.disp.subtr = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 1, pid==34 & visit==11 ~ 0, TRUE ~ partial.disp.subtr),
         
         partial.cspod.add = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 1, pid==34 & visit==11 ~ 0, TRUE ~ partial.cspod.add),
         partial.cspod.subtr = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 0, pid==34 & visit==11 ~ 1, TRUE ~ partial.cspod.subtr),
         
         partial.ospod.add = case_when(pid==114 & visit %in% c(5,7) ~ 1, pid==114 & visit %in% c(6,8:10) ~ 0, pid==34 & visit %in% c(10:11) ~ 0, TRUE ~ partial.ospod.add),
         partial.ospod.subtr = case_when(pid==114 & visit %in% c(6,9) ~ 1, pid==114 & visit %in% c(5,7,8,10) ~ 0, pid==34 & visit %in% c(10:11) ~ 0, TRUE ~ partial.ospod.subtr),
         
         partial.ospodmod.add = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 1, pid==34 & visit==11 ~ 0, TRUE ~ partial.ospodmod.add),
         partial.ospodmod.subtr = case_when(pid==114 & visit %in% c(5:10) ~ 0, pid==34 & visit==10 ~ 0, pid==34 & visit==11 ~ 1, TRUE ~ partial.ospodmod.subtr),
         
         partial.osmod.add = case_when(pid==114 & visit %in% c(5,7,9) ~ 1, pid==114 & visit %in% c(6,8,10) ~ 0, pid==34 & visit %in% c(10:11) ~ 0, TRUE ~ partial.ospodmod.add),
         partial.osmod.subtr = case_when(pid==114 & visit %in% c(6,8,10) ~ 1, pid==114 & visit %in% c(5,7,9) ~ 0, pid==34 & visit %in% c(10:11) ~ 0, TRUE ~ partial.ospodmod.subtr),
         
  )





partial.disp.add.props <- Device.transitions.complex.stats %>% freq_table(partial.disp.add) %>% select(var,n)
partial.disp.subtr.props <- Device.transitions.complex.stats %>% freq_table(partial.disp.subtr) %>% select(var,n)
partial.cspod.add.props <- Device.transitions.complex.stats %>% freq_table(partial.cspod.add) %>% select(var,n)
partial.cspod.subtr.props <- Device.transitions.complex.stats %>% freq_table(partial.cspod.subtr) %>% select(var,n)
partial.ospod.add.props <- Device.transitions.complex.stats %>% freq_table(partial.ospod.add) %>% select(var,n)
partial.ospod.subtr.props <- Device.transitions.complex.stats %>% freq_table(partial.ospod.subtr) %>% select(var,n)
partial.ospodmod.add.props <- Device.transitions.complex.stats %>% freq_table(partial.ospodmod.add) %>% select(var,n)
partial.ospodmod.subtr.props <- Device.transitions.complex.stats %>% freq_table(partial.ospodmod.subtr) %>% select(var,n)
partial.osmod.add.props <- Device.transitions.complex.stats %>% freq_table(partial.osmod.add) %>% select(var,n)
partial.osmod.subtr.props <- Device.transitions.complex.stats %>% freq_table(partial.osmod.subtr) %>% select(var,n)



#MAnual ordering of most common partial switches
partial.switch.props.device <- 
  bind_rows(partial.disp.add.props[2,],
            partial.disp.subtr.props[2,],
            partial.cspod.add.props[2,],
            partial.cspod.subtr.props[2,],
            partial.ospod.add.props[2,],
            partial.ospod.subtr.props[2,],
            partial.ospodmod.add.props[2,],
            partial.ospodmod.subtr.props[2,],
            partial.osmod.add.props[2,],
            partial.osmod.subtr.props[2,])

partial.switch.props.device <- partial.switch.props.device %>% mutate(prop = (n/37)*100)

partial.switch.props.device$prop <- sprintf(partial.switch.props.device$prop, fmt = '%#.2f')
View(partial.switch.props.device)

#--------------------------------------------------------------------------------------------

###########################################################################
### Percent of visits where a participant was using a REFILLABLE device ###
###########################################################################

CRoFT.predom.devices.openclosed <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.devices.openclosed.xlsx")

CRoFT.predom.devices.openclosed <- CRoFT.predom.devices.openclosed %>% 
  mutate(open.device.ind = case_when(grepl("Open",predom.openclosed.rev) ~ 1, TRUE ~ 0))

CRoFT.predom.devices.openclosed %>% freq_table(open.device.ind)


###########################################################################
### Percent of visits where a participant was using a MODIFIABLE device ###
###########################################################################
CRoFT.predom.devices.modifiability <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.devices.modifiability.xlsx")

CRoFT.predom.devices.modifiability <- CRoFT.predom.devices.modifiability %>% 
  mutate(predom.modifiability.ind = case_when(predom.modifiability.rev == "Modifiable" ~ 1, 
                                              grepl("Modifiable;", predom.modifiability.rev) ~ 1,
                                              grepl("; Modifiable", predom.modifiability.rev) ~ 1,
                                              grepl("Abstain",predom.modifiability.rev) ~ NA,
                                              TRUE ~ 0))

CRoFT.predom.devices.modifiability %>% filter(!is.na(predom.modifiability.ind)) %>% freq_table(predom.modifiability.ind)


###################################################################
### Percent of visits where ONE predominant device established  ###
###################################################################

Device.2023_0503 <- Device.2023_0503 %>% 
  mutate(single.device.ind = case_when(!grepl("; ", device) ~ 1, TRUE ~ 0))

Device.2023_0503 %>% filter(device != "Abstained") %>% freq_table(single.device.ind)

##################################################################
### How many participants had NO device changes across visits? ###
##################################################################

Device.2023_0503.no_abstain <- Device.2023_0503 %>% filter(device != "Abstained") %>% select(pid,visit,device.ordered)


Device.2023_0503.no_abstain <- Device.2023_0503.no_abstain %>% 
  group_by(pid) %>% 
  mutate(change = case_when(device.ordered != lag(device.ordered) ~ TRUE, 
                            TRUE ~ FALSE),
         n_change = cumsum(change))


Device.2023_0503.no_abstain <- Device.2023_0503.no_abstain %>% 
  mutate(stable.device = case_when(visit==12 & n_change == 0 ~ 1,
                                   visit==12 & n_change > 0 ~ 0,
                                   TRUE ~ NA))

stable.device.wide <- Device.2023_0503.no_abstain %>% filter(!is.na(stable.device))
#NOTE: PID 092 had abstinent visit at V12; need to add them to denominator; they had ONE switch early, so NO not completely stable (see below dataframe for more deets)
#abstainer.device <- Device.2023_0503 %>% filter(device == "Abstained") %>% select(pid,visit,device.ordered)

stable.device.wide <- stable.device.wide %>% 
  select(pid,visit,n_change, stable.device)

stable.device.wide %>% freq_table(stable.device)


##################################################################
### Avg device changes over study (among any switchers (n=29)  ###
##################################################################

stable.device.wide.stable <- stable.device.wide %>% filter(n_change != 0)
mean(stable.device.wide.stable$n_change)
sd(stable.device.wide.stable$n_change)





######################################################################################################
######################################################################################################
######################################################################################################

####################################
### NICOTINE PREVALENCE AT VISIT ###
####################################
Nic.Conc.2023_0503 <- FDN.2023_0502 %>% mutate(lownic.ind = case_when(most.common.nc == "<24 mg/ml" ~ 1, TRUE ~ 0))
Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% mutate(highnic.ind = case_when(most.common.nc == "24+ mg/ml" ~ 1, TRUE ~ 0))

Nic.Conc.2023_0503 %>% filter(flavor != "abstain" & device != "Abstained") %>% freq_table(lownic.ind)
Nic.Conc.2023_0503 %>% filter(flavor != "abstain" & device != "Abstained") %>% freq_table(highnic.ind)



########################################
### NICOTINE LEVEL SWITCHES AT VISIT ###
########################################
Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  mutate(lag.nicconc = lag(most.common.nc), lead.nicconc = lead(most.common.nc)) %>% arrange(pid,visit)

Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  mutate(transition.ind = case_when(visit ==1 ~ NA, grepl("Abstained",most.common.nc) ~ "Abstained", 
                                    lag.nicconc == most.common.nc ~ "No",
                                    most.common.nc !="Abstained" & grepl("Abstained",lag.nicconc) ~ "Immediate post-abstain visit", 
                                    TRUE ~ "Yes"))

Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  unite("transition.type", c("lag.nicconc", "most.common.nc"), sep= " --> ", na.rm = TRUE, remove = FALSE)



#Remove unite var for stable visits
Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  mutate(transition.type = case_when(visit ==1 ~ "Baseline visit", 
                                     transition.ind == "No" | is.na(transition.ind)  ~ "Stable",
                                     transition.ind == "Abstained" ~ "Abstained", 
                                     TRUE ~ transition.type))


#confirming or reclassifying (manually) visits where participants are returning from abstained visit(s)
nicconc.abst_return <- Nic.Conc.2023_0503 %>% filter(grepl("Abstained -->",transition.type))

nicconc.abst_return.preced_rows <- Nic.Conc.2023_0503 %>% 
  filter(pid==9 | pid==34 | pid==47 | pid==48 | pid==122) %>% select(pid, visit, transition.type, most.common.nc)

nicconc.abst_return <- nicconc.abst_return %>% 
  mutate(transition.abst_return = case_when(pid==9 & visit==11 ~ "Stable",
                                            pid==34 & visit ==5 ~ "Stable",
                                            pid==47 & visit ==5 ~ "<24 mg/ml --> 24+ mg/ml",
                                            pid==48 & visit ==9 ~ "<24 mg/ml --> 24+ mg/ml",
                                            pid==122 & visit ==7 ~ "Stable")) %>% 
  select(pid,visit,transition.abst_return) %>% arrange(pid,visit)

#merge
Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% left_join(nicconc.abst_return, by = c("pid","visit"))

Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  mutate(nicconc.trans.type = case_when(!is.na(transition.abst_return) ~ transition.abst_return, TRUE ~ transition.type))

Nic.Conc.2023_0503 <- Nic.Conc.2023_0503 %>% 
  mutate(transition.ind = case_when(transition.ind == "Immediate post-abstain visit" & transition.type == "Stable"  ~ "No",
                                    transition.ind == "Immediate post-abstain visit" & transition.type != "Stable" ~ "Yes",
                                    TRUE ~ transition.ind))

NicConc.transitions.data <- Nic.Conc.2023_0503 %>% filter(nicconc.trans.type!="Baseline visit") %>% select(pid,visit,nicconc.trans.type)



#PERCENTAGE OF Non-Baseline VISITS WITH VARIOUS DEVICE TRANSITIONS
NicConc.trans.perc <- NicConc.transitions.data %>% freq_table(nicconc.trans.type)



#########################################################################
### How many participants had NO high <--> low changes across visits? ###
#########################################################################

Nic.Conc.2023_0503.no_abstain <- Nic.Conc.2023_0503 %>% 
  filter(flavor != "abstain" & device != "Abstained") %>% select(pid,visit,most.common.nc)


Nic.Conc.2023_0503.no_abstain <- Nic.Conc.2023_0503.no_abstain %>% 
  group_by(pid) %>% 
  mutate(change = case_when(most.common.nc != lag(most.common.nc) ~ TRUE, 
                            TRUE ~ FALSE),
         n_change = cumsum(change))


Nic.Conc.2023_0503.no_abstain <- Nic.Conc.2023_0503.no_abstain %>% 
  mutate(stable.nicconc = case_when(visit==12 & n_change == 0 ~ 1,
                                    visit==12 & n_change > 0 ~ 0,
                                    TRUE ~ NA))


stable.nicconc.highlow.wide <- Nic.Conc.2023_0503.no_abstain %>% filter(!is.na(stable.nicconc))
#NOTE: PID 092 had abstinent visit at V12; need to add them to denominator; they had ONE early nicotine high/low switch

stable.nicconc.highlow.wide <- stable.nicconc.highlow.wide %>% 
  select(pid,visit,n_change, stable.nicconc)

stable.nicconc.highlow.wide %>% freq_table(stable.nicconc)


##################################################################
### Avg device changes over study (among any switchers (n=29)  ###
##################################################################

stable.nicconc.highlow.wide.stable <- stable.nicconc.highlow.wide %>% filter(n_change != 0)
mean(stable.nicconc.highlow.wide.stable$n_change)
sd(stable.nicconc.highlow.wide.stable$n_change)

