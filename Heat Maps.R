

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

CRoFT.predom.flavors <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.flavors.xlsx")

###########################
###     FLAVOR TYPE     ###
###########################

CRoFT.predom.flavors.cat2 <- CRoFT.predom.flavors %>% separate(flavor, c("first.flavor", "second.flavor","third.flavor","fourth.flavor","fifth.flavor"), "; ", remove = FALSE)

CRoFT.predom.flavors.cat2 <- CRoFT.predom.flavors.cat2 %>% 
  mutate(first.flavor = case_when(grepl("fruit", first.flavor) | grepl("sweets", first.flavor) | grepl("other", first.flavor) ~ "sweets/fruit", TRUE ~ first.flavor),
         second.flavor = case_when(grepl("fruit", second.flavor) | grepl("sweets", second.flavor) | grepl("other", second.flavor) ~ "sweets/fruit", TRUE ~ second.flavor),
         third.flavor = case_when(grepl("fruit", third.flavor) | grepl("sweets", third.flavor) | grepl("other", third.flavor) ~ "sweets/fruit", TRUE ~ third.flavor),
         fourth.flavor = case_when(grepl("fruit", fourth.flavor) | grepl("sweets", fourth.flavor) | grepl("other", fourth.flavor) ~ "sweets/fruit",TRUE ~ fourth.flavor))


CRoFT.predom.flavors.cat2 <- CRoFT.predom.flavors.cat2 %>% 
  unite("predom.flavor.cat2", c("first.flavor", "second.flavor", "third.flavor", "fourth.flavor"), sep= "; ", na.rm = TRUE, remove = FALSE)

#alphabetical ordering to classify permutations as combinations in unite var
CRoFT.predom.flavors.cat2$predom.flavor.cat2_ordered <- CRoFT.predom.flavors.cat2$predom.flavor.cat2 %>% str_split(., '; ') %>% 
  lapply(., 'sort') %>%  lapply(., 'paste', collapse='; ') %>% unlist(.)

CRoFT.predom.flavors.cat2$predom.flavor.cat2FINAL <- gsub('sweets/fruit; sweets/fruit', 'sweets/fruit', CRoFT.predom.flavors.cat2$predom.flavor.cat2_ordered)


#################################################################

#Pivot from long to wide
heatmap.flavor.cat2.widedata <- CRoFT.predom.flavors.cat2 %>% select(pid,visit,predom.flavor.cat2FINAL) %>% pivot_wider(names_from = visit, values_from = predom.flavor.cat2FINAL) %>% arrange(pid)

#ggplot heatmaps are counting the pid numerics that are not completers; this fixes that by giving new UID for completer set (1:80)
heatmap.flavor.cat2.widedata <- rowid_to_column(heatmap.flavor.cat2.widedata, "completer.UID")



#ORDER determined from Complex HeatMap package
flav.hm.order <- c(45,12,22,2,14,21,41,58,73,32,34,76,79,7,11,33,37,1,39,40,48,80,64,63,56,55,17,10,3,9,
                   72,49,78,16,47,19,35,8,57,54,53,4,38,24,18,44,28,77,71,66,43,42,26,27,20,36,46,5,68,65,
                   52,51,30,6,15,31,50,61,13,69,29,59,23,60,70,74,25,67,62,75)

flav.hm.order.df <- as.data.frame(flav.hm.order)
flav.hm.order.df <- rowid_to_column(flav.hm.order.df, "seq")

#MAJOR RETROSPECTIVE STEP FOR MANUAL REOGRANIZATION OF HEATMAP
flav.hm.order.df <- flav.hm.order.df %>% 
  mutate(hm.remap.desc = case_when(seq == 1 ~ "Mix all",
                                   seq == 2 ~ "almost all sweet",
                                   seq == 3 ~ "half menthol, all others",
                                   seq == 4 ~ "almost all sweet",
                                   seq == 5 ~ "almost all sweet",
                                   seq == 6 ~ "Mostly sweet, some hybrid",
                                   seq == 7 ~ "almost all sweet",
                                   seq == 8 ~ "almost all sweet",
                                   seq == 9 ~ "almost all tobacco",
                                   seq == 10 ~ "almost all sweet",
                                   seq == 11 ~ "all sweet",
                                   seq == 12 ~ "half sweet, mix menthol-hybrid",
                                   seq == 13 ~ "almost all tobacco",
                                   seq == 14 ~ "almost all sweet",
                                   seq == 15 ~ "all sweet",
                                   seq == 16 ~ "half hybrid, mix menthol-sweet",
                                   seq == 17 ~ "almost all hybrid",
                                   seq == 18 ~ "almost all sweet",
                                   seq == 19 ~ "all sweet",
                                   seq == 20 ~ "75% menthol, 25% hybrid",
                                   seq == 21 ~ "almost all tobacco",
                                   seq %in% 22:30 ~ "all tobacco",
                                   seq == 31 ~ "hybrid & sweet",
                                   seq %in% 32:33 ~ "all hybrid",
                                   seq == 34 ~ "75% sweet, 25% hybrid",
                                   seq == 35 ~ "50-50 hybrid sweet",
                                   seq == 36 ~ "75% hybrid, 25% sweet",
                                   seq == 37 ~ "66% sweet, 33% hybrid",
                                   seq %in% 38:43 ~ "all menthol",
                                   seq == 44 ~ "almost all sweet",
                                   seq %in% 45:46 ~ "all sweet",
                                   seq == 47 ~ "almost all sweet",
                                   seq %in% 48:54 ~ "all sweet",
                                   seq == 55 ~ "almost all sweet",
                                   seq %in% 56:67 ~ "all sweet",
                                   seq == 68 ~ "almost all sweet",
                                   seq == 69 ~ "66% sweet, mix menthol-hybrid",
                                   seq == 70 ~ "all sweet",
                                   seq == 71 ~ "66% sweet, hybrid & abstain",
                                   seq == 72 ~ "all sweet",
                                   seq == 73 ~ "almost all sweet",
                                   seq == 74 ~ "sweet, hybrid, tobacco",
                                   seq == 75 ~ "50% sweet, 50% hybrid",
                                   seq == 76 ~ "75% sweet, 25% hybrid",
                                   seq == 77 ~ "75% sweet, 25% menthol",
                                   seq == 78 ~ "almost all sweet",
                                   seq %in% 79:80 ~ "hybrid-sweet"))

#flav.hm.order.df %>% distinct(hm.remap.desc)


flav.hm.order.df <- flav.hm.order.df %>% 
  mutate(hm.remap.groups = case_when(hm.remap.desc == "all sweet" ~ 10,
                                     hm.remap.desc == "almost all sweet" ~ 9,
                                     hm.remap.desc == "all hybrid" ~ 7,
                                     hm.remap.desc == "all menthol" ~ 7,
                                     hm.remap.desc == "almost all tobacco" ~ 6,
                                     hm.remap.desc == "all tobacco" ~ 5,
                                     TRUE ~ 8)) %>% 
  arrange(hm.remap.groups,seq)

flav.hm.order.df <- rowid_to_column(flav.hm.order.df, "seq.new")

flav.hm.order.df <- flav.hm.order.df %>% rename(completer.UID = flav.hm.order)

heatmap.flavor.cat2.widedata <- heatmap.flavor.cat2.widedata %>% left_join(flav.hm.order.df, by = "completer.UID") %>% arrange(seq.new)



#################################################################
#rename wide formatted column/variable names
heatmap.flavor.cat2.widedata <- heatmap.flavor.cat2.widedata %>% 
  rename("Visit 001" = "1", "Visit 002" = "2", "Visit 003" = "3", "Visit 004" = "4",
         "Visit 005" = "5", "Visit 006" = "6", "Visit 007" = "7", "Visit 008" = "8",
         "Visit 009" = "9", "Visit 010" = "10", "Visit 011" = "11", "Visit 012" = "12") %>% 
  select("seq.new","Visit 001","Visit 002","Visit 003","Visit 004",
         "Visit 005","Visit 006","Visit 007","Visit 008",
         "Visit 009","Visit 010","Visit 011","Visit 012")

hm.prac.cat2 <- heatmap.flavor.cat2.widedata %>% 
  pivot_longer(-seq.new) %>% 
  mutate(val = 1/(str_count(value, "; ")+1),
         var = str_split(value, "; ")) %>% 
  unnest(var) %>% 
  group_by(seq.new, name, var) %>% 
  summarise(val = sum(val))

##################################################################################################

visit_names <- list(
  'Visit 001'='Visit 1', 'Visit 002'="V1-2", 'Visit 003'="V2-3", 'Visit 004'="V3-4",
  'Visit 005'="V4-5", 'Visit 006'="V5-6", 'Visit 007'="V6-7", 'Visit 008'="V7-8",
  'Visit 009'="V8-9", 'Visit 010'="V9-10", 'Visit 011'="V10-11", 'Visit 012'="V11-12")

visit_labeller <- function(variable,value){
  return(visit_names[value])
}

flavor.heatmap.prep <- hm.prac.cat2 %>% ggplot(.) +
  geom_bar(aes(seq.new, val, fill = var), stat = "identity", color = "black", width=1) +
  scale_fill_manual("", labels = c("Sweet", "Hybrid", "Menthol/Mint", "Tobacco", "Abstained"),
                    values = c("sweets/fruit" = "firebrick3", "hybrid" = "purple3", "menthol" = "dodgerblue2", 
                               "tobacco" = "saddlebrown","abstain" = "grey20")) +
  facet_grid(rows = ~name, switch = "x", labeller=visit_labeller) + coord_flip()


#PULL IN FONT OPTIONS
p_load(showtext)
font_add_google("EB Garamond")
showtext_auto()

flavor.heatmap <- flavor.heatmap.prep + 
  theme(rect = element_blank(),
        plot.title = element_text(size = 12, 
                                  family="EB Garamond"),
        axis.title.x = element_text(size = 14,
                                    family="EB Garamond"),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        line = element_blank(), 
        panel.spacing = unit(-0.5, "lines"),
        legend.position = "bottom") + 
  labs(title = "Figure 2. Heatmap of ENDS flavor use in the CRoFT Study", y = "Follow-up Period") +
  scale_x_discrete() #not sure why but this removes weird space between x axis and axis facet labels


ggsave("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 projects/Addict Behavior Manuscript/flavor_heatmap.pdf", width = 6, height = 9, units = "in")



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################


#################################################################
#################################################################
CRoFT.predom.device <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/CRoFT.predom.device.xlsx")


##################
###   DEVICE   ###
###  HeatMap   ###
##################

heatmap.device.widedata <- CRoFT.predom.device %>% select(pid,visit,device) %>% pivot_wider(names_from = visit, values_from = device) %>% arrange(pid)

#rename wide formatted column/variable names
heatmap.device.widedata <- heatmap.device.widedata %>% 
  rename("Visit_001" = "1") %>% 
  rename("Visit_002" = "2") %>% 
  rename("Visit_003" = "3") %>% 
  rename("Visit_004" = "4") %>% 
  rename("Visit_005" = "5") %>% 
  rename("Visit_006" = "6") %>% 
  rename("Visit_007" = "7") %>% 
  rename("Visit_008" = "8") %>% 
  rename("Visit_009" = "9") %>% 
  rename("Visit_010" = "10") %>% 
  rename("Visit_011" = "11") %>% 
  rename("Visit_012" = "12") %>% 
  select("pid","Visit_001","Visit_002","Visit_003","Visit_004",
         "Visit_005","Visit_006","Visit_007","Visit_008",
         "Visit_009","Visit_010","Visit_011","Visit_012") %>% 
  arrange(pid)

#ggplot heatmaps are counting the pid numerics that are not completers; this fixes that by giving new UID for completer set (1:80)
heatmap.device.widedata <- tibble::rowid_to_column(heatmap.device.widedata, "completer.UID")

#ORDER determined from Complex HeatMap package
device.hm.order <- c(13,61,60,41,35,71,69,66,59,55,50,49,47,42,40,37,34,31,30,27,26,24,
                     20,19,17,15,5,6,25,51,28,70,2,18,77,1,36,39,14,68,64,57,56,53,52,48,
                     46,44,12,43,7,16,72,23,32,33,73,22,29,45,62,21,58,78,76,75,74,67,54,65,8,79,11,80,63,38,10,9,3,4)

device.hm.order.df <- as.data.frame(device.hm.order)
device.hm.order.df <- tibble::rowid_to_column(device.hm.order.df, "seq")
device.hm.order.df <- device.hm.order.df %>% rename(completer.UID = device.hm.order)

heatmap.device.widedata <- heatmap.device.widedata %>% left_join(device.hm.order.df, by = "completer.UID") %>% arrange(seq)

#################################################################

#rename wide formatted column/variable names
heatmap.device.widedata <- heatmap.device.widedata %>% 
  select("seq","Visit_001","Visit_002","Visit_003","Visit_004",
         "Visit_005","Visit_006","Visit_007","Visit_008",
         "Visit_009","Visit_010","Visit_011","Visit_012")



hm.device.prac <- heatmap.device.widedata %>% 
  pivot_longer(-seq) %>% 
  mutate(val = 1/(str_count(value, "; ")+1),
         var = str_split(value, "; ")) %>% 
  unnest(var) %>% 
  group_by(seq, name, var) %>% 
  summarise(val = sum(val))


visit_names <- list(
  'Visit 001'='Visit 1', 'Visit 002'="V1-2", 'Visit 003'="V2-3", 'Visit 004'="V3-4",
  'Visit 005'="V4-5", 'Visit 006'="V5-6", 'Visit 007'="V6-7", 'Visit 008'="V7-8",
  'Visit 009'="V8-9", 'Visit 010'="V9-10", 'Visit 011'="V10-11", 'Visit 012'="V11-12")

visit_labeller <- function(variable,value){
  return(visit_names[value])
}

device.heatmap.prep <- hm.device.prac %>% 
  ggplot(.) +
  geom_bar(aes(seq, val, fill = var), stat = "identity", color = "black", width=1) +
  scale_fill_manual("", values = c("Disposable" = "pink", "Closed-System Pod" = "firebrick3", "Open-System Pod" = "skyblue1", 
                                   "Open-System Pod-Mod" = "dodgerblue1", "Open-System Mod" = "blue2", 
                                   "Other" = "saddlebrown", "Abstained" = "grey15")) +
  facet_grid(rows = ~name, switch = "x", labeller=visit_labeller) +
  coord_flip() 


device.heatmap <- device.heatmap.prep + 
  theme(rect = element_blank(),
        plot.title = element_text(size = 12, 
                                  family="EB Garamond"),
        axis.title.x = element_text(size = 14,
                                    family="EB Garamond"),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        line = element_blank(), 
        panel.spacing = unit(-0.5, "lines"),
        legend.position = "bottom") + 
  labs(title = "Figure 3. Heatmap of ENDS device use in the CRoFT Study", y = "Follow-up Period") +
  scale_x_discrete() #not sure why but this removes weird space between x axis and axis facet labels


ggsave("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 projects/Addict Behavior Manuscript/device_heatmap.pdf", width = 6, height = 9, units = "in")



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

#########################
###     NICOTINE      ###
###   CONCENTRATION   ###
#########################
NicConc.2023_0501 <- read_excel("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 Calendars/NicConc.2023_0501.xlsx")


heatmap.nicconc.widedata <- NicConc.2023_0501 %>% select(pid,visit,most.common.nc) %>% pivot_wider(names_from = visit, values_from = most.common.nc) %>% arrange(pid)

#rename wide formatted column/variable names
heatmap.nicconc.widedata <- heatmap.nicconc.widedata %>% 
  rename("Visit_001" = "1") %>% 
  rename("Visit_002" = "2") %>% 
  rename("Visit_003" = "3") %>% 
  rename("Visit_004" = "4") %>% 
  rename("Visit_005" = "5") %>% 
  rename("Visit_006" = "6") %>% 
  rename("Visit_007" = "7") %>% 
  rename("Visit_008" = "8") %>% 
  rename("Visit_009" = "9") %>% 
  rename("Visit_010" = "10") %>% 
  rename("Visit_011" = "11") %>% 
  rename("Visit_012" = "12") %>% 
  select("pid","Visit_001","Visit_002","Visit_003","Visit_004",
         "Visit_005","Visit_006","Visit_007","Visit_008",
         "Visit_009","Visit_010","Visit_011","Visit_012")%>% 
  arrange(pid)

#ggplot heatmaps are counting the pid numerics that are not completers; this fixes that by giving new UID for completer set (1:80)
heatmap.nicconc.widedata <- tibble::rowid_to_column(heatmap.nicconc.widedata, "completer.UID")


#ORDER determined from Complex HeatMap package
nicconc.hm.order <- c(29,8,58,77,21,28,70,78,69,30,72,71,66,64,61,60,59,57,56,55,50,49,47,44,43,
                      42,40,39,38,37,35,34,31,27,26,24,20,19,17,15,5,6,65,41,63,52,67,32,79,48,
                      25,73,9,13,10,2,4,80,76,75,74,68,62,54,53,51,46,45,36,33,23,22,18,16,14,12,7,11,1,3)


nicconc.hm.order.df <- as.data.frame(nicconc.hm.order)
nicconc.hm.order.df <- tibble::rowid_to_column(nicconc.hm.order.df, "seq")
nicconc.hm.order.df <- nicconc.hm.order.df %>% rename(completer.UID = nicconc.hm.order)

heatmap.nicconc.widedata <- heatmap.nicconc.widedata %>% 
  left_join(nicconc.hm.order.df, by = "completer.UID")

heatmap.nicconc.widedata <- heatmap.nicconc.widedata %>% 
  arrange(seq)


#################################################################

#rename wide formatted column/variable names
heatmap.nicconc.widedata <- heatmap.nicconc.widedata %>% 
  select("seq","Visit_001","Visit_002","Visit_003","Visit_004",
         "Visit_005","Visit_006","Visit_007","Visit_008",
         "Visit_009","Visit_010","Visit_011","Visit_012") %>% 
  arrange(seq)



hm.nicconc.prac <- heatmap.nicconc.widedata %>% 
  pivot_longer(-seq) %>% 
  mutate(val = 1/(str_count(value, "; ")+1),
         var = str_split(value, "; ")) %>% 
  unnest(var) %>% 
  group_by(seq, name, var) %>% 
  summarise(val = sum(val))

visit_names <- list(
  'Visit 001'='Visit 1', 'Visit 002'="V1-2", 'Visit 003'="V2-3", 'Visit 004'="V3-4",
  'Visit 005'="V4-5", 'Visit 006'="V5-6", 'Visit 007'="V6-7", 'Visit 008'="V7-8",
  'Visit 009'="V8-9", 'Visit 010'="V9-10", 'Visit 011'="V10-11", 'Visit 012'="V11-12")

visit_labeller <- function(variable,value){
  return(visit_names[value])
}

nicconc.heatmap.prep <- hm.nicconc.prac %>% 
  ggplot(.) +
  geom_bar(aes(seq, val, fill = var), stat = "identity", color = "black", width=1) +
  scale_fill_manual("", values = c("24+ mg/ml" = "green4","<24 mg/ml" = "lightgreen", "Abstained" = "grey15")) +
  facet_grid(rows = ~name, switch = "x", labeller=visit_labeller) +
  coord_flip()

nicconc.heatmap <- nicconc.heatmap.prep +
  theme(rect = element_blank(),
        plot.title = element_text(size = 12, 
                                  family="EB Garamond"),
        axis.title.x = element_text(size = 14,
                                    family="EB Garamond"),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        line = element_blank(), 
        panel.spacing = unit(-0.5, "lines"),
        legend.position = "bottom") + 
  labs(title = "Figure 4. Heatmap of nicotine concentrations used in the CRoFT Study", y = "Follow-up Period") +
  scale_x_discrete() #not sure why but this removes weird space between x axis and axis facet labels


ggsave("/Users/Connor/Library/CloudStorage/OneDrive-Personal/Desktop/CRoFT 3.1 projects/Addict Behavior Manuscript/nic.conc_heatmap.highlow.pdf", width = 6, height = 9, units = "in")


















