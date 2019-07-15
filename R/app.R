#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#devtools::install_github('slucey/RpathDev/Rpath', ref = 'Public')
library(data.table); library(Rpath); library(ggplot2); library(forcats); library(tidyverse)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Anchovy Bay
AB.groups <- c('whales', 'seals', 'cod', 'whiting', 'mackerel', 'anchovy', 'shrimp',
            'benthos', 'zooplankton', 'phytoplankton', 'detritus', 'sealers', 
            'trawlers', 'seiners', 'bait boats', 'shrimpers')

AB.types <- c(rep(0, 9), 1, 2, rep(3, 5))

AB.params <- create.rpath.params(AB.groups, AB.types)

#Biomass, Production, consumption
AB.biomass <- c(0.08, 0.0609, 3, 1.8, 1.2, 7, 0.8, NA, 14.8, 9, 10, rep(NA, 5))

AB.pb <- c(0.05, 0.164, 0.340, 0.581, 0.723, 1.140, 3, 3, 35, 240, rep(NA, 6))

AB.qb <- c(9, 15, 2.58, 3.3, 4.4, 9.13, rep(NA, 10))

AB.params$model[, Biomass := AB.biomass]
AB.params$model[, PB      := AB.pb]
AB.params$model[, QB      := AB.qb]

AB.params$model[Group == 'shrimp',      ProdCons := 0.25]
AB.params$model[Group == 'benthos',     ProdCons := 0.25]
AB.params$model[Group == 'zooplankton', ProdCons := 0.25]

#Add EE's for unknown biomasses
AB.params$model[Group == 'benthos', EE := 0.6]
#Biomass accumulation and unassimilated production
AB.params$model[, BioAcc  := c(rep(0, 11), rep(NA, 5))]
AB.params$model[, Unassim := c(rep(0.2, 9), rep(0, 2), rep(NA, 5))]

#Detrital fate
AB.params$model[, detritus := c(rep(1, 10), rep(0, 6))]

#Landings/Discards
AB.params$model[Group == 'seals',    sealers      := .0045]
AB.params$model[Group == 'cod',      trawlers     := 0.45]
AB.params$model[Group == 'whiting',  trawlers     := 0.2]
AB.params$model[Group == 'mackerel', seiners      := 0.4]
AB.params$model[Group == 'anchovy',  seiners      := 1.2]
AB.params$model[Group == 'anchovy',  "bait boats" := 0.2]
AB.params$model[Group == 'shrimp',   shrimpers    := 0.05]

#Diet
AB.params$diet[, whales      := c(rep(NA, 2), 0.1, 0.1, 0.2, 0.5, NA, 0.1, rep(NA, 3))]
AB.params$diet[, seals       := c(NA, NA, 0.04, 0.05, NA, NA, 0.01, 0.9, rep(NA, 3))]
AB.params$diet[, cod         := c(NA, NA, NA, 0.05, NA, 0.1, 0.01, 0.84, rep(NA, 3))]
AB.params$diet[, whiting     := c(NA, NA, 0.05, 0.05, NA, 0.45, 0.01, 0.44, rep(NA, 3))]
AB.params$diet[, mackerel    := c(rep(NA, 4), 0.05, 0.5, NA, NA, 0.45, NA, NA)]
AB.params$diet[, anchovy     := c(rep(NA, 8), 1, NA, NA)]
AB.params$diet[, shrimp      := c(rep(NA, 7), 1, rep(NA, 3))]
AB.params$diet[, benthos     := c(rep(NA, 7), 0.1, 0.1, 0.1, 0.7)]
AB.params$diet[, zooplankton := c(rep(NA, 9), 0.9, 0.1)]

check.rpath.params(AB.params)

#save(AB.params, file = file.path(data.dir, "Ancovy_Bay_params.RData"))

#Ecopath
AB <- rpath(AB.params, 'Anchovy Bay')

#webplot(AB, labels = T)

#Ecosim
#Add BA to whales and seals
AB.params$model[Group == 'seals', BioAcc := -0.0005]
AB <- rpath(AB.params, 'Anchovy Bay, v2')

AB.base <- rsim.scenario(AB, AB.params, 1:25)
AB.run1 <- rsim.run(AB.base, method = 'AB', 1:25)
#rsim.plot(AB.run1, AB.groups[1:11])
# 
# #Cut seal fishing effort in half
# AB.base <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'sealers', 
#                           value = 0.5, sim.year = 4:25)
# AB.run2 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run2, AB.groups[1:11])
# 
# #Explorer if seals had strong top down effect
# AB.base <- adjust.scenario(AB.base, parameter = 'VV', group = 'cod', 
#                            groupto = 'seals', value = 150)
# AB.run3 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run3, AB.groups[1:11])
# 
# #Decrease trawling
# AB.base <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
#                           value = 0.75, sim.year = 8:25)
# AB.run4 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run4, AB.groups[1:11])
# 
# #Have Cod exert top-down effects
# AB.base <- adjust.scenario(AB.base, parameter = 'VV', group = 'whiting', 
#                            groupto = 'cod', value = 150)
# 
# AB.run5 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run5, AB.groups[1:11])
# 
# #Change whiting's foraging behavior
# AB.base <- adjust.scenario(AB.base, parameter = 'FtimeAdj', group = 'whiting',
#                            value = 0.8)
# AB.run6 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run6, AB.groups[1:11])
# 

#rsim.plot(AB.run6, spname = 'whiting', indplot = T)

# Gulf of Maine
gom.groups <- c("Phytoplankton- Primary Producers", "Bacteria", "Microzooplankton",
            "Small copepods", "Large Copepods", "Gelatinous Zooplankton", "Micronekton",
            "Macrobenthos- polychaetes", "Macrobenthos- crustaceans", "Macrobenthos- molluscs",
            "Macrobenthos- other", "Megabenthos- filterers", "Megabenthos- other",
            "Shrimp et al.", "Larval-juv fish- all", "Small Pelagics- commercial",
            "Small Pelagics- other", "Small Pelagics- squid", "Small Pelagics- anadromous",
            "Medium Pelagics- (piscivores & other)", "Demersals- benthivores",
            "Demersals- omnivores", "Demersals- piscivores", "Sharks- pelagics",
            "HMS", "Pinnipeds", "Baleen Whales", "Odontocetes", "Sea Birds",
            "Discard", "Detritus-POC", "Fishery")
gom.type <- c(1, rep(0, 28), rep(2, 2), 3)

gom.par <- create.rpath.params(gom.groups, gom.type)

gom.par$model[, Biomass := c(22.126, 5.484, 4.885, 10.403, 11.955, 1.283, 4.874,
                             18.942, 4.04, 9.866, 24.936, 2.879, 3.505, 0.396, 0.207,
                             5.714, 1.275, 0.29, 0.153, 0.0229, 2.981, 0.4, 4.006,
                             0.00296, 0.00587, 0.063, 0.602, 0.0336, 0.0035, NA,
                             NA, NA)]

gom.par$model[, PB := c(163.143, 91.25, 72, 30.918, 35, 35, 14.25, 2.55, 3.3, 2.24,
                        2.04, 0.864, 1.68, 2, 15, 0.52, 0.44, 1.4, 0.437, 0.649,
                        0.459, 0.54, 0.55, 0.15, 0.5, 0.0673, 0.042, 0.04, 0.275,
                        rep(NA, 3))]#55.84456, 29.90675, NA)]

gom.par$model[, QB := c(0, 380.208, 242.424, 127.75, 109.5, 146, 36.5, 17.5, 21,
                        13.72, 11.777, 10, 11.03, 5, 45, 1.882, 2, 2, 2, 1.428,
                        0.9, 0.9, 1.014, 0.623, 2.362, 4.85, 2.3, 8.5, 5.362,
                        rep(NA, 3))]

#float path_EE[NUM_GROUPS+1] = { 1, 0.8798624, 0.8832949, 0.9413487, 0.9117724, 0.7440153, 0.9114986, 0.8848614, 0.8999999, 0.8819566, 0.8633214, 0.8862541, 0.8823805, 0.8879386, 0.8046172, 0.8990406, 0.8963319, 0.9790097, 0.9941966, 0.5794412, 0.8889518, 0.9287031, 0.910743,  0.878319, 0.8430014, 0.8114244, 0.2865187, 0.01683634, 0.1337793, 0.1249854, 0.005459445, 0.9885055, 0};
gom.par$model[, BioAcc := c(rep(0, 31), NA)]

gom.par$model[, Unassim := c(0, 0.2, 0.1, 0.25, 0.25, 0.35, 0.25, 0.5, 0.5, 0.6,
                             0.5, 0.7, 0.3, 0.3, 0.15, 0.15, 0.35, 0.15, 0.15,
                             0.15, 0.3, 0.35, 0.15, 0.15, 0.15, 0.2, 0.2, 0.2,
                             0.15, 0, 0, NA)]

#float path_DtImp[NUM_GROUPS+1] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

gom.par$model[, Discard := c(rep(0, 31), 1)]
gom.par$model[, 'Detritus-POC' := c(rep(1, 29), rep(0, 3))]

gom.par$model[, Fishery := c(rep(0, 11), 0.0917, 0.345, 0.0622, 0, 0.874, 0.0000151,
                             0.0138, 0.00561, 0.0101, 0.142, 0.00718, 0.301, 0.00024,
                             0.00182, rep(0, 6), NA)]

gom.par$model[, Fishery.disc := c(rep(0, 5), 6.36E-07, 0, 0.000135, 0.0000183, 0.000189,
                                  0.000724, 0.0302, 0.104, 0.0195, 2.65E-09, 0.131,
                                  0.000156, 0.000562, 0.0167, 0.00303, 0.043, 0.00228,
                                  0.0904, 0.0000721, 0.000545, 0.00115, 0.000405,
                                  0.000139, 0.0000588, rep(0, 2), NA)]

DC.groups <- gom.groups[which(!gom.groups %in% c('Discard', 'Detritus-POC', 'Fishery'))]

DC.c.code <-as.data.table(matrix(c(
  0,0,0.4305193,0.2117425,0.7240587,0.6317893,0.08602893,0.1824,0.1241,0.1351713,0.4000163,0.2008283,0.7909901,0,0.07548419,0.0589119,0.01121766,0.1656693,0,0.0117771,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0.1971663,0,0,0.02000673,0,0.2954761,0.1374624,0.1951299,0.2008283,0.1188099,0.1418395,0.4473137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0.09702543,0.1133734,0.04287973,0.05001682,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0.032,0.1097328,0.3301111,0.2208001,0,0.005453273,0,0,0,0,0,0.4305102,0.1193151,0.08079781,0,0.05613042,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0.1276366,0.3601212,0.3664,0,0.02749248,0,0,0,0,0,0.2492427,0.4344292,0.676395,0.1006037,0.9020878,0,0,0,0,0.03340645,0,0,0.153928,0,0.03427451,0,0,0,
  0,0,0,0,0,0.02392604,0.04867304,0,0,0,0,0,0,0,0,0,0.01923198,0.02177701,0,0,0.01960942,0.008961361,0.08314439,0.04308448,0,0.09437622,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0.0464,0.05909523,0.05383944,0.03414774,0.06733654,0,0,0.1509684,0.06797528,0.2172146,0.04431918,0.5030184,0.02034226,0,0.06147739,0.0328972,0.02203885,0,0,0.00313569,0.5024855,0.000274618,0.1405256,0,0,0,
  0,0,0,0,0,0,0,0,0.02672455,0.1351713,0,0.06561421,0,0.07919377,0,0.02096442,0.01121766,0,0,0.001553198,0,0.1471426,0.1663658,0.01101942,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0.000327961,0,0,0.004904852,0.03310977,0,0.009938519,0,0.066255,0.008152554,0.01274636,0.0545647,0.000721625,0.1509055,0.002170789,0.01226363,0.1844322,0.1663658,0.01101942,0.01113548,0,0.005412052,0,0.002591398,0,0,0,0,
  0,0,0,0,0,0,0,0,0.000639306,0.02196813,0.004286418,0.02239663,0,0.2375813,0,0.005,0.00803651,0.00072132,0,0,0,0.1229548,0.1663658,0.03305828,0,0,0.005412052,0,0.002591398,0,0,0,0,
  0,0,0,0,0,0.001748159,0,0,0.02106318,0.1346288,0.022337,0.0363167,0,0.2375813,0.1048391,0.02096442,0.01121766,0.002212085,0.01676727,0,0.01226363,0.1844322,0.1109106,0.1091925,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0.001053159,0.004799868,0.002984548,0.001145771,0,0.009741648,0,0,0,0,0,0,0,0.05240696,0.04041656,0.01101942,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0.001404212,0.004421631,0.000735662,0.006267365,0,0.04154278,0,0,0,0,0,0,0.1941741,0.09574349,0.1109106,0.02203885,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.001258382,0,0,0,0.02361985,0,0.01720621,0.01696089,0.003024275,0.1207828,0,0,0.00313569,0,0.000274618,0.02157306,0,0,0,
  0,0,0,0,0,0,0.004059562,0,0,0,0,0,0,0,0,0.0747728,0.1080974,0.00738653,0.1676727,0.005938409,0.01226363,0,0.01127904,0.01101942,0,0,0,0,0,0.10,0,0,0,
  0,0,0,0,0,0,0.000358531,0,0,0,0,0,0,0,0,0,0,0,0.0075355,0,0.3638209,0.04829028,0.04365378,0.2704768,0.2338451,0.1801901,0.1359761,0.1550972,0.2288119,0.3118983,0,0,0,
  0,0,0,0,0,0,0.000454902,0,0,0,0,0,0,0,0,0,0,0,0.007557068,0,0.03679087,0.002015652,0.001879841,0.0560989,0.05567741,0.7006319,0.256553,0.08101361,0.06583849,0.2684838,0,0,0,
  0,0,0,0,0,0,9.18E-05,0,0,0,0,0,0,0,0,0,0,0,0.02131235,0,0.02591145,0.003626702,0.001691515,0.04413503,0.1781677,0.02480174,0.06566829,0.03142953,0.3652223,0.03182262,0,0,0,
  0,0,0,0,0,0,4.37E-05,0,0,0,0,0,0,0,0,0,0,0,0.001007609,0,0.01925174,0,0,0.001604244,0.02227096,0,0,0,0,0.02017987,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.000721882,0,0,0,0.031213,0,0,0,0,0.000353094,0,0,0,
  0,0,0,0,0,0,0,0,0,0.001607866,0,0.00181822,0,0.002898938,0,0,0.001819155,0,0,0,0.1216143,0.03728956,0.01221896,0.01101942,0.05567741,0,0.251599,0.01532696,0.1114401,0.02,0,0,0,
  0,0,0,0,0,0,0,0,0,0.000314047,0,0.000115925,0,0.000863633,0,0,0.001819155,0,0,0,0.04249413,0.006046957,0.0093992,0.004007064,0.08908386,0,0.02449874,0.0110239,0.04565654,0.01322618,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0.001225253,0,0.001656536,0,0,0.001819155,0,0,0,0.1216143,0.004031305,0.005639521,0.218385,0.07794838,0,0.2486094,0.04969525,0.1772986,0.02856211,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.02227096,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0091007,0,0,0,
  0,0,0.5694807,0.4940658,0.130568,0.06195949,0.1000336,0.184,0.465539226,0.304559621,0.340362397,0.386167839,0.0902,0.18084539,0.21198369,0.0589119,0,0,0,0,0,0.02418782,0.03383712,0,0.05567741,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 32, 33, byrow = T))

DC.c.code[, V1  := NULL]
DC.c.code[, V31 := NULL]
DC.c.code[, V32 := NULL]
DC.c.code[, V33 := NULL]

gom.par$diet <- cbind(gom.par$diet[, Group], DC.c.code)

setnames(gom.par$diet, c(paste('V', 1:30, sep = '')), c('Group', DC.groups))

check.rpath.params(gom.par)

#Run model
GOM <- rpath(gom.par, 'Gulf of Maine')
#Check sim
gom.base <- rsim.scenario(GOM, gom.par, 1:100)
gom.run <- rsim.run(gom.scence)
#rsim.plot(gom.run, gom.groups)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Food Web Model Scenario Viewer"),
   
   # Select model to view
   selectInput("model", "Model", c("Anchovy Bay", "Gulf of Maine")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       helpText("App by G. Fay and S. Gaichas, Rpath by Sean Lucey"),
       
       #numericInput('Nsims', 'Number of simulations', Nsims, value = 2),
       #numericInput('Nyr', 'Number of years', Nyr),
       conditionalPanel(condition = "input.model == 'Anchovy Bay'",
                        sliderInput("hrateSeal",
                                    "Relative sealer effort:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("hrateT",
                                    "Relative trawler effort:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("hrateS",
                                    "Relative seiner effort:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("hrateSh",
                                    "Relative shrimper effort:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("codVV",
                                    "Cod Top-down predation:",
                                    min = 0,
                                    max = 500,
                                    value = 2),
                        sliderInput("whitingAdj",
                                    "Whiting foraging behavior:",
                                    min = 0,
                                    max = 3,
                                    value = 0) #,
                        # sliderInput("hrateP",
                        #             "Pelagics harvest rate:",
                        #             min = 0,
                        #             max = 1,
                        #             value = 0.2),
                        # sliderInput("hrateE",
                        #             "Elasmobranchs harvest rate:",
                        #             min = 0,
                        #             max = 1,
                        #             value = 0.05)         
       ),
       
       conditionalPanel(condition = "input.model == 'Gulf of Maine'",
                        sliderInput("hrateFishery",
                                    "Relative combined fishing effort:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("herringZ",
                                    "Mortality, Commercial small pelagics:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2),
                        sliderInput("seabirdZ",
                                    "Mortality, Seabirds:",
                                    min = 0,
                                    max = 5,
                                    value = 1,
                                    step = 0.2)
                        
       )
     ), #end sidebar panel
     
      # Show a plot of the generated distribution
      mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Food Web", plotOutput("webplot")),
                    tabPanel("Biomass", plotOutput("bioplot")),
                    tabPanel("Catch", 
                             
                             #fluidRow( verticalLayout(plotOutput("catplot1"), plotOutput("catplot2")))),
                             
                             plotOutput("catplot3")),#,
                    # tabPanel("Summary", verbatimTextOutput("summary")),
                    tabPanel("Summary Table", tableOutput("table"))
        )
        #plotOutput("EwEplot")
      ) #end main panel
   ) #end sidebar layout
) #end ui

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   modelInput <- reactive({
    switch(input$model,
           "Anchovy Bay" = AB,
           "Gulf of Maine" = GOM)
   })
   
   output$webplot <- renderPlot({
     
     #par(mfrow=c(2,2))
     webplot(modelInput(), labels = T)
   })
   
   output$bioplot <- renderPlot({
     #Increase trawling
     if(input$model == "Anchovy Bay"){
       AB.b2 <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
                               value = input$hrateT, sim.year = 8:25)
       AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'sealers',
                               value = input$hrateSeal, sim.year = 4:25)
       AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'seiners',
                               value = input$hrateS, sim.year = 8:25)
       AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'shrimpers',
                               value = input$hrateSh, sim.year = 8:25)     
       
       #Have Cod exert top-down effects
       AB.b2 <- adjust.scenario(AB.b2, parameter = 'VV', group = 'whiting', 
                                groupto = 'cod', value = input$codVV)
       
       #Change whiting's foraging behavior
       AB.b2 <- adjust.scenario(AB.b2, parameter = 'FtimeAdj', group = 'whiting',
                                value = input$whitingAdj)
       
       AB.run7 <- rsim.run(AB.b2, method = 'AB', 1:25)
       rsim.plot(AB.run7, AB.groups[1:11])
       #rsim.plot(AB.run7, AB.groups[12:16])
     }
     if(input$model == "Gulf of Maine"){
       #Change fishing effort
       GOM.b2 <- adjust.fishing(gom.base, parameter = 'EFFORT', group = 'Fishery',
                                value = input$hrateFishery, sim.year = 25:100)
       
       #Change herring mort (commercial small pelagics in GOM mostly herring)
       GOM.b2 <- adjust.forcing(GOM.b2, parameter = 'bymort', group = 'Small Pelagics- commercial',
                                value = input$herringZ, sim.year = 25:100)
       
       #Change seabird mort
       GOM.b2 <- adjust.forcing(GOM.b2, parameter = 'bymort', group = 'Sea Birds',
                                value = input$seabirdZ, sim.year = 25:100)
       
       
       GOM.run1 <- rsim.run(GOM.b2)
       rsim.plot(GOM.run1, gom.groups[1:29])
     }
     
# #      res <- do_msprod(hrateG = input$hrateG, Nsims = input$Nsims, Nyr = input$Nyr)
#       res <- do_msprod(hrateG = input$hrateG,
#                        hrateP = input$hrateP, 
#                        hrateE = input$hrateE, 
#                        Nsims = input$Nsims, Nyr = 30)
      
      # draw the plots
      #viz_msprod(res, Nsims = input$Nsims, Nyr = input$Nyr)
      # viz_msprod(res, Nsims = input$Nsims, Nyr = 30)
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   output$catplot1 <- renderPlot({
     AB.b2 <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
                             value = input$hrateT, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'sealers',
                             value = input$hrateSeal, sim.year = 4:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'seiners',
                             value = input$hrateS, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'shrimpers',
                             value = input$hrateSh, sim.year = 8:25)     
     
     #Have Cod exert top-down effects
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'VV', group = 'whiting', 
                              groupto = 'cod', value = input$codVV)
     
     #Change whiting's foraging behavior
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'FtimeAdj', group = 'whiting',
                              value = input$whitingAdj)
     
     AB.run7 <- rsim.run(AB.b2, method = 'AB', 1:25)
     
     #     layout(matrix(1,2,nrow=2,ncol=1))
     
          rsim.plot(AB.run7, AB.groups[12:16])
   })
      
   output$catplot2 <- renderPlot({
     AB.b2 <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
                             value = input$hrateT, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'sealers',
                             value = input$hrateSeal, sim.year = 4:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'seiners',
                             value = input$hrateS, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'shrimpers',
                             value = input$hrateSh, sim.year = 8:25)     
     
     #Have Cod exert top-down effects
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'VV', group = 'whiting', 
                              groupto = 'cod', value = input$codVV)
     
     #Change whiting's foraging behavior
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'FtimeAdj', group = 'whiting',
                                value = input$whitingAdj)
     
     AB.run7 <- rsim.run(AB.b2, method = 'AB', 1:25)
     
#     layout(matrix(1,2,nrow=2,ncol=1))
     
#     rsim.plot(AB.run7, AB.groups[12:16])
     
     tyield=matrix(NA,nrow=150,ncol=4)
     tyield[,1]=seq(.1,15,.1)
     for(i in 1:150){
       releff=tyield[i,1]
       AB2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'trawlers',
                             value = releff, sim.year = 8:25)
       AB.run <- rsim.run(AB2, method = 'AB', 25)
       #rsim.plot(AB.run, AB.groups[1:11])
       s2=print(AB.run)
       tyield[i,2]=sum(s2[4:5,6])
       tyield[i,3]=sum(s2[6:7,6])
       tyield[i,4]=sum(s2[8,6])
     }
     tyield=as.data.frame(tyield)
     colnames(tyield)=c('Effort','Trawl','Seiner','Shrimper')
     #summary(tyield)
     #identify which 
     effmaxyield <- tyield[which.max(tyield[,2]),1]
     #plot(tyield[,2]~tyield[,1],type='l',xlab='Trawler effort',ylab='Catch in last year',main='Trawlers')
     #abline(v=effmaxyield,lty=2)
     plot(tyield$Seiner~tyield[,1],type='l',xlab='Trawler effort',ylab='Catch in last year',
          col='blue',ylim=c(0,3.5),lwd=2)
     abline(v=effmaxyield,lty=2)
     lines(tyield$Trawl~tyield$Effort,lwd=2)
     lines(tyield$Shrimper~tyield$Effort,col='magenta',lwd=2)
     legend('right',legend=c('Trawlers','Seiners','Shrimpers'),lty=1,col=c('black','blue','magenta'),
            title='Fleet')
   })

   
   output$catplot3 <- renderPlot({
     AB.b2 <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
                             value = input$hrateT, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'sealers',
                             value = input$hrateSeal, sim.year = 4:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'seiners',
                             value = input$hrateS, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'shrimpers',
                             value = input$hrateSh, sim.year = 8:25)     
     
     #Have Cod exert top-down effects
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'VV', group = 'whiting', 
                              groupto = 'cod', value = input$codVV)
     
     #Change whiting's foraging behavior
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'FtimeAdj', group = 'whiting',
                              value = input$whitingAdj)
     
     AB.run7 <- rsim.run(AB.b2, method = 'AB', 1:25)
     
     #     layout(matrix(1,2,nrow=2,ncol=1))
     
     #     rsim.plot(AB.run7, AB.groups[12:16])
     
     tyield=matrix(NA,nrow=150,ncol=4)
     tyield[,1]=seq(.1,15,.1)
     syield=matrix(NA,nrow=150,ncol=9)
     syield[,1]=seq(.1,15,.1) 
     for(i in 1:150){
       releff=tyield[i,1]
       AB2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'trawlers',
                             value = releff, sim.year = 8:25)
       AB.run <- rsim.run(AB2, method = 'AB', 1:25)
       #rsim.plot(AB.run, AB.groups[1:11])
       s2=print(AB.run)
       tyield[i,2]=sum(s2[4:5,6])
       tyield[i,3]=sum(s2[6:7,6])
       tyield[i,4]=sum(s2[8,6])
       syield[i,2:9] <- s2[2:9,6]
     }
     tyield=as.data.frame(tyield)
     colnames(tyield)=c('Effort','Trawl','Seiner','Shrimper')
     s2yield=as.data.frame(syield[,c(1,3:8)])
     colnames(s2yield)=c('effort',AB.groups[2:7])
     s2yield <- tidyr::gather(s2yield, key = "group", value = "catch", -1) #%>% 
     ggplot(s2yield) +
       geom_area(aes(x=effort, y=catch, fill = fct_relevel(group, AB.groups[c(3:7,2)]))) +
       scale_fill_brewer(type = "qual") +
       xlab("Trawler effort") +
       ylab("Catch in last year") +
       guides(fill=guide_legend(title="group")) +
       title("Catch")
     #summary(tyield)
     #identify which 
     # effmaxyield <- tyield[which.max(tyield[,2]),1]
     # #plot(tyield[,2]~tyield[,1],type='l',xlab='Trawler effort',ylab='Catch in last year',main='Trawlers')
     # #abline(v=effmaxyield,lty=2)
     # plot(tyield$Seiner~tyield[,1],type='l',xlab='Trawler effort',ylab='Catch in last year',
     #      col='blue',ylim=c(0,3.5),lwd=2)
     # abline(v=effmaxyield,lty=2)
     # lines(tyield$Trawl~tyield$Effort,lwd=2)
     # lines(tyield$Shrimper~tyield$Effort,col='magenta',lwd=2)
     # legend('right',legend=c('Trawlers','Seiners','Shrimpers'),lty=1,col=c('black','blue','magenta'),
     #        title='Fleet')
   })
   
   output$table <- renderTable({
     AB.b2 <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
                             value = input$hrateT, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'sealers',
                             value = input$hrateSeal, sim.year = 4:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'seiners',
                             value = input$hrateS, sim.year = 8:25)
     AB.b2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'shrimpers',
                             value = input$hrateSh, sim.year = 8:25)     
     
     #Have Cod exert top-down effects
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'VV', group = 'whiting', 
                              groupto = 'cod', value = input$codVV)
     
     #Change whiting's foraging behavior
     AB.b2 <- adjust.scenario(AB.b2, parameter = 'FtimeAdj', group = 'whiting',
                              value = input$whitingAdj)
     
     AB.run7 <- rsim.run(AB.b2, method = 'AB', 1:25)
     
     print(AB.run7)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


## multiple scenarios
## compare different scenarios for indivudal functional group
##  or two or three
## profile over other fisheries
