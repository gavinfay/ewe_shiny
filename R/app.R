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
groups <- c('whales', 'seals', 'cod', 'whiting', 'mackerel', 'anchovy', 'shrimp',
            'benthos', 'zooplankton', 'phytoplankton', 'detritus', 'sealers', 
            'trawlers', 'seiners', 'bait boats', 'shrimpers')

types <- c(rep(0, 9), 1, 2, rep(3, 5))

AB.params <- create.rpath.params(groups, types)

#Biomass, Production, consumption
biomass <- c(0.08, 0.0609, 3, 1.8, 1.2, 7, 0.8, NA, 14.8, 9, 10, rep(NA, 5))

pb <- c(0.05, 0.164, 0.340, 0.581, 0.723, 1.140, 3, 3, 35, 240, rep(NA, 6))

qb <- c(9, 15, 2.58, 3.3, 4.4, 9.13, rep(NA, 10))

AB.params$model[, Biomass := biomass]
AB.params$model[, PB      := pb]
AB.params$model[, QB      := qb]

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
#rsim.plot(AB.run1, groups[1:11])
# 
# #Cut seal fishing effort in half
# AB.base <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'sealers', 
#                           value = 0.5, sim.year = 4:25)
# AB.run2 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run2, groups[1:11])
# 
# #Explorer if seals had strong top down effect
# AB.base <- adjust.scenario(AB.base, parameter = 'VV', group = 'cod', 
#                            groupto = 'seals', value = 150)
# AB.run3 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run3, groups[1:11])
# 
# #Decrease trawling
# AB.base <- adjust.fishing(AB.base, parameter = 'EFFORT', group = 'trawlers',
#                           value = 0.75, sim.year = 8:25)
# AB.run4 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run4, groups[1:11])
# 
# #Have Cod exert top-down effects
# AB.base <- adjust.scenario(AB.base, parameter = 'VV', group = 'whiting', 
#                            groupto = 'cod', value = 150)
# 
# AB.run5 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run5, groups[1:11])
# 
# #Change whiting's foraging behavior
# AB.base <- adjust.scenario(AB.base, parameter = 'FtimeAdj', group = 'whiting',
#                            value = 0.8)
# AB.run6 <- rsim.run(AB.base, method = 'AB', 1:25)
# #rsim.plot(AB.run6, groups[1:11])
# 

#rsim.plot(AB.run6, spname = 'whiting', indplot = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Anchovy Bay: EcoSim"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       helpText("App by G. Fay, Rpath by Sean Lucey"),
       
         #numericInput('Nsims', 'Number of simulations', Nsims, value = 2),
         #numericInput('Nyr', 'Number of years', Nyr),
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
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$webplot <- renderPlot({
     
     #par(mfrow=c(2,2))
     webplot(AB, labels = T)
   })
   
   output$bioplot <- renderPlot({
     #Increase trawling
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
     rsim.plot(AB.run7, groups[1:11])
     #rsim.plot(AB.run7, groups[12:16])
     
     
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
     
          rsim.plot(AB.run7, groups[12:16])
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
     
#     rsim.plot(AB.run7, groups[12:16])
     
     tyield=matrix(NA,nrow=150,ncol=4)
     tyield[,1]=seq(.1,15,.1)
     for(i in 1:150){
       releff=tyield[i,1]
       AB2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'trawlers',
                             value = releff, sim.year = 8:25)
       AB.run <- rsim.run(AB2, method = 'AB', 25)
       #rsim.plot(AB.run, groups[1:11])
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
     
     #     rsim.plot(AB.run7, groups[12:16])
     
     tyield=matrix(NA,nrow=150,ncol=4)
     tyield[,1]=seq(.1,15,.1)
     syield=matrix(NA,nrow=150,ncol=9)
     syield[,1]=seq(.1,15,.1) 
     for(i in 1:150){
       releff=tyield[i,1]
       AB2 <- adjust.fishing(AB.b2, parameter = 'EFFORT', group = 'trawlers',
                             value = releff, sim.year = 8:25)
       AB.run <- rsim.run(AB2, method = 'AB', 1:25)
       #rsim.plot(AB.run, groups[1:11])
       s2=print(AB.run)
       tyield[i,2]=sum(s2[4:5,6])
       tyield[i,3]=sum(s2[6:7,6])
       tyield[i,4]=sum(s2[8,6])
       syield[i,2:9] <- s2[2:9,6]
     }
     tyield=as.data.frame(tyield)
     colnames(tyield)=c('Effort','Trawl','Seiner','Shrimper')
     s2yield=as.data.frame(syield[,c(1,3:8)])
     colnames(s2yield)=c('effort',groups[2:7])
     s2yield <- tidyr::gather(s2yield, key = "group", value = "catch", -1) #%>% 
     ggplot(s2yield) +
       geom_area(aes(x=effort, y=catch, fill = fct_relevel(group, groups[c(3:7,2)]))) +
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
