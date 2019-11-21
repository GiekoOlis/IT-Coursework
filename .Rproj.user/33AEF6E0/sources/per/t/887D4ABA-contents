#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
#library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session){
        
        #data = read.table(file = input$file, header = true, sep = ";",dec = ",",nrows=100)
        
        ACTR=0.5
        #С ЭИТ
        efp <- reactive((exp(-9.74+1.82*input$LP+0.41*input$HSNFK-0.01*input$IMMLZH-0.24*input$SRB+0.35*input$GL+0.003*input$GAL+0.06*input$FV-0.02*input$SAD+2.48*input$GZ+1.72*input$TZ))/(1+exp(-9.74+1.82*input$LP+0.41*input$HSNFK-0.01*input$IMMLZH-0.24*input$SRB+0.35*input$GL+0.003*input$GAL+0.06*input$FV-0.02*input$SAD+2.48*input$GZ+1.72*input$TZ))*100)
        #epg - вероятность повторной госпитализации
        epg <- reactive((exp(-3.76+0.96*ACTR+0.38*input$HSNS+0.01*input$SAD+0.16*input$SRB))/(1+exp(-3.76+0.96*ACTR+0.38*input$HSNS+0.01*input$SAD+0.16*input$SRB))*100) 
        #keya - вероятность кардиоэмболии 
        keya <- reactive((exp(3.09-0.44*input$OK-2.13*input$HOBL-0.03*input$DAD+0.56*ACTR))/(1+exp(3.09-0.44*input$OK-2.13*input$HOBL-0.03*input$DAD+0.56*ACTR))*100) 
        #ehsn - вероятность ХСН в течение года
        #Без ЭИТ 
        #efp - вероятность рецидива ФП 
        #efp - вероятность повторной госпитализации
        #efp - вероятность кардиоэмболии 
        #efp - вероятность ХСН в течение года  
        #efp - вероятность рецидива ФП 
        ehsn <- reactive((exp(-13.04+1.35*input$LP-1.18*ACTR-0.004*input$MK-1.15*input$KDR+0.07*input$CHSS))/(1+exp(-13.04+1.35*input$LP-1.18*ACTR-0.004*input$MK-1.15*input$KDR+0.07*input$CHSS))*100)
        
        bfp <- reactive((exp(26.22+3.24*input$HSNFK-2.01*input$KDR-0.14*input$FV-1.25*input$GL+0.01*input$GAL-0.89*input$SRB))/(exp(26.22+3.24*input$HSNFK-2.01*input$KDR-0.14*input$FV-1.25*input$GL+0.01*input$GAL-0.89*input$SRB)+1)*100)
        
        bpg <- reactive((exp(-4.62+2.66*input$HSNFK+0.05*input$GAL-2.7*input$GZ-2.15*input$GL+0.16*input$FV-0.1*input$fsa+2.25*input$LP))/(exp(-4.62+2.66*input$HSNFK+0.05*input$GAL-2.7*input$GZ-2.15*input$GL+0.16*input$FV-0.1*input$fsa+2.25*input$LP)+1)*100)
        
        bke <- reactive((exp(9.06-0.15*input$CHSS-1.31*input$GL+3.12*input$LPV+0.03*input$SKF))/(1+exp(9.06-0.15*input$CHSS-1.31*input$GL+3.12*input$LPV+0.03*input$SKF))*100)
        
        bhsn <- reactive((exp(35.56-0.15*input$SAD-0.17*input$CHSS+0.03*input$NUP))/(exp(35.56-0.15*input$SAD-0.17*input$CHSS+0.03*input$NUP)+1)*100)
        
        
        observeEvent(input$add, {
            insertUI(
                selector = "#add",
                where = "afterEnd",
                ui = textInput(paste0("txt", input$add),
                               "Insert some text")
            )
        })
        
    }
)       