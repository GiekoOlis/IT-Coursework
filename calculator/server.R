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
        #efp - вероятность рецидива ФП 
        efp <- reactive((exp(-9.74+1.82*input$LP+0.41*input$HSNFK-0.01*input$IMMLZH-0.24*input$SRB+0.35*input$GL+0.003*input$GAL+0.06*input$FV-0.02*input$SAD+2.48*input$GZ+1.72*input$TZ))/(1+exp(-9.74+1.82*input$LP+0.41*input$HSNFK-0.01*input$IMMLZH-0.24*input$SRB+0.35*input$GL+0.003*input$GAL+0.06*input$FV-0.02*input$SAD+2.48*input$GZ+1.72*input$TZ))*100)
        #epg - вероятность повторной госпитализации
        epg <- reactive((exp(-3.76+0.96*ACTR+0.38*input$HSNS+0.01*input$SAD+0.16*input$SRB))/(1+exp(-3.76+0.96*ACTR+0.38*input$HSNS+0.01*input$SAD+0.16*input$SRB))*100) 
        #keya - вероятность кардиоэмболии 
        keya <- reactive((exp(3.09-0.44*input$OK-2.13*input$HOBL-0.03*input$DAD+0.56*ACTR))/(1+exp(3.09-0.44*input$OK-2.13*input$HOBL-0.03*input$DAD+0.56*ACTR))*100) 
        #ehsn - вероятность ХСН в течение года
        ehsn <- reactive((exp(-13.04+1.35*input$LP-1.18*ACTR-0.004*input$MK-1.15*input$KDR+0.07*input$CHSS))/(1+exp(-13.04+1.35*input$LP-1.18*ACTR-0.004*input$MK-1.15*input$KDR+0.07*input$CHSS))*100)
        #Без ЭИТ  
        #bfp - вероятность рецидива ФП
        bfp <- reactive((exp(26.22+3.24*input$HSNFK-2.01*input$KDR-0.14*input$FV-1.25*input$GL+0.01*input$GAL-0.89*input$SRB))/(exp(26.22+3.24*input$HSNFK-2.01*input$KDR-0.14*input$FV-1.25*input$GL+0.01*input$GAL-0.89*input$SRB)+1)*100)
        #bpg - вероятность повторной госпитализации 
        bpg <- reactive((exp(-4.62+2.66*input$HSNFK+0.05*input$GAL-2.7*input$GZ-2.15*input$GL+0.16*input$FV-0.1*input$fsa+2.25*input$LP))/(exp(-4.62+2.66*input$HSNFK+0.05*input$GAL-2.7*input$GZ-2.15*input$GL+0.16*input$FV-0.1*input$fsa+2.25*input$LP)+1)*100)
        #bke - вероятность кардиоэмболии
        bke <- reactive((exp(9.06-0.15*input$CHSS-1.31*input$GL+3.12*input$LPV+0.03*input$SKF))/(1+exp(9.06-0.15*input$CHSS-1.31*input$GL+3.12*input$LPV+0.03*input$SKF))*100)
        #bnsn - вероятность ХСН в течение года 
        bhsn <- reactive((exp(35.56-0.15*input$SAD-0.17*input$CHSS+0.03*input$NUP))/(exp(35.56-0.15*input$SAD-0.17*input$CHSS+0.03*input$NUP)+1)*100)
        
        l1 <- reactive(sprintf('<font color="%s">%s</font>','green', ' < 1 %'))
        h99 <- reactive(sprintf('<font color="%s">%s</font>', 'green', ' > 99 %'))
        l1r <- reactive(sprintf('<font color="%s"> %s</font>','red', ' < 1 %'))
        h99r <- reactive(sprintf('<font color="%s">%s</font>','red', ' > 99 %'))
        
        efpf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(efp(), 2), '%'))
        epgf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(epg(), 2), '%'))
        keyaf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(keya(), 2), '%'))
        ehsnf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(ehsn(), 2), '%'))
        bfpf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(bfp(), 2), '%'))
        bpgpf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(bpg(), 2), '%'))
        bkef <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(bke(), 2), '%'))
        bhsnf <- reactive(sprintf('<font color="%s">%s  %s</font>','green', round(bhsn(), 2), '%'))
        efpfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(efp(), 2), '%'))
        epgfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(epg(), 2), '%'))
        keyafr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(keya(), 2), '%'))
        ehsnfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(ehsn(), 2), '%'))
        bfpfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(bfp(), 2), '%'))
        bpgpfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(bpg(), 2), '%'))
        bkefr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(bke(), 2), '%'))
        bhsnfr <- reactive(sprintf('<font color="%s">%s  %s</font>','red', round(bhsn(), 2), '%'))
        
        
        
        observeEvent(input$add, {
            insertUI(
                selector = "#add",
                where = "afterEnd",
                ui = textInput(paste0("txt", input$add),
                               "Insert some text")
            )
        })
        
        
        
        
        output$EFP <- renderText({
            if(efp() < bfp()){
                if (efp()<1) { paste(l1())}  
                else if(efp()>99){paste(h99())} 
                else { paste(efpf())}
            } else {
                if (efp()<1) {paste(l1r()) }  
                else if(efp()>99){paste(h99r())} 
                else { paste(efpfr()) }
            }
        })
        output$EPZ <- renderText({
            if(epg() < bpg()){
                if (epg()<1) { paste(l1())}  
                else if(epg()>99){paste(h99())} 
                else {paste(epgf())}
            } else {
                if (epg()<1) {paste(l1r())}  
                else if(epg()>99){paste(h99r())} 
                else {paste(epgfr())}
            }
        })
        
        output$EKEYA <- renderText({if(keya() < bke()){
                if (keya()<1) {paste(l1())}  
            else if(keya()>99){paste(h99())} 
            else {paste(keyaf())}
            } else {
                if (keya()<1) {paste(l1r())}  
                else if(keya()>99){paste(h99r())} 
                else {paste(keyafr())}
            }   
        })
        
        output$EHSN <- renderText({
            if(ehsn() < bhsn()){
                if (ehsn()<1) {paste(l1())}  
                else if(ehsn()>99){paste(h99())} 
                else {paste(ehsnf())}
            } else {
                if (ehsn()<1) {paste(l1r())}  
                else if(ehsn()>99){paste(h99r()) }
                else {paste(ehsnfr())}
            }
            
        })
        
        
        
        
        
    }
)       