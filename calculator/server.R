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
        
        
##########################################################################################################
        observeEvent(input$CreateCategory, {
                showModal(modalDialog(
                    textInput("category", "Категория:", placeholder="введите название"),
                    fluidPage(   
                        #h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
                        checkboxGroupInput("Predictors","Выберите предикторы",
                                           c("ОХ"="1",
                                             "ЛПНП"="2",
                                             "ТГ"="3",
                                             "ЛПВП"="4",
                                             "Глюкоза"="5",
                                             "Калий"="6",
                                             "Мочевина"="7",
                                             "Креатинин"="8",
                                             "СКФ"="9",
                                             "СРБ"="10",
                                             "Фибриноген"="11",
                                             "МК"="12",
                                             "NTproBNP"="13",
                                             "Галлектин-3"="14",
                                             "ХСНС"="15",
                                             "ХСНФК"="16",
                                             "Терапия"="17",
                                             "ИММЛЖ"="18",
                                             "САД"="19",
                                             "ДАД"="20",
                                             "ЧСС"="21",
                                             "ЛП"="22",
                                             "КДР"="23",
                                             "ФВ"="24",
                                             "Длительность АГ"="25",
                                             "Рецидив ФП"="26")
                                           )),
                       footer= tagList(
                       modalButton ("Отмена"),
                       actionButton("Add", "Создать")),easyClose = TRUE))
        })
##########################################################################################################
        observeEvent(input$Add, { 
            N1=paste(input$category)
            K1=paste(input$Predictors,collapse = ",")  
            removeModal()
            insertUI(
                selector = "#CreateCategory",
                where = "afterEnd",
                ui =  fluidPage(br(),
                                h4(paste(N1),": ",paste(K1))
                )  )
        })
##########################################################################################################
        observeEvent(input$FinishRegression, { 
          removeModal()
          removeUI(
            selector = "#CreateCategory"
          )
           insertUI(
            selector = "#FinishRegression",
            where = "beforeBegin",
            ui =  actionButton("CreateComplication", "Создать осложнение")
            )
          removeUI(
            selector = "#FinishRegression",
            h4
          )
         
        })
##########################################################################################################
        observeEvent(input$CreateComplication, {
          showModal(modalDialog(
            textInput("complication", "Осложнение:", placeholder="введите название"),
            fluidPage(   
              h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
              checkboxGroupInput("Predictors","Выберите предикторы",
                                 c("ОХ"="1",
                                   "ЛПНП"="2",
                                   "ТГ"="3",
                                   "ЛПВП"="4",
                                   "Глюкоза"="5",
                                   "Калий"="6",
                                   "Мочевина"="7",
                                   "Креатинин"="8",
                                   "СКФ"="9",
                                   "СРБ"="10",
                                   "Фибриноген"="11",
                                   "МК"="12",
                                   "NTproBNP"="13",
                                   "Галлектин-3"="14",
                                   "ХСНС"="15",
                                   "ХСНФК"="16",
                                   "Терапия"="17",
                                   "ИММЛЖ"="18",
                                   "САД"="19",
                                   "ДАД"="20",
                                   "ЧСС"="21",
                                   "ЛП"="22",
                                   "КДР"="23",
                                   "ФВ"="24",
                                   "Длительность АГ"="25",
                                   "Рецидив ФП"="26")
              )),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Создать")),easyClose = TRUE))
        })
        
##########################################################################################################        
        #EXAMPLE
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
      
        
        
        
        
        
        
        
    }
)       