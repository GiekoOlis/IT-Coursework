
library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(ggplot2)
#library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session){
      
      options (shiny.maxRequestSize = 30*1024^5) 
      data = reactive ({ 
        File = input$file 
        if(is.null(File)){return()} 
        else {datat=read.csv2(na.strings = c("NA",""," "),File$datapath)
        
        datap=as.data.frame(datat)
        return(datap)
       # output$x=row.names.data.frame({datap()})
        #data=as.data.table(datap)
      } 
      } 
      ) 
      data.header =reactive({names(data())})
      #outputOptions(output, "data.header", suspendInterrUpts())
      output$raw_data = renderDataTable({data()}) 
      #outputOptions(output, "data.header", suspendWhenHidden = FALSE)
      output$main_grid = renderUI({ 
        if (is.null(data())) 
        { 
          p("Для того, чтобы воспользоваться калькулятором, добавьте файл с тбалицей") 
        } 
        else {
          tabsetPanel(tabPanel("Data",dataTableOutput('raw_data')))
        }} 
      )  
      
        
##########################################################################################################        
        #читаем строку названий столбцов из файла
        #создаем для каждого названия чекбокс, название - имя предиктора, id - можно цифру
        #создаем массив списков, хранящих название категории и предикторы к ней относящиеся, по обращениям к элементам
        #будем выводить эти категории в интерфейс через массив(но при выводе
        #будем выводить не id чекбокса, а его метку label)
        
        #при создании категории создаем list в который помещаем название категории, вектор выбранных чекбоксов
        #создаем массив списков который будет хранить осложнения и формулы и все такое прочее
      
        #при создании осложнения создаем list  в который помещаем название осложнения, вектор значений(выбранных чекбоксов)
        #которые будут учитывтаься при подсчете этого осложнения
        #далее Влада пополняет этот список формулой и ее переменными
        #
        #
        #
        
        
        
        
        
        
        
##########################################################################################################
        observeEvent(input$CreateCategory, {
                showModal(modalDialog(
                    textInput("category", "Категория:", placeholder="введите название"),
                    fluidPage(   
                        #h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
                        #checkboxInput("", label = "ЛП", value = FALSE),
                        checkboxGroupInput("Predictors","Выберите предикторы",
                                        names(data()))
                      ),
                       footer= tagList(
                       modalButton ("Отмена"),
                       actionButton("Add", "Создать")),easyClose = TRUE))
        })
##########################################################################################################
        observeEvent(input$Add, { 
            N1=paste(input$category)
            K1=paste(input$Predictors,collapse = ",    ")
           # els = document.getElementsByName("Kalc")
            #names = names(data)
            removeModal()
            insertUI(
                selector = "#CreateCategory",
                where = "afterEnd",
                ui =  fluidPage(br(),
                                h4(paste(N1),":   ",paste(K1)), br()
                )  )
        })
##########################################################################################################
        observeEvent(input$FinishRegression, { 
          removeModal()
          #getelementbyid().removeChild(getelementbyid())
          
          
          removeUI(
            selector = "#CreateCategory"
          )
           insertUI(
            selector = "#FinishRegression",
            where = "beforeBegin",
            ui =  actionButton("CreateComplication", "Создать осложнение")
            )
          removeUI(
            selector = "#FinishRegression"
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