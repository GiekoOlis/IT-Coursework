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
##########################################################################################################
library(shiny)
library(shinyjs)
library(DT)
library(data.table)
#library(tidyverse)

shinyServer(
    function(input, output, session){
##########################################################################################################      
      options (shiny.maxRequestSize = 30*1024^5) 
      data = reactive ({ 
        File = input$file 
        if(is.null(File)){return()} 
        else {datat=read.csv2(na.strings = c("NA",""," "),File$datapath)
        datap=as.data.frame(datat)
        return(datap)
      }})
##########################################################################################################
      S=reactive({input$Predictors})
      vsp = reactive({names(data())})
      #data.header =reactive({vsp()[-S()]})
##########################################################################################################
      data.header = reactive({names(data())})
########################################################################################################## 
      
      #outputOptions(output, "data.header", suspendWhenHidden = FALSE)

##########################################################################################################      
      output$raw_data = renderDataTable({data()}) 
      output$main_grid = renderUI({ 
        if (is.null(data())){ p("Для того, чтобы воспользоваться калькулятором, добавьте файл с тбалицей")} 
        else {  tabsetPanel(tabPanel("Data",dataTableOutput('raw_data')))}})  
##########################################################################################################  
        observeEvent(input$CreateCategory, {
                showModal(modalDialog(
                    textInput("category", "Категория:", placeholder="введите название"),
                    fluidPage(
                        checkboxGroupInput("Predictors","Выберите предикторы",
                                           data.header())
                      ),
                       footer= tagList(
                       modalButton ("Отмена"),
                       actionButton("Add", "Создать")),easyClose = TRUE))
        })
##########################################################################################################
        observeEvent(input$Add, { 
            data.header =reactive({names(data())})
            N1=paste(input$category)
            K1=paste(input$Predictors,collapse = ",    ")
            TT=paste(data.header(),collapse = ",    ")
            PP=paste(vsp(),collapse = ",    ")
            KK=paste(S(),collapse = ",    ")
            removeModal()
            insertUI(
                selector = "#CreateCategory",
                where = "afterEnd",
                ui =  fluidPage(br(),
                                h4(paste(N1),":   ",paste(K1),":   ",br(),paste(TT),":   ",br(),paste(PP),":   ",br(),paste(KK))
                )  )
        })
##########################################################################################################
        observeEvent(input$FinishRegression, { 
          removeModal()
          #getelementbyid().removeChild(getelementbyid())
          HTML(sprintf(
            "<script>
            var node = document.getElementById('CreateCategory');
            var element = document.node.parentNode;
            while (element.firstChild) {
            element.removeChild(element.firstChild)
            </script>"))
          
          
          #removeUI(
          #  selector = "#CreateCategory"
          #)
           insertUI(
            selector = "#FinishRegression",
            where = "beforeBegin",
            ui =  actionButton("CreateComplication", "Создать осложнение")
            )
          #removeUI(
          #  selector = "#FinishRegression"
          #)
         
        })
##########################################################################################################
        observeEvent(input$CreateComplication, {
          showModal(modalDialog(
            textInput("complication", "Осложнение:", placeholder="введите название"),
            fluidPage(   
              h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
              checkboxGroupInput("Predictors","Выберите предикторы",
                                 data.header())
            )),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Создать"),easyClose = TRUE))
        })
##########################################################################################################   
      observeEvent(input$Add1, { 
        data.header =reactive({names(data())})
        N2=paste(input$category)
        K2=paste(input$Predictors,collapse = ",    ")
        removeModal()
        insertUI(
          selector = "#CreateCategory",
          where = "afterEnd",
          ui =  fluidPage(br(),
                          h4(paste(N2),":   ",paste(K2))
          )  )
      })
##########################################################################################################        
        #EXAMPLE окрашивания результата
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
    })    