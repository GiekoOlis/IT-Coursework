       # #читаем строку названий столбцов из файла
       # #создаем для каждого названия чекбокс, название - имя предиктора, id - можно цифру
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
      #observe({
      #S=reactive({input$Predictors})
      #vsp = reactiveValues()
      #vsp$k=names(data())
      #vsp$k= vsp$k[,-S()]
      #data.header = vsp$k
      #return(data.heared)
      #})
      
      #data.header =reactive({vsp$k = vsp()[,-S()]})
      #S=reactiveValues({input$Predictors})
      #vsp = reactiveValues({names(data())})
      #data.header =reactive({vsp = vsp[,-S()]})
##########################################################################################################
      data.header = reactive({names(data())})
      #outputOptions(output, "data.header", suspendWhenHidden = FALSE)
########################################################################################################## 
      

##########################################################################################################      
      #output$raw_data = renderDataTable({data()}) 
      #output$main_grid = renderUI({ 
      #if (is.null(data())){ p("Для того, чтобы воспользоваться калькулятором, добавьте файл с тбалицей")} 
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
            Pr=input$Predictors
            P=length(input$Predictors)
            removeModal()
            insertUI(
                selector = "#CreateCategory",
                where = "afterEnd",
                ui =  fluidPage(br(),
                                h4(paste(N1),":   ",paste(K1))
                )  )
            
            for(i in 1:P){
              insertUI(
                selector = "#gender",
                where = "afterEnd",
                ui =  fluidPage(
                          numericInput(inputId = Pr[i],label=Pr[i], min = 0, max = 10, value = 4.6, step = 0.5, width = 300),
                    )
                )}
            insertUI(
              selector = "#gender",
              where = "afterEnd",
              ui =   
                tabPanel(N1,h3(paste(N1)),tags$style("h1{text-align:center;}")))
        })
##########################################################################################################
        observeEvent(input$FinishRegression, { 
          removeModal()
           insertUI(
            selector = "#Kalc",
            where = "beforeEnd",
            ui =  tabPanel( where="beforeEnd",
              h4("Осложнения"),tags$style("h1{text-align:center;}"),
              absolutePanel(top=NULL,left=NULL, where="beforeBegin",
                            actionButton("CreateComplication", "Создать осложнение"),
                            actionButton("CreateComSZfdxg", "Создать осложнение")))
            )
          
        })
##########################################################################################################
      #observe({
      #  Y=matrix(nrow = 1, ncol = 10)  
      #  i=1
      #})
      #Y=reactive({
      #  matrix(nrow = 1, ncol = 10)
      #})
      observeEvent(input$CreateComplication, {
          showModal(modalDialog(
            fluidPage(   
              h4("Выберите осложнение"), tags$style("h4{text-align:center;}"),
              checkboxGroupInput("Complication","Выберите Осложнение", data.header())
            ),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Выбрать")),easyClose = TRUE))
          #Y[i]=input$Complication
          showModal(modalDialog(
            textInput("Arguments", "Предикторы:", placeholder="введите название"),
            fluidPage(   
              h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
              checkboxGroupInput("Param","Выберите предикторы", data.header())
            ),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Создать")),easyClose = TRUE))
          #i=i+1
        })
########################################################################################################## 
      createArray <- reactive({
        X<-array(0,dim=c(500,input$nsim))
        X[1,]<-rep(input$p,input$nsim)
        
        for(j in 1:N.sim){
          for(i in 2:N.gen){
            p.i = rbinom(1,N.chrom,prob=1-X[i-1,j])/N.chrom
            meanfit <-p.i^2*w.aa+2*p.i*(1-p.i)*w.ab+(1-p.i)^2*w.bb
            X[i,j] =1-(p.i^2*w.aa/meanfit+p.i*(1-p.i)*w.ab/meanfit)
          }  
        }
        X 
      })
      
      output$plot <- renderPlot({
        ourArray<-createArray() 
        plot(ourArray) #plot the results of the simulations (loops).
      })    
########################################################################################################## 
      observeEvent(input$Add1, { 
        N2=paste(input$category)
        K2=paste(input$Param,collapse = ",    ")
        removeModal()
        insertUI(
          selector = "#CreateComplication",
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