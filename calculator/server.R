library(shiny)
library(shinyjs)
library(DT)
library(data.table)
    function(input, output, session){
##########################################################################################################      
      options (shiny.maxRequestSize = 30*1024^5) 
      data = reactive ({ 
        File = input$file 
        if(is.null(File)){return()} 
        else {datat=read.csv2(na.strings = c("NA",""," "),File$datapath)
        #datap=as.data.frame(datat)
        return(datat)
      }})
      data.header = reactive({names(data())})  
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
                                h4(paste(N1),":   ",paste(K1))))
            for(i in 1:P){
                insertUI(
                  selector = "#gender",
                  where = "afterEnd",
                  ui =  fluidPage(
                    numericInput(inputId = Pr[i],label=Pr[i], min = 0, max = 10, value = 4.6, step = 0.5, width = 300)))}
            insertUI(
              selector = "#gender",
              where = "afterEnd",
              ui =   
                tabPanel(N1,h3(paste(N1)),tags$style("h1{text-align:center;}")))
        })
##########################################################################################################
      observeEvent(input$CreateComplication, {
          showModal(modalDialog(
            fluidPage(   
              h4("Выберите осложнение"), tags$style("h4{text-align:center;}"),
              radioButtons("Complication","Выберите Осложнение", data.header())
            ),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Выбрать")),easyClose = TRUE))
        })
########################################################################################################## 
      observeEvent(input$Add1, { 
        removeModal()
        showModal(modalDialog(
          fluidPage(   
            h4("Выберите параметры"), tags$style("h4{text-align:center;}"),
            checkboxGroupInput("Param","Выберите предикторы", data.header())
          ),
          footer= tagList(
            modalButton ("Отмена"),
            actionButton("Add2", "Создать")),easyClose = TRUE))
      })
########################################################################################################## 
        TL=reactive({input$Complication})
        KL=reactive({input$Param})
        RR=reactive({l=paste(KL(),collapse ="+")
          return(l)})
        f=reactive({K=as.formula(paste(TL(), "~",RR()))
          return(K)})
        m=reactive({t=glm(f(),data=data(),family = binomial)
        p=as.matrix(t$coefficients)
        return(p)})
        mm=reactive({t=glm(f(),data=data(),family = binomial)
        return(t)})
        data2=reactive({dataq=data()
        dataq=dataq[FALSE, ]
        KT=input$Param
        P=length(KT)
        tyu=dataq
        tyu[1,]=0
        for(i in 1:P){
        tyu$KT[i]=input$KT[i]
        }
        p=predict(mm(),type="response", tyu[1, ])
        return(p)
        })
##########################################################################################################
      observeEvent(input$Add2, { 
        N2=paste(input$Complication)
        K2=paste(input$Param,collapse = ",    ")
        PT=Reduce( paste, deparse(m()) )
        removeModal()
        TR=m()
        KT=input$Param
        P=length(KT)
        Formula=paste(input$Complication,"~",TR[1],"+")
        for(i in 1:P){
          if(i==P){
            Formula=paste(Formula,TR[i+1],"*",KT[i]) }
          else{
            Formula=paste(Formula,TR[i+1],"*",KT[i],"+")}
        }
        insertUI(
          selector = "#CreateComplication",
          where = "afterEnd",
          ui =  fluidPage(br(),
                          h4(paste(Formula)),br(),paste(data2())))
        })
##########################################################################################################
    }