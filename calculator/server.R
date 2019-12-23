       # #читаем строку названий столбцов из файла
       # #создаем для каждого названия чекбокс, название - имя предиктора, id - можно цифру
        #создаем массив списков, хранящих название категории и предикторы к ней относящиеся, по обращениям к элементам
        #будем выводить эти категории в интерфейс через массив(но при выводе
        #будем выводить не id чекбокса, а его метку label)
        
        #при создании осложнения создаем list  в который помещаем название осложнения, вектор значений(выбранных чекбоксов)
        #которые будут учитывтаься при подсчете этого осложнения
        #далее Влада пополняет этот список формулой и ее переменными
        #
##########################################################################################################
library(shiny)
library(shinyjs)
library(DT)
library(data.table)

Compls = list()
Compls[[1]]=vector()    #Столбец осложнения
Compls[[2]]=vector()    #Вектор предикторов
Compls[[3]]=vector()    #Рассчитанная формула с коэффициентами

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
      #if (is.null(data())){ p("Для того, чтобы воспользоваться калькулятором, добавьте файл с таблицей")} 
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
        #observeEvent(input$FinishRegression, { 
        #  removeModal()
        #   insertUI(
        #    selector = "#Kalc",
        #    where = "beforeEnd",
        #    ui =  tabPanel( where="beforeEnd",
        #      h4("Осложнения"),tags$style("h1{text-align:center;}"),
        #      absolutePanel(top=NULL,left=NULL, where="beforeBegin",
        #                    actionButton("CreateComplication", "Создать осложнение"),
        #                    actionButton("CreateComSZfdxg", "Создать осложнение"))) )})
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
              radioButtons("Complication","Выберите Осложнение", data.header())
            ),
            footer= tagList(
              modalButton ("Отмена"),
              actionButton("Add1", "Выбрать")),easyClose = TRUE))
          #Y[i]=input$Complication
          
          #i=i+1
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
        #form=reactive({
        #  fr=as.formula(IOP())
        #  result=(exp(fr))/(1+exp(fr))*100
        #  return(result)
        #  })
        
       # PFP <- function(KL{
          #KT=input$Param
        #  P=length(KL)
        #  for(i in 1:P){
        #    J=KL[i+1]#*m$coefficient[i]*
        #    H=H+J
         # }
         # return(H+KL[1])
        #}
      #qwer=reactive({
       # p=predict(m(),KL)
      #  return(p)
      #})
########################################################################################################## 
      #IOP=reactive({
      #  TR=m()
      #  KT=input$Param
      #  P=length(KT)
      #  Formula=paste(input$Complication,"~",TR[1],"+")
      #  for(i in 1:P){
      #    if(i==P){
      #      Formula=paste(Formula,TR[i+1],"*",KT[i]) }
      #    else{
      #    Formula=paste(Formula,TR[i+1],"*",KT[i],"+")}
      #  }
      #  return(Formula)
      # })
      
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
    }