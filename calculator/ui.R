library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)

shinyUI(
    fluidPage(
        useShinyjs(),
        #theme = "trtr.css",
        theme = shinytheme("superhero"), 
    navlistPanel(id ="menu", "Прогноз",well = FALSE, widths = c(3,9),
                 
                 tabPanel("Файл",h4("Подключите таблицу с данными о пациентах"),column(12, 
                          fileInput("file", h3(""),placeholder="выберите файл",accept = c(".csv", "text/csv", 
                                             "txt/comma-separated-values,text/plain")), 
                          uiOutput('main_grid')),
                          tags$style("#file{text-align:center; ")),
        
        tabPanel("Админка", value="Kalc",
                 style = "max-height: 700px; overflow: hidden; overflow-y: auto;}",
                tabsetPanel( tabPanel(
                        h4("Категории"),tags$style("h1{text-align:center;}"),
                        absolutePanel(top=NULL,left=NULL, where="beforeBegin",
                            actionButton("CreateCategory", "Создать категорию") )),
                 tabPanel(
                        h4("Осложнения"),tags$style("h1{text-align:center;}"),
                        absolutePanel(top=NULL,left=NULL, where="beforeBegin",
                          actionButton("CreateComplication", "Создать осложнение"))))),
        tabPanel("Челядь", value="TRTR",
                 style = "max-height: 700px; overflow: hidden; overflow-y: auto;}",
                 h1("Калькулятор"),tags$style("h1{text-align:center;}"),
                 fluidPage( h3("Пациент"),
                            column(4,textInput("surname", "Фамилия", placeholder="введите фамилию"), 
                                   numericInput("age", "Возраст:", min = 18, max = 105, value = 40, step = 1, width = 300),),
                            column(4, textInput("name", "Имя", placeholder="введите имя")),
                            column(4, textInput("fname", "Отчество", placeholder="введите отчество")),
                            radioButtons("gender","Пол:", c("Мужской","Женский")),
                 tabPanel(h4("Калькулятор"),tags$style("h1{text-align:center;}"))
                          #actionButton("Begin", "Начать заполнение")
                 )),      
                
        
        tabPanel("Помощь",h4("не дождетесь, в России же живете")), 
        tabPanel("О разработчке(ах?)",h4("ну тут все понятно, уебок делал")),
        tabPanel("Текст задания курсача", 
        style = "max-height: 700px; overflow: hidden; overflow-y: auto;}",
                 
        h3("Создайте онлайн калькулятор для расчета вероятности осложнений при лечении фибрилляции предсердий"),
        h4(" Используемые технологии: Shiny, R, - обязательно HTML, Javascript по желанию."),
        h4("Требование к калькулятору:"),
        h4("1. web приложение"),
        h4("2. нужна консоль или ее аналог для работы в разных вкладках "),
        h5("https://eios.sibsutis.ru/pluginfile.php/33392/question/questiontext/127258/1/59 "),
        h4("3. Во вкладке Файл должна быть возможность сохранить введенные показатели пациента и результаты прогноза. "),
        h5("Файл можно сохранить только тогда, когда заполнены ФИО на главной вкладке. Данные сохраняйте в .csv или .xlsx файлах с названием как ФИО пациента. "),
        h4("4. Описание предикторов и формулы находятся в файле https://eios.sibsutis.ru/pluginfile.php/33392/question/questiontext/127258/1/59 "),
        h4("5. По умолчанию серым полупрозрачным шрифтом заполните все вводимые показатели. Предусмотрите возможность сброса к значениям по умолчанию. "),
        h5("Если пользователь ввел значение из 50% области, то отображайте это значение черным шрифтом, если внутри 99% области, но больше 50% то светло оранжевым, если за пределами 99% интервала, то красным. Можно придумать свою систему подсказки пользователю, что он вышел из 99% или 50% области типичнвых значений предикторов. "),
        h4("6. Прогноз 4-х осложнений делается для двух типов лечения. 
           "),
        h5("Если вероятность осложнения ЭИТ меньше, чем без ЭИТ, то эту вероятность отображать зеленым, а вероятность без ЭИТ - красным цветом. https://eios.sibsutis.ru/pluginfile.php/33392/question/questiontext/127258/1/59 "),
        h4("7. Во вкладке помощь описать помощь пользователю для работы с Вашим приложением. "),
        h4("8. Во вкладке о программе написать сведения о себе как о разработчике. "),
        h4("9. На оценку 3 можно просто реализовать существующий дизайн, без размещения приложения в интернете. На оценки выше нужно разработать собственный дизайн, главная цель максимальная интуитивная понятность и современный деловой дизайн (можно не строго деловой) + размещение в интернете. "),
        h4("10. Для выставления оценки архивируйте winrar-ом работающий проект без пароля. Название архива calculator_(YOUR_FIO).rar. Прикрепите архив. Если разместили в интернете добавьте ссылку в ответе.
")), 
        tags$style("
                    
                   
                   
                   
                                    
                                    #complication{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #complication:focus{ text-align:center;border: solid #df691a;}
                                    #category{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #category:focus{ text-align:center;border: solid #df691a;} 
                                    #surname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #surname:focus{ text-align:center;border: solid #df691a;} 
                                    #name{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #name:focus{ text-align:center;border: solid #df691a;} 
                                    #fname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #fname:focus{ text-align:center;border: solid #df691a;}
                                    #Возраст{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #Возраст:focus{ text-align:center;border: solid #df691a;}
                                    #ОХ{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ОХ:focus{text-align:center;border: solid #df691a;}
                                    #ЛПНП{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #ЛПНП:focus{text-align:center; border: solid #df691a;}
                                    #КДР{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #КДР:focus{text-align:center; border: solid #df691a;}
                                    #ФВ{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #ФВ:focus{text-align:center;border: solid #df691a;}
                                    #ДАД{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ДАД:focus{text-align:center;border: solid #df691a;}
                                    #САД{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #САД:focus{text-align:center;border: solid #df691a;}
                                    #ЧСС{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ЧСС:focus{text-align:center;border: solid #df691a;}
                                    #ИММЛЖ{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ИММЛЖ:focus{text-align:center;border: solid #df691a;}
                                    #ТГ{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ТГ:focus{text-align:center;border: solid #df691a;}
                                    #ЛПВП{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ЛПВП:focus{text-align:center;border: solid #df691a;}
                                    #ЛП{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #ЛП:focus{text-align:center;border: solid #df691a;}
                                    #СКФ{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #СКФ:focus{text-align:center;border: solid #df691a;}
                                    #СРБ{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #СРБ:focus{text-align:center;border: solid #df691a;}
                                    
                                    
                                    
                "),
        mainPanel()
    )
    )
)
