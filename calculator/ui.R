library(shiny)
library(shinythemes)
library(shinyjs)
library (shinydashboard)

shinyUI(
    fluidPage(
        
        #theme = "trtr.css",
        theme = shinytheme("superhero"), 
    navlistPanel(id ="menu", "Прогноз",well = FALSE, widths = c(3,9),
                 
        tabPanel("Файл",h4("Подключите таблицу с данными о пациентах"),column(12, 
            fileInput("file", h3(""),placeholder="выберите файл")),
            tags$style("#file{text-align:center; ")),
        
        tabPanel("Калькулятор",
                 style = "max-height: 700px; overflow: hidden; overflow-y: auto;}",
                 h1("Калькулятор"), tags$style("h1{text-align:center;}"),
                 absolutePanel(top=NULL,left=NULL, where="beforeBegin",
                              actionButton("CreateCategory", "Создать категорию"))),
                actionButton("FinishRegression", "Завершить регрессию"),
                
        
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
                    
                   
                   
                   
                   
                                    #category{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #category:focus{ text-align:center;border: solid #df691a;} 
                                    #surname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #surname:focus{ text-align:center;border: solid #df691a;} 
                                    #name{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #name:focus{ text-align:center;border: solid #df691a;} 
                                    #fname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #fname:focus{ text-align:center;border: solid #df691a;}
                                    #age{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #age:focus{ text-align:center;border: solid #df691a;}
                                    #LP{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #LP:focus{text-align:center;border: solid #df691a;}
                                    #IMMLZH{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #IMMLZH:focus{text-align:center; border: solid #df691a;}
                                    #KDR{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #KDR:focus{text-align:center; border: solid #df691a;}
                                    #FV{text-align:center;background-color:#363636; color: #FFFFFF}
                                    #FV:focus{text-align:center;border: solid #df691a;}
                                    #OK{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #OK:focus{text-align:center;border: solid #df691a;}
                                    #LPV{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #LPV:focus{text-align:center;border: solid #df691a;}
                                    #GL{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #GL:focus{text-align:center;border: solid #df691a;}
                                    #GAL{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #GAL:focus{text-align:center;border: solid #df691a;}
                                    #NUP{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #NUP:focus{text-align:center;border: solid #df691a;}
                                    #SRB{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #SRB:focus{text-align:center;border: solid #df691a;}
                                    #MK{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #MK:focus{text-align:center;border: solid #df691a;}
                                    #SKF{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #SKF:focus{text-align:center;border: solid #df691a;}
                                    #SAD{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #SAD:focus{text-align:center;border: solid #df691a;}
                                    #DAD{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #DAD:focus{text-align:center;border: solid #df691a;}
                                    #CHSS{text-align:center;background-color:#363636; color: #FFFFFF} 
                                    #CHSS:focus{text-align:center;border: solid #df691a;}
                "),
        mainPanel()
    )
    )
)
