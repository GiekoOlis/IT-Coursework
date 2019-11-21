library(shiny)
library(shinythemes)
library(shinyjs)
library (shinydashboard)

shinyUI(
    fluidPage(
        
        #theme = "trtr.css",
        theme = shinytheme("superhero"), 
    navlistPanel(id ="menu", "Прогноз",well = FALSE, widths = c(3,8),
        tabPanel("Файл",h4("Подключите таблицу с данными о пациентах"),column(12, 
            fileInput("file", h3(""),placeholder="выберите файл")),
        tags$style("#file{text-align:center; ")),
        tabPanel("Калькулятор",
                 fluidPage( h4("Пациент"),
                            checkboxGroupInput("param", "Выберите плз:",
                                               c(
                                                   "dfgh"="sfdg",
                                                   "dfgh"="sfdg",
                                               )
                            )
                            
                 ),
                     fluidPage( h4("Пациент"),
                        
                         column(4,
                               
                                textInput("surname", "Фамилия", placeholder="введите фамилию"),
                                 tags$style("#surname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #surname:hover{ text-align:center;
                                        border: solid #df691a;} }")
                         ),
                         column(4,
                                
                                textInput("name", "Имя", placeholder="введите имя"),
                                tags$style("#name{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #name:hover{ text-align:center;
                                        border: solid #df691a;} } ")
                         ),
                         column(4,
                                
                                textInput("fname", "Отчество", placeholder="введите отчество"),
                                tags$style("#fname{text-align:center;background-color:#363636; color: #FFFFFF;} 
                                    #fname:hover{ text-align:center;
                                        border: solid #df691a;
                                           box-shadow: 0 0 8px rgba(0, 0, 0, 0.15);} } ")
                                )),
            
                         fluidPage( hr(),
                             h4("ЭХО КГ"),
                             column(4,
                                    
                                    
                                    numericInput("LP", "ЛП:", min = 0, max = 10, value = 4.6, step = 0.5, width = 300),
                                    tags$style("#LP{background-color:#363636; color: #FFFFFF}} "),
                                    numericInput("IMMLZH", "ИММЛЖ:", min = 0, max = 300, value = 124, step = 1, width = 300),
                                    tags$style("#IMMLZH{background-color:#363636; color: #FFFFFF}} ")
                                    
                             ),
                             column(4,
                                    
                                    numericInput("KDR", "КДР:", min = 0, max = 20, value = 5.8, step = 0.5, width = 300),
                                    tags$style("#KDR{background-color:#363636; color: #FFFFFF}} "),
                                    numericInput("FV", "ФВ:", min = 0, max = 100, value = 59, step = 1, width = 300),
                                    tags$style("#FV{background-color:#363636; color: #FFFFFF}} ")
                             )
                         
                ),
            fluidPage( hr(), h4("Биохимические показатели"),
                
                             column(4,
                                   
                                    numericInput("OK", "Общий холестирин:", min = 0, max = 10, value = 4.97, step = 0.5, width = 300),
                                    numericInput("LPV", "ЛПВП:", min = 0, max = 5, value = 1.42, step = 0.5, width = 300),
                                    numericInput("GL", "Глюкоза:", min = 0, max = 10, value = 5.93, step = 1, width = 300),
                                    numericInput("GAL", "Галлектин:", min = 0, max = 150, value = 16.34, step = 1, width = 300)
                            ),
                             column(4, 
                                    numericInput("NUP", "НУП:", min = 0, max = 300, value = 98.7, step = 1, width = 300),
                                    numericInput("SRB", "СРБ:", min = 0, max = 15, value = 5.6, step = 0.5, width = 300),
                                    numericInput("MK", "Мочевя кислота:", min = 0, max = 900, value = 234.1, step = 1, width = 300),
                                    numericInput("SKF", "СКФ:", min = 0, max = 200, value = 70.4, step = 1, width = 300)
                            )
                      ),
            
            fluidPage(hr(),h4("Прогноз"),
                      
                             fluidRow(
                                 column(2, " "),
                                 column(2, "ФП"),
                                 column(2, "Повторная госпитализация"),
                                 column(2, "Кардиоэмболия"),
                                 column(2, "ХСН")),
                             br(),
                             fluidRow(
                                 column(2, "ЭИТ"),
                                 column(2, ("EFP")),
                                 column(2, ("EPZ")),
                                 column(2, ("EKEYA")),
                                 column(2, ("EHSN"))),
                             
                             br(),
                             fluidRow(
                                 column(2, "Без ЭИТ"),
                                 column(2, ("BFP")),
                                 column(2, ("BPG")),
                                 column(2, ("BKE")),
                                 column(2, ("BHSN"))),
                             
                             br())
                         ),
        
        tabPanel("Помощь",h4("не дождетесь, в России же живете")), 
        tabPanel("О разработчке(ах?)",h4("ну тут все понятно, уебок делал")),
        tabPanel("Текст задания курсача",
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
       # tags$style("color:Black;"),
        mainPanel()
    )
    )
)