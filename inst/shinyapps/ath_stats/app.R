library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)

# Устанавливаем локаль один раз при запуске
Sys.setlocale("LC_TIME", "RUSSIAN")
# load("joined_data_vars_trim.RData")
load("joined_data_rus_points.RData")

# создаем функцию для фильтрации соревнований на основе встречающихся слов
text_filter <- function(x, y) {
  pattern <- paste(y, collapse = "|")
  str_detect(x, regex(pattern, ignore_case = TRUE))
}
# 
# off_data_vars  <-  load("C:/Users/iivanova/Documents/R/1_sstps/rpackages/skigraph2/data/joined_data_rus_points.RData")
# 
# off_data_vars  <-  load(system.file("C:/Users/iivanova/Documents/R/1_sstps/rpackages/skigraph2/data/joined_data_rus_points.RData", package = "skigraph2")) 

# off_data_vars <- get(load(system.file("../../data/joined_data_rus_points.RData", package = "skigraph2")))

off_data_vars <- joined_data_rus_points

ui <- fluidPage(
  titlePanel("Статистика выступлений: сумма РУС пунктов за зимний сезон по спринту и дистанциям"),
  
  fluidRow(
    
    column(
      width = 8,
      plotOutput("histFacetPlot3")
    ),
    
    column(
      width = 2,
      tableOutput("athleteResults3")
    )
    
  ),
  
  titlePanel("Сумма очков за зимний сезон по спринту и дистанциям"),
  
  fluidRow(
    
    column(
      width = 8,
      plotOutput("histFacetPlot2")
    ),
    
    column(
      width = 2,
      tableOutput("athleteResults2")
    )
    
  ),
  
  titlePanel("Отчет по выбранному сезону"),  
  fluidRow(
    column(
      width = 4,
      selectizeInput(
        inputId = "athlete",
        label = "Выберите спортсмена:",
        choices = sort(unique(off_data_vars$select_ath)),
        multiple = FALSE,
        options = list(placeholder = 'Начните вводить для поиска')
      )
    ),
    column(
      width = 4,
      selectInput("season", "Выберите сезон:", choices = sort(unique(as.numeric(as.character(off_data_vars$Сезон))), decreasing = T))
    ),
    column(
      width = 4,
      selectInput("variable", "Выберите показатель:", choices = sort(c("Дистанция", "Вид старта", "Стиль", 'Категория')))
      
    ),
    column(
      width = 4,
      selectInput("variable2", "Выберите значения y:", choices = c("Место", "РУС пункты", "Очки"))
      
    )
  ),
  
  
  # Остальной контент
  fluidRow(
    column(
      width = 8,
      offset = 2,
      
      plotOutput("histFacetPlot"),
      
      tableOutput("athleteResults"),
      
      
      # Кнопки для скачивания таблиц
      fluidRow(
        column(
          width = 8,
          offset = 2,
          downloadButton("downloadTable1", "Скачать таблицу результатов спортсмена (CSV)")
        )
      ),
      
      fluidRow(
        column(
          width = 8,
          offset = 2,
          downloadButton("downloadTable2", "Скачать сводную таблицу очков (CSV)")
        )
      )
      
    )
  )
)

server <- function(input, output) {
  
  # Данные выбранного спортсмена
  # selected_data <- reactive({
  #   req(input$athlete)
  #   # req(input$season)
  #   off_data_vars %>%
  #     filter(select_ath == input$athlete)
  # })
  
  # Загрузить датасет при запуске
  # off_data_vars <- get(load(system.file("../data/joined_data_rus_points.RData", package = "skigraph2")))
  

  
  selected_data <- reactive({
    req(input$athlete)
    
    off_data_vars %>%
      filter(`RUS код` == str_extract(input$athlete, "(?<=_)[0-9]+$"))
  })
  
  # Таблица результатов выбранного спортсмена
  athlete_results_table <- reactive({
    selected_data() %>%
      filter(Сезон == input$season) %>% 
      select("Дата", "Место", "Очки", `РУС пункты`, 'Сп. Дисциплина', 'Город', 'Категория') %>% 
      mutate(Дата = as.character(as.Date(Дата, "%Y-%m-%d"))) %>% 
      mutate(Место = as.character(Место, 0)) %>% 
      mutate(Очки = ifelse(is.na(Очки), "-", Очки))%>% 
      mutate(`РУС пункты` = ifelse(is.na(`РУС пункты`), "-", `РУС пункты`)) %>% 
      mutate(`РУС пункты` = as.character(`РУС пункты`, 0)) 
  })
  
  # Таблица сводных очков
  athlete_results_summary <- reactive({
    
    selected_data()%>% 
      group_by(Сезон, `Тип дистанции`) %>% 
      summarise(`Сумма очков` = sum(Очки, na.rm = T)) %>% 
      filter(is.na(`Тип дистанции`) == F) %>% 
      pivot_wider(names_from = 'Тип дистанции', values_from = 'Сумма очков') %>% 
      filter(Дист > 0 | Спринт > 0)
    
  })
  
  # Таблица сводных рус-пунктов
  athlete_rpoints_summary <- reactive({
    
    selected_data()%>%
      group_by(`RUS код`, Сезон) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(is.na(`РУС пункты дист`) == F & is.na(`РУС пункты спринт`) == F) %>% 
      select(Сезон, `РУС пункты дист` , `РУС пункты спринт`)%>% 
      rename(c(Дист = `РУС пункты дист`, Спринт = `РУС пункты спринт`))
    
    
  })
  
  # Вывод таблицы результатов спортсмена
  output$athleteResults <- renderTable({
    athlete_results_table()
  })
  
  # Вывод сводной таблицы
  output$athleteResults2 <- renderTable({
    athlete_results_summary()
  })
  
  # Вывод сводной таблицы по очкам
  output$athleteResults3 <- renderTable({
    athlete_rpoints_summary()
  })
  
  # График с фасетами и линиями всех результатов выбранного спортсмена
  output$histFacetPlot <- renderPlot({
    
    
    selected_data() %>%
      filter(Сезон == input$season)%>%
      mutate(`РУС пункты`=as.numeric(`РУС пункты`))%>%
      filter(!is.na(!!sym(input$variable2)))%>%
      filter(!text_filter(`Сп. Дисциплина`, c('общий', 'чистое'))) %>%
      ggplot(aes(x=Дата,y=!!sym(input$variable2),col=!!sym(input$variable),group=!!sym(input$variable)))+
      geom_line(linewidth=1.2, na.rm=TRUE)+
      geom_point(size=3, na.rm=TRUE)+
      scale_x_date(date_breaks="1 month", date_labels="%b")+
      theme_bw()
    
  })
  
  # График с суммой очков или РУС пунктов по сезонам
  output$histFacetPlot2 <- renderPlot({
    
    selected_data() %>%
      group_by(Сезон, `Тип дистанции`) %>%
      filter(is.na(`Тип дистанции`) == F) %>% 
      summarise(`Сумма очков` = sum(Очки, na.rm = T)) %>%
      ungroup() %>%
      ggplot(aes(x = Сезон, y = `Сумма очков`, fill = `Тип дистанции`)) +
      geom_col()+
      theme_bw()+
      ylab("Сумма")
  })
  
  # График с суммой очков или РУС пунктов по сезонам
  output$histFacetPlot3 <- renderPlot({
    
    selected_data() %>%
      group_by(`RUS код`, Сезон) %>% 
      slice(1) %>%
      filter(is.na(`РУС пункты дист`) == F & is.na(`РУС пункты спринт`) == F) %>%
      rename(c(Дист = `РУС пункты дист`, Спринт = `РУС пункты спринт`)) %>%
      pivot_longer(cols = c(`Дист` , `Спринт`)) %>%
      # pivot_longer(cols = c(`РУС пункты дист` , `РУС пункты спринт`)) %>%
      mutate(value = as.numeric(value)) %>% 
      ggplot(aes(x = Сезон, y = value, fill = name)) +
      geom_col()+
      theme_bw()+
      ylab("Сумма РУС пунктов")+
      theme(legend.title = element_blank())
    
  })
  
  # Обработчики для скачивания таблиц
  output$downloadTable1 <- downloadHandler(
    filename = function() {
      paste0("результаты_", input$athlete, "_сезон_", input$season, ".csv")
    },
    content=function(file){
      write.csv(athlete_results_table(), file, row.names=FALSE, fileEncoding = "Windows-1251")
    }
  )
  
  output$downloadTable2 <- downloadHandler(
    filename=function() {
      paste0("сводные_очки_", input$athlete, "_сезон_", input$season, ".csv")
    },
    content=function(file){
      write.csv(athlete_results_summary(), file, row.names=FALSE, fileEncoding = "Windows-1251")
    }
  )
}

shinyApp(ui, server)