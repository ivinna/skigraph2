library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Устанавливаем локаль один раз при запуске
Sys.setlocale("LC_TIME", "RUSSIAN")
# load("joined_data_vars_trim.RData")
# load("results_2025.RData")
load("results_df.RData")

results_2025 <- results_df %>% 
  # select(Город, Дата,`Сп. Дисциплина`, comp_id, Событие, Пол) %>% 
  mutate(Title = paste(Дата, Город,`Сп. Дисциплина`, Событие, Пол)) %>% 
  mutate(split = str_replace_all(split, "Претайминг", ""))
  

text_filter <- function(x, y) {
  pattern <- paste(y, collapse = "|")
  str_detect(x, regex(pattern, ignore_case = TRUE))
}

laps <- results_2025 %>% 
  mutate(len = as.numeric(len), 
         n = as.numeric(n)) %>% 
  group_by(comp_id) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(Город, Дата,`Сп. Дисциплина`, comp_id, Событие, Пол) %>% 
  mutate(Title = paste(Дата, Город,`Сп. Дисциплина`, Событие, Пол))

find_laps = function(vec){
  x = vec[1]
  y = vec[2]
  seq(from = 0, to = x, by = y)}

diff_w_next <- function(x){c(x[1],(x[2:length(x)]-x[1:(length(x)-1)]))}


# пример app.R

library(shiny)

ui <- fluidPage(
  
  # Название приложения
  titlePanel("Анализ прохождения отрезков дистанции"),
  
  # ввод данных
  fluidRow(
    column(
      width = 4,
      selectizeInput(
        inputId = "comp",
        label = "Выберите соревнование:",
        choices = laps$Title),
        multiple = FALSE,
        options = list(placeholder = 'Начните вводить для поиска')
      )
    ),
  
    column(
      width = 4,
      selectizeInput(
        inputId = "ath_1",
        label = "Выберите спортсмена:",
        choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
    ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_2",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_3",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_4",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_5",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_6",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_7",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_8",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_9",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_2025$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
    
    # Показать график 
    mainPanel(
      
      h3("График 1: Разница времени прохождения отдельных отрезков гонки"),
      
      plotOutput("my_plot"),
      actionButton("save_plot1", "Сохранить график 1"),
      
      h3("График 2: Текущий проигрыш на промежуточных отсечках"),
      
      plotOutput("my_plot2"),
      actionButton("save_plot2", "Сохранить график 2"),
      
      # tableOutput("splits"),
      
      # tableOutput("ath_vec"),
      
      tableOutput("report")
  )
)

# Задаем логику сервера, требуемую для рисования гистограммы
server <- function(input, output) {

  ff <- reactive({
    req(input$comp)

    results_2025 %>% 
      filter(Title == input$comp) %>% 
      ungroup()
    # filter(split == "Финиш") 
    # %>%
    #   mutate(place = as.integer(as.character(place))) %>% 
    #   filter(place < input$n_ath) 
    
    
    
  })
  
  report <- reactive({
    req(input$comp)
    
    ff() %>%
      filter(split == "Финиш")%>%
      mutate(place = as.integer(place)) %>% 
      select(place, athlete, res, diff)

  })
  
  
  splits <- reactive({
    
    find_laps(as.numeric(laps[laps$comp_id == input$comp, -(1:3)]))
    
  })
  
  # default_ath_vec <- reactive({
  #   
  #   ff() %>%
  #     filter(split == unique(ff()$split)[length(unique(ff()$split))]) %>%
  #     # filter(split == "Финиш") %>%
  #     mutate(place = as.integer(place)) %>%
  #     filter(place <= 6) %>%
  #     pull(athlete) %>%
  #     unique()
  #   
  # })
  
  ath_vec <- reactive({
    req(input$ath_1)
    req(input$ath_2)
    req(input$ath_3)
    req(input$ath_4)
    req(input$ath_5)
    req(input$ath_6)
    req(input$ath_7)
    req(input$ath_8)
    req(input$ath_9)

    c(input$ath_1, input$ath_2, input$ath_3, input$ath_4, input$ath_5, input$ath_6, input$ath_7, input$ath_8, input$ath_9)

  })
  
  selected_data <- reactive({
    
    left_join(ff() %>%
                filter(text_filter(as.character(athlete), as.character(ath_vec()))) %>%
                group_by(athlete) %>%
                mutate(lap_res = diff_w_next(res_secs)) %>%
                ungroup() %>%
                group_by(split) %>%
                mutate(lap_best = min(lap_res, na.rm = T)) %>% #лучший показатель за отрезок запишем в столбец
                mutate(diff_time = lap_res - lap_best,
                         speed = lap_len/lap_res) %>%
                mutate(cur_diff_res = res_secs - min(res_secs)) %>%
                mutate(split = factor(split, levels = unique(ff()$split))),
              
              ff() %>%
                ungroup() %>% 
                filter(split == "Финиш") %>%
                mutate(ath_place = paste(athlete, place)) %>% 
                select(bib, ath_place), by = "bib")
    
    

    
  })
  
  
  
  # 
  #   ff() %>%
  #     # mutate(lap = cut(split_m, breaks = splits(), labels = 1:(length(splits) - 1))) %>%
  #     filter(text_filter(athlete, ath_vec())) %>% 
  #   # %>%
  #     mutate(res_secs = as.numeric(ms(res))) %>%
  #     group_by(athlete) %>%
  #     mutate(lap_res = diff_w_next(res_secs)) %>%
  #   #   ungroup() %>%
  #   #   group_by(split) %>%
  #   #   mutate(lap_best = min(lap_res, na.rm = T)) %>% #лучший показатель за отрезок запишем в столбец
  #   #   mutate(diff_time = lap_res - lap_best,
  #   #          speed = lap_len/lap_res) %>%
  #   #   mutate(split = factor(split, levels = unique(ff$split)))
  # 
  # })
  

  # plot_1 <- reactive({
  #   selected_data()  %>% 
  #     ggplot(aes(x = split, y = diff_time, col = athlete, group = as.character(athlete))) + geom_point() + geom_line() +
  #     theme_bw()
  # })
  
  plot_1 <- reactive({
    selected_data() %>%
      ggplot(aes(x = split, y = diff_time, group = ath_place, col = ath_place))+
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL)) +
      ylab("Разница в секундах") +
      xlab("")
    
  })
  
  plot_2 <- reactive({
    selected_data() %>%
      ggplot(aes(x = split, y = cur_diff_res, group = ath_place, col = ath_place))+
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL)) +
      ylab("Разница в секундах") +
      xlab("")

  })
  
  output$ath_vec <- renderTable({
    ath_vec()
  })
  
  output$splits <- renderTable({
    splits()
  })
  
  output$ff <- renderTable({
    ff() %>%
      filter(text_filter(athlete, ath_vec())) %>% 
      filter(place <= input$n_ath) 
  })
  
  output$selected_data <- renderTable({
    selected_data()
  })

  output$my_plot <- renderPlot({
    plot_1()
  })
  
  observeEvent(input$save_plot1, {
    # Задаем путь для сохранения
    save_plot1 <- selected_data() %>%
      ggplot(aes(x = split, y = diff_time, group = ath_place, col = ath_place)) +
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL)) +
      ylab("Разница в секундах") +
      xlab("")
    ggsave("my_plot_1.png", plot = save_plot1, width = 8, height = 6)
    showNotification("График сохранен как my_plot_1.png")
    
    
  })
  
  output$my_plot2 <- renderPlot({
    plot_2()
  })
  
  observeEvent(input$save_plot2, {
    # Перед сохранением вызываем нужный plot именно для второго графика
    # Можно сохранить конкретный график
    # Для этого создайте его отдельно
    
    save_plot2 <- selected_data() %>%
      ggplot(aes(x = split, y = cur_diff_res, group = ath_place, col = ath_place))+
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL))
    ggsave("my_plot_2.png", plot = save_plot2, width = 8, height = 6)
    showNotification("Второй график сохранен как plot2.png")
  })

  
  
  output$report <- renderTable({
    report()
  })
  
}


# Выполняем приложение 

shinyApp(ui = ui, server = server)

