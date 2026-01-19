library(shiny)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Устанавливаем локаль один раз при запуске
Sys.setlocale("LC_TIME", "RUSSIAN")

lf <- list.files("data/comps/")

dat <- lapply(paste0("data/comps/", lf), read.xlsx)

results_2026 <- data.frame(do.call(rbind, dat))

results_df <- results_2026 %>%
  select(-len, -n, -lens) %>%
  mutate(Title = paste(Дата, Город,`Сп..Дисциплина`, Событие, Пол)) %>%
  mutate(split = str_replace_all(split, "Претайминг", ""))


text_filter <- function(x, y) {
  pattern <- paste(y, collapse = "|")
  str_detect(x, regex(pattern, ignore_case = TRUE))
}

laps <- results_df %>%
  group_by(comp_id) %>%
  slice(1) %>%
  ungroup %>%
  select(Город, Дата,`Сп..Дисциплина`, comp_id, Событие, Пол) %>%
  mutate(Title = paste(Дата, Город,`Сп..Дисциплина`, Событие, Пол))

diff_w_next <- function(x){c(x[1],(x[2:length(x)] - x[1:(length(x)-1)]))}

# app.R

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
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_2",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_3",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_4",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_5",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_6",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_7",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_8",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
      multiple = FALSE,
      options = list(placeholder = 'Начните вводить для поиска')
    )
  ),
  
  column(
    width = 4,
    selectizeInput(
      inputId = "ath_9",
      label = "Выберите спортсмена:",
      choices = c("Оставить пустым", sort(unique(results_df$athlete))),
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

# Задаем требуемую логику сервера
server <- function(input, output) {
  
  ff <- reactive({
    req(input$comp)
    
    results_df %>%
      filter(Title == input$comp) %>%
      ungroup()
    
    
  })
  
  report <- reactive({
    req(input$comp)
    
    ff() %>%
      filter(split == "Финиш")%>%
      mutate(place = as.integer(place)) %>%
      select(place, athlete, res, diff)
    
  })
  
  
  
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
                filter(lap_res !=0) %>%
                group_by(split) %>%
                mutate(lap_best = min(lap_res, na.rm = T)) %>% #лучший показатель за отрезок запишем в столбец
                mutate(diff_time = lap_res - lap_best,
                       speed = "") %>%
                mutate(cur_diff_res = res_secs - min(res_secs)) %>%
                mutate(split = factor(split, levels = unique(ff()$split))),
              
              ff() %>%
                ungroup() %>%
                filter(split == "Финиш") %>%
                mutate(ath_place = paste(athlete, place)) %>%
                select(bib, ath_place), by = "bib", relationship = "many-to-many")
    
    
  })
  
  plot_1 <- reactive({
    selected_data() %>%
      ggplot(aes(x = split, y = diff_time, group = ath_place, col = ath_place))+
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL)) +
      labs(
        title = input$comp,
        x = "",
        y = "Разница в секундах"
      ) +
      theme(legend.position = "bottom")
    
  })
  
  plot_2 <- reactive({
    selected_data() %>%
      ggplot(aes(x = split, y = cur_diff_res, group = ath_place, col = ath_place))+
      geom_point()+
      geom_line()+
      theme_bw() +
      guides(color = guide_legend(title = NULL)) +
      labs(
        title = input$comp,
        x = "",
        y = "Разница в секундах"
      ) +
      theme(legend.position = "bottom")
    
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
      labs(
        title = paste(input$comp, "разница по отрезкам"),
        x = "",
        y = "Разница в секундах"
      ) +
      theme(legend.position = "bottom")
    
    ggsave(paste(input$comp, "отрезки.png", collapse = "_"), plot = save_plot1, width = 8, height = 6)
    # ggsave("my_plot_1.png", plot = save_plot1, width = 10, height = 6)
    showNotification("График сохранен")
    
    
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
      guides(color = guide_legend(title = NULL))+
      labs(
        title = paste(input$comp, "текущая разница"),
        x = "",
        y = "Разница в секундах"
      ) +
      theme(legend.position = "bottom")
    
    ggsave(paste(input$comp, "текущая разница.png", collapse = "_"), plot = save_plot2, width = 8, height = 6)
    # ggsave("my_plot_2.png", plot = save_plot2, width = 10, height = 6)
    showNotification("Второй график сохранен")
  })
  
  
  
  output$report <- renderTable({
    report()
  })
  
}


# Выполняем приложение

shinyApp(ui = ui, server = server)
