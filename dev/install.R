cat("Отключение пакета, если он использовался...\n")
if ("package:skigraph2" %in% search()) {
  detach("package:skigraph2", unload = TRUE)
}

cat("Установка пакета с гитхаба...\n")
devtools::install_github("ivinna/skigraph2")

cat("Загрузка пакета...\n")
library(skigraph2)

cat("Запуск приложения для генерации отчетов...\n")
shiny::runApp(system.file("shinyapps/live_report/app.R", package = "skigraph2"))
