library(devtools)
library(roxygen2)

cat("Установка папки разработки пакета как корневой директории...\n")
setwd("C:/Users/iivanova/Documents/R/1_sstps/rpackages/skigraph2")

cat("Генерация документации...\n")
document()

setwd("..")

cat("Сборка пакета \n")
build("skigraph2")

cat("Установка пакета...\n")
install("skigraph2")

cat("Процесс завершен.\n")
