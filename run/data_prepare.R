prepare_data <- function(dataPath) {

  table1 <- readRDS(file.path("out", "table1Save.rds"))
  table2 <- readRDS(file.path("out", "table2Save.rds"))

  list(
    table1 = as.data.frame(table1),
    table2 = as.data.frame(table2)
  )
}