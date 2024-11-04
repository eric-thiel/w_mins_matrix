library(readr)

NBA_MINS_MATRIX_cleaned <- read_csv("~/Downloads/NBA MINS MATRIX - cleaned (1).csv")
NBA_MINS_MATRIX_cleaned$minutes = round(NBA_MINS_MATRIX_cleaned$minutes, 2)

write.csv(NBA_MINS_MATRIX_cleaned, file = "NBA_final_results.csv", row.names = FALSE)



