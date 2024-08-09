library(quarto)
library(tidyverse)

setwd("C:/Users/Gk Coordinator/Desktop/r-projects/kakumiro-village-reports/reports/village-reports/")

kakumiro_villages <- read_csv(file ="./data/Kakumiro_House_to_House_registeration_2024_07_28_08_38_21_278023.csv",name_repair = "universal_quiet", na = "n/a" ) |> 
  unite("section2.q4_village", `section2.q4_village1`:`section2.q4_village2`, na.rm = TRUE) |> 
  mutate(section2.q3_parish = str_to_title(section2.q3_parish ), 
         section2.q4_village = str_to_title(section2.q4_village))

villages <-
  kakumiro_villages |> 
  distinct(section2.q4_village )|>
  pull(section2.q4_village ) |>
  as.character()

reports <-
  tibble(
    input = "village-report.qmd",
    output_file = str_glue("{villages}-village-report.docx"),
    execute_params = map(villages , ~ list(village = .))
  )

pwalk(reports, quarto_render)

