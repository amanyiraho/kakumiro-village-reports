library(quarto)
library(tidyverse)

setwd("C:/Users/Gk Coordinator/Desktop/r-projects/kakumiro-village-reports/reports/parish-reports/")

kakumiro_villages <- read_csv(file ="./data/Kakumiro_House_to_House_registeration_2024_07_28_08_38_21_278023.csv",name_repair = "universal_quiet", na = "n/a" ) |> 
  unite("section2.q4_village", `section2.q4_village1`:`section2.q4_village2`, na.rm = TRUE) |> 
  mutate(section2.q3_parish = str_to_title(section2.q3_parish ), 
         section2.q4_village = str_to_title(section2.q4_village))

parishes <-
  kakumiro_villages |> 
  distinct(section2.q3_parish )|>
  pull(section2.q3_parish ) |>
  as.character()

reports <-
  tibble(
    input = "parish-report.qmd",
    output_file = str_glue("{parishes}-parish-report.docx"),
    execute_params = map(parishes , ~ list(parish = .))
  )

pwalk(reports, quarto_render)


