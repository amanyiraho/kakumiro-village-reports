

setwd("C:/Users/Gk Coordinator/Desktop/r-projects/kakumiro-village-reports/baseline/")

library(UBOSpopulation)
library(UNEPItargets)
library(tidyverse)
library(scales)
library(showtext)
library(svglite)


font_add_google("Roboto", family = "Roboto")


font_add_google("Lora", family = "Lora")


font_add_google("Roboto Mono", "Roboto Mono")

showtext_auto()

grey_lighter <- "#dee0e3"
grey_light <- "#888A8C"
grey_dark <- "#939598"
blue_dark <- "#144673"
red_dark <- "#852922"




data("population_estimates")

UNEPItargets::surviving_infants_0_11m(population = 21300)/12
## 
HF_surviving_infants 

### Mwitanzige and District performance for the last year

district_sc_data <- read_csv("data/Kakumiro and Mwitanzige SC- DPT1 and MR2 FY2324.csv") |> 
  mutate(month = ym(month))
  
  
  district_sc_data |> 
    mutate(DPT1 = scales::percent_format("DPT1", scale = 1,accuracy = 1))
 # pivot_longer(cols = c(MR1, DPT1), names_to = "antigen", values_to = "coverage")

### DPT1 coverage

  ggplot() +
  geom_line(data = district_sc_data, mapping = aes(x = month, y = DPT1, colour = orgunit), linewidth = 1, lineend = "round") +
  geom_hline(yintercept = c(100), linetype = "dashed", color = grey_light )+
  geom_point(data = district_sc_data |> filter(month == dmy("01062024")),
             aes(x= month, y = DPT1, colour = orgunit)) +
  geom_text(data =district_sc_data |> filter(orgunit == "Mwitanzige Subcounty"), mapping = aes(x= month , y = DPT1, label = glue::glue("{DPT1}%"), colour = orgunit),nudge_y = 5, size = 5, family = "Roboto Mono") +
  theme_classic() +
  scale_color_manual(values = c("Mwitanzige Subcounty" = blue_dark,
                                 "Kakumiro District" = grey_dark)) + 
    scale_x_date(date_breaks = "month",date_labels = "%b%y",
                 limits = c(dmy("01072023"), dmy("01072024")))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,accuracy = 1), limits = c(0, NA) ) +
    labs(x = "Period",
         y = "DPT1 Coverage",
         caption = "Data source: DHIS2 07/08/2024",
         title = NULL,
         subtitle = NULL) +
    theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
          axis.ticks = element_line(colour = grey_light),
          axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
          axis.line = element_line(colour = grey_light),
          plot.caption = element_text(family = "Roboto",  size = 11, colour = grey_light),
          legend.text = element_text(colour = grey_light),
          legend.title = element_text(colour = grey_light, face = "bold"),
          legend.position = "none",
          plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
          plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
  
  ggsave(filename = "DPT1-coverage.svg", device = "svg", width = 36, height = 18, units = "cm" )  

  ### MR1 coverage
  
  
  ggplot() +
    geom_line(data = district_sc_data, mapping = aes(x = month, y = MR1, colour = orgunit), linewidth = 1, lineend = "round") +
    geom_hline(yintercept = c(100), linetype = "dashed", color = grey_light )+
    geom_point(data = district_sc_data |> filter(month == dmy("01062024")),
               aes(x= month, y = MR1, colour = orgunit)) +
    geom_text(data =district_sc_data |> filter(orgunit == "Mwitanzige Subcounty"), mapping = aes(x= month , y = MR1, label = glue::glue("{MR1}%"), colour = orgunit),nudge_y = 30, size = 5, family = "Roboto Mono") +
    theme_classic() +
    scale_color_manual(values = c("Mwitanzige Subcounty" = blue_dark,
                                  "Kakumiro District" = grey_dark)) + 
    scale_x_date(date_breaks = "month",date_labels = "%b%y",
                 limits = c(dmy("01072023"), dmy("01072024")))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,accuracy = 1, big.mark = ","), limits = c(0, 1500) ) +
    labs(x = "Period",
         y = "MR1 Coverage",
         caption = "Data source: DHIS2 07/08/2024",
         title = NULL,
         subtitle = NULL) +
    theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
          axis.ticks = element_line(colour = grey_light),
          axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
          axis.line = element_line(colour = grey_light),
          plot.caption = element_text(family = "Roboto",  size = 11, colour = grey_light),
          legend.text = element_text(colour = grey_light),
          legend.title = element_text(colour = grey_light, face = "bold"),
          legend.position = "none",
          plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
          plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
  
  ggsave(filename = "MR1-coverage.svg", device = "svg", width = 36, height = 18, units = "cm" ) 
  
  
## Zero dose of Mwitanzige subcounty for the Last 5 years
  
  
  subcounty_zd <- read_csv("data/ZD in mwitanzige 2021 - 2024.csv") |> 
    mutate(period = factor(period)) |> 
    filter(orgunit == "Mwitanzige Subcounty")

#### ZD children found
   
   zd_children <- read_csv("C:/Users/Gk Coordinator/Desktop/r-projects/kakumiro-village-reports/reports/activity-summary-report/data/geospatial-data.csv")
   
 zd_children_cleaned <-   zd_children |> 
     select(- c(gps,  lat,   lon, HHhead , HHtel, caregivers_name ,caregivers_tel ,caregivers_common)) |> 
     mutate(YOB = as.factor(year(childs_DOB)),
       age = interval(childs_DOB, dmy("31-07-2024")) %/% months(1)) |>
     filter(age >= 4) |> 
     select(-c(HHnum, childs_name, childs_age, childs_DOB))
 
zd_data_by_year <-  zd_children_cleaned |> 
   count(YOB, name = "ZD_reached") |> 
   left_join( subcounty_zd, by = c("YOB" = "period")) 

   ggplot()+
   geom_col(data = zd_data_by_year, aes(x = YOB, y = ZD), fill = grey_lighter) +
   geom_col(data = zd_data_by_year, aes(x = YOB, y = ZD_found), fill = blue_dark)+
   geom_label(data =zd_data_by_year, mapping = aes(x= YOB , y = ZD, label = ZD), size = 5, family = "Roboto Mono", color = grey_light) +
     geom_label(data =zd_data_by_year, mapping = aes(x= YOB , y = ZD_found, label = ZD_found), size = 5, family = "Roboto Mono", color = blue_dark) +
   theme_classic() +
   geom_hline(yintercept = 0, linetype = "solid", color = "black" )+
   scale_fill_manual(values = c("pos" = red_dark,
                                "neg" = grey_dark)) + 
   scale_y_continuous( limits = c(-200, 100) ) +
   labs(x = "Year",
        y = "Number of ZD",
        caption = "Data source: DHIS2 & H2H registeration 07/08/2024",
        title = NULL,
        subtitle = NULL) +
   theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
         axis.ticks = element_line(colour = grey_light),
         axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
         axis.line = element_line(colour = grey_light),
         plot.caption = element_text(family = "Roboto",  size = 11, colour = grey_light),
         legend.text = element_text(colour = grey_light),
         legend.title = element_text(colour = grey_light, face = "bold"),
         legend.position = "none",
         plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
         plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
   
   ggsave(filename = "ZD by year- Mwitanzige.svg", device = "svg", width = 36, height = 18, units = "cm" ) 
   
   
###
   
   zd_by_place_birth <- zd_children_cleaned |> 
     mutate(place_of_birth = case_when(place_of_birth == "other" ~ place_of_birth_other, 
                                                            .default = place_of_birth)) |> 
     select(-place_of_birth_other) |> 
     count(place_of_birth) |> 
     mutate(status = case_when(place_of_birth == "traditional birth attendants" ~ "pos", 
                                       .default = "neg"))
     ggplot() +
     geom_col(data = zd_by_place_birth , aes(x = reorder(place_of_birth, desc(n)), y = n, fill =status)) +
     geom_label(data =zd_by_place_birth , mapping = aes(x = place_of_birth, y = n, label = n), size = 5, family = "Roboto Mono", color = grey_dark) +
     theme_classic() +
     scale_fill_manual(values = c("pos" = blue_dark,
                                  "neg" = grey_lighter)) + 
     scale_y_continuous( limits = c(0, 40) ) +
     labs(x = "Place of Birth",
          y = "Number of ZD",
          caption = "Data source: H2H registeration 07/08/2024",
          title = NULL,
          subtitle = NULL) +
     theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
           axis.ticks = element_line(colour = grey_light),
           axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
           axis.line = element_line(colour = grey_light),
           plot.caption = element_text(family = "Roboto",  size = 11, colour = grey_light),
           legend.text = element_text(colour = grey_light),
           legend.title = element_text(colour = grey_light, face = "bold"),
           legend.position = "none",
           plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
           plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
     
     ggsave(filename = "ZD by Place of Birth- Mwitanzige.svg", device = "svg", width = 36, height = 18, units = "cm" )     
     
     
     
     #### Underimmunised by antigen 
    underimmunised_by_antigen <- read_csv("C:/Users/Gk Coordinator/Desktop/r-projects/kakumiro-village-reports/reports/activity-summary-report/data/underimmunised_by_antigen.csv")
    
    
    
    ggplot()+
      geom_col(data = underimmunised_by_antigen, aes(x = antigen, y = number_with_cards), fill = grey_lighter) +
      geom_col(data = underimmunised_by_antigen, aes(x = antigen, y = underimmunised), fill = blue_dark)+
      geom_label(data = underimmunised_by_antigen, mapping = aes(x= antigen , y = number_with_cards, label = number_with_cards), size = 5, family = "Roboto Mono", color = grey_light) +
      geom_label(data = underimmunised_by_antigen, mapping = aes(x= antigen , y = underimmunised, label = underimmunised), size = 5, family = "Roboto Mono", color = blue_dark) +
      theme_classic() +
      scale_y_continuous( limits = c(0, 800) ) +
      labs(x = "Antigen",
           y = "Number of underimmunised children",
           caption = "Data source: H2H registeration 31/07/2024",
           title = NULL,
           subtitle = NULL) +
      theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
            axis.ticks = element_line(colour = grey_light),
            axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
            axis.line = element_line(colour = grey_light),
            plot.caption = element_text(family = "Roboto",  size = 11, colour = grey_light),
            legend.text = element_text(colour = grey_light),
            legend.title = element_text(colour = grey_light, face = "bold"),
            legend.position = "none",
            plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
            plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
    
    ggsave(filename = "ZD by year- Mwitanzige.svg", device = "svg", width = 36, height = 18, units = "cm" ) 
    
     