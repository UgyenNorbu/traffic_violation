library(ggplot2)
library(lubridate)
library(scales)
library(janitor)
file_path <- "offense_details.xlsx"
violation_data <- readxl::read_xlsx(file_path)

my_theme <- theme(axis.text = element_text(size = 11, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      legend.position = "top")

# 1. TRAFFIC VIOLATION TREND ----------

by_month <- violation_data %>%
    group_by(offence_date = floor_date(Offence_Date, "month")) %>%
    summarize(no_of_off = n())

selected_years <- by_month %>% 
    filter(!is.na(offence_date)) %>% 
    filter(year(offence_date) > 2011) %>%
    filter(year(offence_date) < 2020) %>% 
    group_by(year(offence_date)) %>%
    mutate(annual_avg = mean(no_of_off))

selected_years %>% 
    ggplot(aes(x = offence_date, y = no_of_off)) +
    geom_area(fill = "#FDEDEC", colour ="#F5B7B1", alpha = 0.5) +
    geom_line(aes(x = offence_date, y = annual_avg, colour = "Avg. monthly traffic violation"),
              size = 1.5, linetype = "dashed") +
    theme_light() +
    labs(x = "Year",
         y = "No. of MVC",
         colour = "") +
    theme(plot.title = element_text(face = "bold", family = "Times", hjust = 0.5),
          axis.title = element_text(family = "Times"),
          axis.text  = element_text(family = "Times"),
          legend.position = c(0.89, 1)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_manual(values = c("Avg. monthly traffic violation" = "#B22222"))
    
ggsave("traffic violation.jpg", width = 25, height = 15, units = "cm")

# 2. MAJOR AND MINOR OFFENCEE --------------------------------------------

# violation_data %>% 
#     select(Offence_Name) %>% 
#     unique() %>% 
#     View()

major_off_list <- c("Using mobile phone while driving", "Unlicensed driving", "Reckless Driving", "Over speeding", "Over-loading", "No RC on the spot", "No DL while driving", "Invalid DL", "Hit and run", "Excess passenger", "DUI", "Driving while intoxicated")

maj_min_offence <- violation_data %>% 
    mutate(offence_category = ifelse(Offence_Name %in% major_off_list, 
                                     "Major_offence", "Minor_offence")) %>%
    group_by(offence_date = floor_date(Offence_Date, "month"), offence_category) %>%
    summarize(no_of_off = n()) %>% 
    filter(!is.na(offence_date)) %>% 
    filter(year(offence_date) > 2011) %>%
    filter(year(offence_date) < 2020)

maj_min_offence %>% 
    ggplot(aes(x = offence_date, y = no_of_off, colour = offence_category)) +
    geom_line()

# 3. MVC CASAULTIES -------------------------------------------------------

data_path <- "MVC_compiled_data.xlsx"
MVC_data_original <-  readxl::read_xlsx(data_path)

MVC_data <- MVC_data_original %>% 
    select(-no_MVC)

MVC_data <-  MVC_data %>% 
    pivot_longer(-Time, names_to = "Elements", values_to = "Frequency") %>% 
    separate(Elements, c("crash_severity", "gender"), sep = "_")

MVC_data %>% 
    ggplot(aes(x = Time, y = Frequency, color = gender)) +
    geom_line() +
    facet_grid(crash_severity~.,) +
    theme_bw() +
    labs(x = "Year",
         y = "Number of road users",
         # title = "Trend of road user injury and death due to motor vehicle crash",
         color = "") +
    scale_color_discrete(labels = c("Female", "Male")) +
    theme(axis.text = element_text(size = 11, family = "Times"),
          axis.title = element_text(size = 12, family = "Times"),
          plot.title = element_text(size = 15, family = "Times", face = "bold",
                                    hjust = 0.5),
          legend.position = "top")
ggsave("MVC injury and death trend.jpg", width = 25, height = 15, units = "cm")    

MVC_data %>% 
    group_by()
# 3. REPEATED OFFENDERS -----------------------------------------------

violation_individual <- violation_data %>% 
    filter(!is.na(Driving_License_No)) %>% 
    group_by(Driving_License_No) %>% 
    count() %>% 
    arrange(desc(n))

violation_individual %>% 
    filter(n >40)

cut_violation <- violation_individual %>% 
    mutate(cat_n = cut(n, c(0,5,10,15,20,25,30,35,40,45))) %>%
    group_by(cat_n) %>%
    summarise(num = n())

cut_violation[-(1:2),] %>% 
    ggplot(aes(x = cat_n, y = num)) +
    geom_bar(stat = 'identity', width = 0.7, fill = c("#EEB4B4"), alpha =0.7) +
    labs(x = "Repeated traffic rule violation",
         y = "Number of drivers") +
    theme_light() +
    my_theme

ggsave("repeated offence.jpg", width = 20, height = 15, units = "cm")

violation_individual$new_ref <- 1:nrow(violation_individual)


more_than_2_off <- violation_data %>% 
    filter(!is.na(Driving_License_No)) %>% 
    group_by(Driving_License_No, Offence_Name) %>% 
    count() %>% 
    arrange(desc(n)) %>%
    filter(n > 2)

more_than_2_off %>% 
    ungroup() %>% 
    select(Driving_License_No) %>% 
    unique() %>% 
    count()

# 4. PREDICTING MVC -------------------------------------------------------

MVC_data %>% 
    group_by(year(Time)) %>% 
    summarise(sum = sum(Frequency)) %>% 
    View()

MVC_data %>% 
    filter(year)
# 5. MVC 2015-2019 --------------------------------------------------------

MVC_data_original %>% 
    ggplot(aes(x = Time, y = no_MVC)) +
    geom_line(colour = c("#F08080"))+
    geom_point(colour = c("#B22222"), size = 2) +
    labs(x = "Year", 
         y = "No. of MVC") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
    theme_light() +
    my_theme

ggsave("MVC 2015-2019.jpg", width = 25, height = 15, units = "cm")

# 6. MVC vs population and MVC vs vehicle population ----------------------

MV_year <- read_csv("MV_year.csv")

population <- read_csv("Bhutan_population.csv")

population <- pivot_longer(population, -indicator, names_to = "Year", 
                           values_to = "ann_population") 

population <- population %>% 
    select(Year, ann_population) %>% 
    mutate(Year = as.numeric(Year))

veh_n_pop <- left_join(MV_year, population, by = "Year") %>% 
    mutate(ppl_to_veh = ann_population/MV)

veh_n_pop %>% 
    ggplot(aes(x = Year, y = ppl_to_veh)) +
    geom_line(color = c("#4682B4")) +
    # geom_point() +
    xlim(1997, 2019) +
    ylim(2, 41) +
    labs(x = "Year",
         y = "No. of people per vehicle") +
    theme_light() +
    my_theme

ggsave("Veh_pop_ratio.jpg", width = 20, height = 15, units = "cm")

MVC_2016_2019 <- MVC_data_original %>% 
    mutate(death = death_M + death_F) %>% 
    mutate(injury = injury_M + injury_F) %>% 
    select(Time, death, injury) %>% 
    group_by(year(Time)) %>% 
    summarise(tot_death = sum(death),
              tot_injury = sum(injury))

colnames(MVC_2016_2019) <- c("Year", "total_death", "total_injury")

MVC_2016_2019 <- left_join(MVC_2016_2019, veh_n_pop, by = "Year")

MVC_2016_2019 <- MVC_2016_2019 %>% 
    mutate(death_to_pop = total_death/ann_population*100000) %>% 
    mutate(death_to_veh = total_death/MV*10000)

MVC_2016_2019 %>% 
    ggplot(aes(x = Year)) +
    geom_line(aes(y = death_to_pop, 
                  color = "no. of death per 100,000 population")) +
    geom_line(aes(y = death_to_veh,  
                  color = "no. of death per 10,000 vehicles")) +
    labs(x = "Year",
         y = "No. of death due to road crash",
         color = "") +
    scale_color_manual(values = c("no. of death per 100,000 population" = "#4682B4",
                                  "no. of death per 10,000 vehicles" = "#B22222")) +
    theme_light() +
    my_theme

ggsave("death ratio.jpg", width = 25, height = 15, units = "cm")    



