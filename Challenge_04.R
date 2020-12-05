library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

glimpse(covid_data_tbl)

filter_list <- c("Germany","United_Kingdom","France","Spain","United_States_of_America")

country_data_0 <- subset(covid_data_tbl, countriesAndTerritories %in% filter_list) 

glimpse(country_data_0)

country_data_1 <- country_data_0%>%
    select(countriesAndTerritories,cases,month,year,dateRep) %>%
    mutate(date = lubridate::dmy(dateRep)) %>%
    arrange(date) %>%
    group_by(countriesAndTerritories) %>%
    mutate(case_sum = cumsum(cases)) %>%
    ungroup() 


# plotting challenge 4.1
country_data_1 %>%
  ggplot() +
  geom_line(aes(x     = date,
                y     = case_sum,
                color = countriesAndTerritories)) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 12/02/2020",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Continent / Country") +
  scale_color_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = " M",prefix=""))+
  scale_x_continuous(labels = c("January","February","March","April","May","June","July","August",
    "September","October","November","December"),breaks = seq(as.Date("2020/1/1"), by = "month", length.out = 12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom")
  

#Challenge 4.2
country_data_3 <- covid_data_tbl %>%
  select(countriesAndTerritories,deaths,popData2019) %>%
  mutate(across(countriesAndTerritories, str_replace_all,
                "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  group_by(countriesAndTerritories,popData2019)  %>%
  summarize(overall_deaths = sum(deaths)) %>%
  mutate(mortality = overall_deaths / popData2019) %>%
  rename(region = countriesAndTerritories)

world <- map_data("world")

combined_covid_data <- merge(x = world, y = country_data_3, 
        by    = "region", 
        all.x = TRUE, 
        all.y = FALSE)

maximum <- max(combined_covid_data$mortality, na.rm = TRUE)
combined_covid_data %>% 
  ggplot() +
  geom_map(aes(x=long,y=lat,map_id=region, fill = mortality),map = world)+
  scale_fill_gradient(low="#ffcccb", high="#940008",labels = percent,limits = c(0,maximum),breaks=c(0, 0.0005, 0.001,maximum)) +
  theme(axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank())+
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    fill = "Mortality Rate",
    caption = str_glue("Date: 12/02/2020"))
