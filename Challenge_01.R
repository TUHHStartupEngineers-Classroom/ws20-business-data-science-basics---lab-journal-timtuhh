# 1.0 Load libraries ----
library(tidyverse)
library(readxl)


# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
orderlines_tbl %>%
  head(n = 5)

glimpse(orderlines_tbl)

bikes_tbl %>%
  head(n = 5)


# 4.0 Joining Data ----

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

glimpse(bike_orderlines_joined_tbl)


# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

glimpse(bike_orderlines_joined_tbl)

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  #split up a column in 2 different ones
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  #add ne column with calculation
  mutate(total.price = price * quantity)


glimpse(bike_orderlines_wrangled_tbl)

# prepare for plot 1 ----

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  #select only the two important columns
  select(state, total.price) %>%
  group_by(state) %>%
  #aufaddieren der sales je state
  summarize(sales = sum(total.price)) %>%
  #adding a currency column
  #mutate(sales_text = scales::dollar(sales))
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

glimpse(sales_by_state_tbl)

# plotting plot 1 ----

sales_by_state_tbl %>%
  #creating a canvas
  ggplot(aes(x = state, y = sales)) +
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# preparing for plot 2 ----

library(lubridate)

sales_by_state_and_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order.date, total.price, state) %>%
  mutate(year = year(order.date)) %>%
  group_by(state, year) %>%
  summarise(sales = sum(total.price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

glimpse(sales_by_state_and_year_tbl)

# plotting plot 2 ----

sales_by_state_and_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state and year",
    subtitle = "A comparison between the states",
    fill = "Main category" # Changes the legend name
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


