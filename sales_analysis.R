# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

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

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  #split up a column in 3 different ones
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  #add ne column with calculation
  mutate(total.price = price * quantity) %>%
  # delete the 2 columns
  select(-...1, -gender) %>%
  #delete all columns which end with .id
  select(-ends_with(".id")) %>%
  #bring back order.id column
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  #reorder the coulumns
  select(order.id, contains("order"), contains("model"), contains("category"),
       price, quantity, total.price,
       everything())%>%
  #rename newname = oldname
  rename(bikeshop = name) %>%
  #dot in names is used to pass the data to another part of the function, then change dots to underlines
  set_names(names(.) %>% str_replace_all("\\.", "_"))

glimpse(bike_orderlines_wrangled_tbl)

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
#install.packages("lubridate")
library(lubridate)

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  #select only the two important columns
  select(order_date, total_price) %>%
  #extract the year in a new column
  mutate(year = year(order_date)) %>%
  #nach jahr gruppieren
  group_by(year) %>%
  #aufaddieren der sales je jahr
  summarize(sales = sum(total_price)) %>%
  #adding a currency column
  #mutate(sales_text = scales::dollar(sales))
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " ")) #hat nicht funktioniert da encoding noch definiert werden m[sste
  
glimpse(sales_by_year_tbl)

# Step 2 - Visualize

sales_by_year_tbl %>%
  #creating a canvas
  ggplot(aes(x = year, y = sales)) +
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " â¬")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
  

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €"))

glimpse(sales_by_year_cat_1_tbl)

# Step 2 - Visualize

sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----

install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")