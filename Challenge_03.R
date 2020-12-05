library(vroom)
library(data.table)
library(tidyverse)
library(readr)
library(base)
# import of patent.tsv ----
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_tbl

#oper1 <- patent_tbl[,c("number","country","date","kind")]
#oper1

#import of assignee.tsv ----
col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
assignee_tbl


#import of patent_assignee.tsv ----
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl


# first task: which companies have the most patents ----

setnames(assignee_tbl, "id", "assignee_id")

combined_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
combined_data
class(combined_data)


combined_data <- combined_data %>% 
    filter(type == 2)%>%
    filter(!is.na(patent_id)) %>%
    count(organization, sort = T) %>%
    slice(1:10)
    #head(10)


# second task ----

combined_data1 <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

setnames(patent_tbl, "id", "patent_id")

combined_data2 <- merge(x = combined_data1,y = patent_tbl,
                        by = "patent_id",
                        all.x = TRUE, 
                        all.y = FALSE)

glimpse(combined_data2) 

combined_data2 <- combined_data2 %>% 
    filter(type.x == 2)%>%
    filter(year(date)==2019)%>%
    filter(!is.na(patent_id)) %>%
    count(organization, sort = T) %>%
    slice(1:10)
    #head(10)

# third task ----

#import of uspc.tsv
col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
uspc_tbl

combined_data3 <- merge(x = combined_data1,y = uspc_tbl,
                        by = "patent_id",
                        all.x = TRUE, 
                        all.y = FALSE)

combined_data3

#gibt top 10 firmen weltweit aus
combined_data1 %>% 
  filter(!is.na(patent_id)) %>%
  filter(!is.na(organization)) %>%
  count(organization, sort = T) %>%
  slice(1:10)


#Get the top 5 USPTO tech main classes, their patents are assigned to
  
#sorry dass ich keine idee hatte das ergebnis automatisch in einen vector zu überführen. Hätte es auch lieber anders gemacht...
filter_list <- c("International Business Machines Corporation","Samsung Electronics Co., Ltd.","Canon Kabushiki Kaisha",
                 "Sony Corporation","Kabushiki Kaisha Toshiba","General Electric Company","Hitachi, Ltd.",
                 "Intel Corporation","Fujitsu Limited","Hewlett-Packard Development Company, L.P.")

combined_data4 <- subset(combined_data3, organization %in% filter_list) 

combined_data4 <- combined_data4%>% 
    filter(!is.na(mainclass_id)) %>%
    count(mainclass_id, sort = T) %>%
    #slice(1:5)
    head(5)

saveRDS(combined_data,"result_3_1.rds")
saveRDS(combined_data2,"result_3_2.rds")
saveRDS(combined_data4,"result_3_3.rds")
