library(tidyverse)
library(lubridate)
library(googlesheets4)

#---- Googlesheet ----
gsheet <- read_sheet("https://docs.google.com/spreadsheets/d/13GwXHaf5ds6_T1kQS6TMF0k-HKhgERsyUj-Wpr0FIik/edit#gid=0")
gsheet_trader <- gsheet %>%
    select(week_start, trader_has, trader_name, trader_location, trader_zone, trader_tier, sales_total)

gsheet_trader$week_start <- as.Date(gsheet_trader$week_start)


#---- Sales data imported ATT ----

df <- read_csv("datasets/all_sales_AA.csv") %>%
    select(-"...1") %>%
    filter(week_start < (as.Date(Sys.time()) - 7)) # exclude current week (partial data)

#df$week_start <- mdy(df$week_start) #Had formating issues recently... TO DELETE



#---- Weekly sales ----

df_sales_weekly <- df %>% 
    select(sellerName, price, week_start) %>%
    group_by(sellerName, week_start) %>%
    summarize(total_sales = sum(price), )
df_sales_weekly

df_metrics_weekly <- df_sales_weekly %>%
    group_by(week_start) %>%
    summarise(
        guild = sum(total_sales),
        over500k = sum(total_sales>500000),
        over100k = sum(total_sales>100000),
        soldAny = n(),
        median = median(total_sales)
    )
df_metrics_weekly


#---- Sales in time ----

df_sales_6h <- df %>%
    mutate(time_6h = floor_date(dateTime, unit = "6 hour")) %>%
    select(time_6h, price, week_start, internal) %>%
    group_by(time_6h, week_start) %>%
    summarise(sales_gold = sum(price),
              sales_n = n(),
              sales_internal = mean(internal))
df_sales_6h

#---- Save data ----
write_rds(gsheet_trader, "dashboard/gsheet_trader_history.rds")

write_rds(df_sales_weekly, "dashboard/data_sales_weekly.rds")
write_rds(df_metrics_weekly, "dashboard/data_metrics_weekly.rds")
write_rds(df_sales_6h, "dashboard/data_sales_6h.rds")

#---- if need to read data... ----
# gsheet_trader <- read_rds("gsheet_trader_history.rds")
# df_sales_weekly <- read_rds("data_sales_weekly.rds")
# df_metrics_weekly <- read_rds("data_metrics_weekly.rds")
# df_sales_6h <- read_rds("data_sales_6h.rds")





