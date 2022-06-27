library(tidyverse)
library(lubridate)





# Snippet for week start
# current_week <- case_when(
#     (wday(now("UTC")) == 3) & (hour(now("UTC")) >= 14) ~ (date(now("UTC"))),
#     (wday(now("UTC")) <= 3) ~ (date(now("UTC")) - wday(now("UTC")) - 4),
#     TRUE ~ (date(now("UTC")) - wday(now("UTC")) + 3)
# )


paste(
    "We got a trader @everyone",
    "XXX in XXX",
    "",
    "Last week top sellers: 👑 (>1M)",
    "top here!!!",
    "",
    "Total Guild Metrics: 💰 📈 ",
    "Total guild sales: ", format(dd_week_metric$guild, big.mark = ",", scientific = FALSE) , " :coin:",
    dd_week_metric$over500k, " traders sold > 500k",
    dd_week_metric$over100k, " traders sold > 100k",
    dd_week_metric$soldAny, " of us sold something",
    sep = "\n"
)



#---- Prep for extra ----
df <- read_csv("datasets/all_sales_AA.csv") %>%
    select(-"...1") %>%
    filter(week_start < (as.Date(Sys.time()) - 7)) # exclude current week (partial data)

dd <- df %>% 
    filter(week_start == max(df$week_start))
dd




#---- Extra: 2022-05-31 ----
dd$group <- case_when(
    dd$internal == 0 ~ "External",
    dd$internal == 1 ~ "Internal"
)
dd$group <- factor(dd$group, levels = c("Internal", "External"))



ggplot(dd, aes(x = dateTime, fill = group)) +
    geom_histogram(bins = 28, alpha =0.75) +
    theme_bw() +
    ggtitle("Our sales during last week") + 
    scale_y_continuous(name = "Number of Sales",
                       expand = c(0,0),
                       limits = c(0, 201),
                       minor_breaks = 50) +
    scale_x_datetime(name = NULL,
                     expand = c(0,0),
                     breaks = "day",
                     date_labels = "%A") +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "#440381", size = 1.15),
          axis.title = element_text(colour = "#440381"),
          plot.title = element_text(hjust = 0.5, colour = "#440381"),
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(colour = "#440381")) +
    scale_fill_manual(values = c("#EE8434", "#60D394"))
    
    


#---- Extra: 2022-06-07 ----
library(treemapify)

week <- dd %>%
    select(sellerName, price) %>%
    group_by(sellerName) %>%
    summarize(sales = sum(price)) %>%
    arrange(desc(sales)) %>%
    mutate(sold = case_when(
        sales >= 1000000 ~ "1M+",
        sales >= 500000 ~ "500k+",
        sales >= 100000 ~ "100k+",
        sales > 0 ~ "any"))

week$sold <- factor(week$sold, levels = c("1M+", "500k+", "100k+", "any"))

ggplot(week, aes(fill = sold, 
           area = sales, 
           label = sellerName)) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre") +
    scale_fill_brewer(palette = "Dark2") +
    ggtitle("Sales June 7 to 14", subtitle = "Area proportional to sales in GOLD") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))



#---- Extra: 2022-06-14 ----

df_sales_6h <- read_rds("dashboard/data_sales_6h.rds")

dd <- df_sales_3h %>%
    filter(week_start == max(df_sales_6h$week_start))


dd$day <- wday(dd$time_3h,
               week_start = 2,
               abbr = FALSE,
               label = TRUE)
### 1st version bellow... but wday from lubridate much better!
# dd$day <- factor(weekdays(dd$time_3h),
#                  levels = c("Wednesday", "Thursday",
#                             "Friday",
#                             "Saturday", "Sunday",
#                             "Monday", "Tuesday"))
dd$hour <- hour(dd$time_3h)
dd <- dd %>% 
    filter(day != "Tuesday")
dd$day

ggplot(dd, aes(x = hour, y = sales_n, group = day, color = day)) +
    geom_line(size = 1.25) +
    geom_point() +
    theme_bw() +
    ggtitle("When do we sell things?", subtitle = "each point = 3 hours after") + 
    scale_x_continuous(name = "Hour of the day (GMT)", 
                       expand = c(0, 0),
                       breaks = c(0, 6, 12, 18, 24)) +
    scale_y_continuous(name = "Number of sales", 
                       expand = c(0, 0),
                       limits = c(0, 300)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "darkgrey", size = 1.17),
          axis.title = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank()
          ) +
    scale_color_manual(values = c("cornflowerblue", "dodgerblue", "purple", "tomato1", "tomato3", "royalblue"))

# Without Friday

dd <- dd %>% 
    filter(day != "Friday")

ggplot(dd, aes(x = hour, y = sales_n, group = day, color = day)) +
    geom_line(size = 1.25) +
    geom_point() +
    theme_bw() +
    ggtitle("When do we sell things?", subtitle = "each point = 3 hours after") + 
    scale_x_continuous(name = "Hour of the day (GMT)", 
                       expand = c(0, 0),
                       breaks = c(0, 6, 12, 18, 24)) +
    scale_y_continuous(name = "Number of sales", 
                       expand = c(0, 0),
                       limits = c(0, 175)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "darkgrey", size = 1.17),
          axis.title = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank()
    ) +
    scale_color_manual(values = c("cornflowerblue", "dodgerblue", "tomato1", "tomato3", "royalblue"))




# Check compare to average
dd$sales_n_percdiff_mean <- (dd$sales_n / mean(dd$sales_n) * 100) - 100

summary(dd)

ggplot(dd, aes(x = hour, y = sales_n_percdiff_mean, group = day, color = day)) +
    geom_line(size = 1.25) +
    geom_point() +
    theme_bw() +
    ggtitle("When do we sell things?", subtitle = "each point = 3 hours after") + 
    scale_x_continuous(name = "Hour of the day (GMT)", 
                       expand = c(0, 0),
                       breaks = c(0, 6, 12, 18, 24)) +
    scale_y_continuous(name = "Number of sales", 
                       expand = c(0, 0),
                       limits = c(0, 300)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "darkgrey", size = 1.17),
          axis.title = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank()
    ) +
    scale_color_manual(values = c("cornflowerblue", "dodgerblue", "purple", "tomato1", "tomato3", "royalblue"))


# Friday and Tuesday
dd <- read_rds("dashboard/data_sales_6h.rds")

dd$day <- wday(dd$time_6h, week_start = 2, label = TRUE)

ds <- dd %>%
    select(-sales_internal, -sales_gold) %>%
    filter(week_start != as.Date("2022-04-19")) %>%
    group_by(week_start, day) %>%
    summarise(sales = sum(sales_n)) %>% 
    pivot_wider(names_from = day, values_from = sales)

ds$dayweek <- round(rowSums(ds[ ,c("Wed", "Thu", "Mon")]) / 3)

 
for(ii in names(ds)[2:8]){
    ds[ii] <- ds[ii] / ds$dayweek * 100
}

dc <- ds %>%
    select(-dayweek) %>%
    pivot_longer(
        cols = c(names(ds)[2:8]),
        names_to = "day_of_week",
        values_to = "percent_sales_weekdays"
    )
dc$day_of_week <- factor(dc$day_of_week, 
                         levels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon"))


dc %>%
    ggplot(aes(x = day_of_week, y = percent_sales_weekdays, group = week_start)) + 
    geom_point() +
    geom_hline(yintercept = 100, colour = "red")
ds
