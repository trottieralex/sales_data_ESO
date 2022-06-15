library(tidyverse)
library(lubridate)






#---- Prep for extra ----
df <- read_csv("datasets/all_sales_AA.csv") %>%
    select(-"...1") %>%
    filter(week_start < (as.Date(Sys.time()) - 7)) # exclude current week (partial data)

dd <- df %>% 
    filter(week_start == max(df$week_start)) %>%
    select(dateTime, internal, quantity, price)

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
    
    

