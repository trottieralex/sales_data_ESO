library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

gsheet_trader <- read_rds("gsheet_trader_history.rds")
df_sales_weekly <- read_rds("data_sales_weekly.rds")
df_metrics_weekly <- read_rds("data_metrics_weekly.rds")
df_sales_6h <- read_rds("data_sales_6h.rds")


ui <- dashboardPage(
    dashboardHeader(title = "Anrchy Alliance"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Info", tabName = "info", icon = icon("info")),
            menuItem("Week Summary", tabName = "week_summary", icon = icon("calendar-week")),
            menuItem("Trading History", tabName = "history", icon = icon("chart-line")),
            menuItem("Individual Stats", tabName = "individual", icon = icon("user"))
        )
    ),
    dashboardBody(
        tabItems(
            # Info tab
            tabItem(tabName = "info",
                    h1("Hey,"),
                    h3("Welcome to the first version of this dashboard!", br(), 
                       "It shows the sales of Anrchy Aliance (ESO, server PC-EU)"),
                    br(),
                    p("This is very much a work in progress... some section are probably slow and/or not optimized", br(),
                      "Just", strong("choose a tab on the left"), "to access different section.", br(),
                      "Please,",  strong("close the tab"), "once you are done...", br(), 
                      "and Let me know if you have any questions or comments!", br(), 
                      ":)")
            ), 
            
            # Numbers of the week tab
            tabItem(tabName = "week_summary",
                    h1("Week in Numbers"),
                    fluidRow(
                        box(
                            selectInput("week_selector",
                                        label = "For the week starting:",
                                        choices = sort(unique(df_sales_weekly$week_start), decreasing = TRUE))
                        ),
                        box(htmlOutput("trader_info"))
                    ),
                    fluidRow(
                        box(tableOutput("top_traders"), title = "Top traders"),
                        box(verbatimTextOutput("metrics"))
                    ),
                    fluidRow(
                        box(plotOutput("sales_6h_gold")),
                        box(plotOutput("sales_6h_n"))
                    )
            ),
            
            
            # Long history tab
            tabItem(tabName = "history",
                    h1("Trading History"),
                    p("Hover the bars to see details,", br(),
                      "(sometime slow to load)"),
                    plotlyOutput("trading_barplot", height = "600px")
            ),
            
            # personal history tab
            tabItem(tabName = "individual",
                    h1("Individual statistics"),
                    p("basic numbers, since March 8th only... (more to come!)"),
                    selectInput("member_selector",
                                label = "Want to see how you do? Select/type your name!",
                                choices = unique(df_sales_weekly$sellerName),
                                selected = "@trotalex"),
                    plotOutput("sales_individual")
            )
        )
    )
)

server <- function(input, output) {
    
    output$trader_info <- renderUI({
        trader <- gsheet_trader %>%
            filter(week_start == input$week_selector)
        
        if(trader$trader_has == 1){
            HTML(paste("<b>Our guild trader was:</b>", "<br>",
                       '<p style="text-align:center; font-size:125%">',
                       paste('<b style="color:blue;">', 
                             trader$trader_name, "in", trader$trader_location, ",<br>",
                             '</b>'),
                       paste('<b style="color:darkorange;">',
                             trader$trader_zone,
                             '</b>'),
                       '</p>'
            )
            )
        } else if(trader$trader_has == 0){
            HTML("<b> No trader </b>")
        } else {
            HTML("<b> Error! </b>")
        }
        
    })
    
    output$top_traders <- renderTable({
        
        top_table <- df_sales_weekly %>%
            filter(week_start == input$week_selector) %>%
            filter(total_sales >= 1000000) %>%
            select(sellerName, total_sales) %>%
            arrange(desc(total_sales))
        
        if(nrow(top_table) < 3){
            top_table <- df_sales_weekly %>%
                filter(week_start == input$week_selector) %>%
                select(sellerName, total_sales) %>%
                arrange(desc(total_sales))
            
            top_table <- top_table[1:3, ]
        }
        
        top_table$total_sales <- format(top_table$total_sales,
                                        digits = NULL,
                                        big.mark = ",",
                                        scientific = FALSE)
        colnames(top_table) <- c("", "Sales (gold)")
        top_table
    })
    
    output$metrics <- renderText({
        dd_week_metric <- df_metrics_weekly %>%
            filter(week_start == input$week_selector)
        
        paste0("Total guild sales: ", format(dd_week_metric$guild, big.mark = ",", scientific = FALSE) , " gold \n",
               dd_week_metric$over500k, " traders sold > 500k \n",
               dd_week_metric$over100k, " traders sold > 100k \n",
               dd_week_metric$soldAny, " of us sold something \n",
               "Median amount sold: ", format(dd_week_metric$median, big.mark = ",", scientific = FALSE, ), " gold"
        )
    })
    
    output$sales_6h_gold <- renderPlot({
        dd_week <- df_sales_6h %>%
            filter(week_start == input$week_selector)
        
        weekend <- range(dd_week$time_6h[weekdays(dd_week$time_6h) %in% c("Sunday", "Saturday")])
        
        ggplot(data = dd_week, aes(x = time_6h, y = sales_gold)) +
            geom_line(stat = "identity", size = 1, color = "darkorange") +
            geom_point(size = 3, shape = 15, color = "darkorange3") +
            geom_rect(aes(xmin = weekend[1], xmax = weekend[2]),
                      ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.005) +
            scale_y_continuous(name = "Gold", 
                               labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
            scale_x_datetime(name = NULL) +
            ggtitle("GOLD FROM sales during the week", subtitle = "each point represent 6 hours") +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.line = element_line(colour = "black"),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, color = "darkgrey"))
    })
    
    output$sales_6h_n <- renderPlot({
        dd_week <- df_sales_6h %>%
            filter(week_start == input$week_selector)
        
        weekend <- range(dd_week$time_6h[weekdays(dd_week$time_6h) %in% c("Sunday", "Saturday")])
        ggplot(dd_week, aes(x = time_6h, y = sales_n)) + 
            geom_area(fill = "orange", alpha = 0.75) +
            geom_rect(aes(xmin = weekend[1], xmax = weekend[2]),
                      ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.005) +
            scale_y_continuous(name = "Number of Sales",
                               expand = c(0,0)) + 
            scale_x_datetime(name = NULL,
                             expand = c(0,0)) +
            ggtitle("NUMBER OF sales during the week", subtitle = "each point represent 6 hours") +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.line = element_line(colour = "black"),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, color = "darkgrey"))
        
    })
    
    output$trading_barplot <- renderPlotly({
        plot_ly(
            x = gsheet_trader$week_start,
            y = gsheet_trader$sales_total*1000,
            type = "bar",
            text = paste(gsheet_trader$trader_location, ",", gsheet_trader$trader_zone),
            color = gsheet_trader$trader_tier,
            colors = c("#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#636363") 
        ) %>%
            layout(legend=list(title=list(text='<b> Trader Tier </b> <br> (++subjective)')))
    })
    
    output$sales_individual <- renderPlot({
        dd_seller <- df_sales_weekly %>%
            filter(sellerName == input$member_selector)
        
        medline <- round(median(dd_seller$total_sales[dd_seller$total_sales > 0]))
        
        ggplot(data = dd_seller, 
               aes(x = week_start, y = total_sales)) +
            geom_line(stat = "identity", size = 1, color = "darkorange") +
            geom_point(size = 3, shape = 15, color = "darkorange3") +
            scale_y_continuous(name = "Gold", 
                               labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
            scale_x_date(name = NULL,
                         date_breaks = "2 week",
                         limits = c(min(dd_seller$week_start)-7, max(dd_seller$week_start)+14 ),
                         expand = c(0,0)) + 
            ggtitle("Sales per week of:", subtitle = dd_seller$sellerName[1]) +
            geom_hline(yintercept = medline,
                       linetype="dashed", 
                       color = "deepskyblue4",
                       size = 1.2) + 
            geom_text(aes( x = max(dd_seller$week_start), 
                           y = medline, 
                           label = paste("Median: \n", format(medline, big.mark = ",", scientific = FALSE)),
                           vjust = 0.5,
                           hjust = -0.25),
                      colour = "deepskyblue4") +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.line = element_line(colour = "black"),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, color = "deepskyblue", size = 18),
                  plot.margin = unit(c(10,100,10,10), "pt")
            )
        
    })
}

shinyApp(ui, server)