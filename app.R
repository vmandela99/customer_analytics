# Required libraries
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)       # For data manipulation functions like summarize
library(scales)      # For dollar_format and other formatting functions
library(lubridate)   # For floor_date and other date functions
library(ggplot2)     # For ggplot visualizations
library(shinylive)

# UI Definition
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Customer Analytics", titleWidth = 500),
    
    # Sidebar with navigation menu
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Customer Activity", tabName = "activity", icon = icon("user-clock")),
            menuItem("Purchase Behavior", tabName = "purchases", icon = icon("shopping-cart")),
            menuItem("Demographics", tabName = "demographics", icon = icon("users")),
            menuItem("Product Analysis", tabName = "products", icon = icon("box")),
            menuItem("Raw Data", tabName = "data", icon = icon("table"))
        )
    ),
    
    # Dashboard body with all panels
    dashboardBody(
        tags$head(
            tags$style(HTML("
        .box {border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);}
        .small-box {border-radius: 5px;}
        .content-wrapper {background-color: #f9f9f9;}
      "))
        ),
        
        tabItems(
            # Overview Tab
            tabItem(tabName = "overview",
                    fluidRow(
                        valueBoxOutput("total_sales_box", width = 3),
                        valueBoxOutput("conversion_rate_box", width = 3),
                        valueBoxOutput("aov_box", width = 3),
                        valueBoxOutput("active_customers_box", width = 3)
                    ),
                    fluidRow(
                        box(title = "Sales Trend", status = "primary", solidHeader = TRUE,
                            plotlyOutput("sales_trend", height = 300), width = 8),
                        box(title = "Sales by Channel", status = "primary", solidHeader = TRUE,
                            plotlyOutput("sales_by_channel", height = 300), width = 4)
                    ),
                    fluidRow(
                        box(title = "Top Products", status = "info", solidHeader = TRUE,
                            plotlyOutput("top_products", height = 300), width = 6),
                        box(title = "Customer Segments", status = "info", solidHeader = TRUE,
                            plotlyOutput("customer_segments", height = 300), width = 6)
                    )
            ),
            
            # Customer Activity Tab
            tabItem(tabName = "activity",
                    fluidRow(
                        box(title = "Site Traffic", status = "primary", solidHeader = TRUE,
                            plotlyOutput("site_traffic", height = 250), width = 8),
                        box(title = "Engagement Metrics", status = "primary", solidHeader = TRUE,
                            plotlyOutput("engagement_metrics", height = 250), width = 4)
                    ),
                    fluidRow(
                        box(title = "User Journey", status = "info", solidHeader = TRUE,
                            plotlyOutput("user_journey", height = 300), width = 6),
                        box(title = "Abandoned Cart Analysis", status = "warning", solidHeader = TRUE,
                            plotlyOutput("abandoned_cart", height = 300), width = 6)
                    ),
                    fluidRow(
                        box(title = "Time on Page by Category", status = "success", solidHeader = TRUE,
                            plotlyOutput("time_on_page", height = 250), width = 12)
                    )
            ),
            
            # Purchase Behavior Tab
            tabItem(tabName = "purchases",
                    fluidRow(
                        box(title = "Purchase Frequency", status = "primary", solidHeader = TRUE,
                            plotlyOutput("purchase_frequency", height = 300), width = 6),
                        box(title = "Average Order Value Over Time", status = "primary", solidHeader = TRUE,
                            plotlyOutput("aov_time", height = 300), width = 6)
                    ),
                    fluidRow(
                        box(title = "Customer Repurchase Rate", status = "info", solidHeader = TRUE,
                            plotlyOutput("repurchase_rate", height = 250), width = 4),
                        box(title = "Popular Product Combinations", status = "info", solidHeader = TRUE,
                            plotlyOutput("product_combinations", height = 250), width = 4),
                        box(title = "Purchase Time Heatmap", status = "info", solidHeader = TRUE,
                            plotlyOutput("purchase_heatmap", height = 250), width = 4)
                    ),
                    fluidRow(
                        box(title = "Customer Lifetime Value Distribution", status = "success", solidHeader = TRUE,
                            plotlyOutput("clv_distribution", height = 250), width = 12)
                    )
            ),
            
            # Demographics Tab
            tabItem(tabName = "demographics",
                    fluidRow(
                        box(title = "Age Distribution", status = "primary", solidHeader = TRUE,
                            plotlyOutput("age_distribution", height = 300), width = 6),
                        box(title = "Gender Breakdown", status = "primary", solidHeader = TRUE,
                            plotlyOutput("gender_breakdown", height = 300), width = 6)
                    ),
                    fluidRow(
                        box(title = "Geographic Distribution", status = "info", solidHeader = TRUE,
                            leafletOutput("geo_map", height = 400), width = 8),
                        box(title = "Device Usage", status = "info", solidHeader = TRUE,
                            plotlyOutput("device_usage", height = 400), width = 4)
                    ),
                    fluidRow(
                        box(title = "Customer Segmentation", status = "success", solidHeader = TRUE,
                            plotlyOutput("customer_segmentation", height = 300), width = 12)
                    )
            ),
            
            # Product Analysis Tab
            tabItem(tabName = "products",
                    fluidRow(
                        box(title = "Product Category Performance", status = "primary", solidHeader = TRUE,
                            plotlyOutput("category_performance", height = 300), width = 12)
                    ),
                    fluidRow(
                        box(title = "Product View to Purchase Ratio", status = "info", solidHeader = TRUE,
                            plotlyOutput("view_purchase_ratio", height = 300), width = 6),
                        box(title = "Product Return Rates", status = "warning", solidHeader = TRUE,
                            plotlyOutput("return_rates", height = 300), width = 6)
                    ),
                    fluidRow(
                        box(title = "Seasonal Product Trends", status = "success", solidHeader = TRUE,
                            plotlyOutput("seasonal_trends", height = 300), width = 12)
                    )
            ),
            
            # Raw Data Tab
            tabItem(tabName = "data",
                    fluidRow(
                        box(title = "Filter Data", status = "primary", solidHeader = TRUE, width = 12,
                            column(4, dateRangeInput("date_range", "Date Range", 
                                                     start = Sys.Date() - 365, end = Sys.Date())),
                            column(4, selectInput("data_category", "Data Category", 
                                                  choices = c("Transactions", "Customer Profiles", "Product Views", "Cart Activity"))),
                            column(4, downloadButton("download_data", "Download Selected Data"))
                        )
                    ),
                    fluidRow(
                        box(title = "Raw Data", status = "primary", solidHeader = TRUE, width = 12,
                            DTOutput("raw_data_table"))
                    )
            )
        )
    )
)

# Server logic
server <- function(input, output){
    
    # Sample data generation (in a real application, you would connect to your database)
    set.seed(123)
    
    # Generate sample data - in a real scenario, you'd connect to your database
    sales_data <- data.frame(
        date = seq.Date(from = Sys.Date() - 365, to = Sys.Date(), by = "day"),
        sales = runif(366, 1000, 5000) + 
            sin(seq(0, 2*pi, length.out = 366)) * 2000 + 
            rep(c(0, 500, 1000, 1500, 2000), each = 73.2),
        transactions = round(runif(366, 50, 200) + 
                                 sin(seq(0, 2*pi, length.out = 366)) * 100),
        visitors = round(runif(366, 500, 2000) + 
                             sin(seq(0, 2*pi, length.out = 366)) * 500),
        new_customers = round(runif(366, 20, 100))
    )
    
    # Add monthly pattern
    monthly_effect <- rep(c(1, 1.1, 0.9, 1.2, 1.3, 0.8, 1, 1.1, 1.4, 1.5, 2, 2.5), each = 30.5)
    sales_data$sales <- sales_data$sales * monthly_effect[1:366]
    
    # Add channel data
    channels <- c("Direct", "Organic Search", "Paid Search", "Social Media", "Email", "Referral")
    channel_data <- data.frame(
        channel = channels,
        sales = c(350000, 280000, 220000, 190000, 150000, 110000)
    )
    
    # Product data
    product_categories <- c("Electronics", "Clothing", "Home & Kitchen", "Books", "Beauty", "Sports", "Toys")
    product_data <- data.frame(
        category = product_categories,
        sales = c(420000, 380000, 290000, 180000, 210000, 150000, 120000),
        views = c(1200000, 950000, 720000, 650000, 480000, 420000, 380000),
        conversion_rate = c(8.2, 12.5, 10.3, 7.8, 11.2, 8.7, 9.1),
        return_rate = c(4.2, 8.7, 5.3, 1.8, 3.2, 4.7, 2.1)
    )
    
    # Customer segments
    customer_segments <- data.frame(
        segment = c("New", "Occasional", "Regular", "Loyal", "VIP"),
        count = c(12000, 8500, 6200, 3800, 1500)
    )
    
    # Demographic data
    age_groups <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    age_data <- data.frame(
        age_group = age_groups,
        count = c(4500, 8200, 7800, 5600, 3900, 2000)
    )
    
    gender_data <- data.frame(
        gender = c("Male", "Female", "Other"),
        count = c(15000, 16500, 500)
    )
    
    # Device data
    device_data <- data.frame(
        device = c("Desktop", "Mobile", "Tablet"),
        count = c(12000, 18000, 2000)
    )
    
    # Output definitions
    
    # Overview tab outputs
    output$total_sales_box <- renderValueBox({
        valueBox(
            paste0("$", format(sum(sales_data$sales), big.mark = ",", scientific = FALSE)),
            "Total Sales", icon = icon("dollar-sign"), color = "green"
        )
    })
    
    output$conversion_rate_box <- renderValueBox({
        conv_rate <- sum(sales_data$transactions) / sum(sales_data$visitors) * 100
        valueBox(
            paste0(round(conv_rate, 1), "%"),
            "Conversion Rate", icon = icon("percentage"), color = "yellow"
        )
    })
    
    output$aov_box <- renderValueBox({
        aov <- sum(sales_data$sales) / sum(sales_data$transactions)
        valueBox(
            paste0("$", round(aov, 2)),
            "Average Order Value", icon = icon("shopping-bag"), color = "blue"
        )
    })
    
    output$active_customers_box <- renderValueBox({
        valueBox(
            format(sum(sales_data$new_customers) + 15000, big.mark = ","),
            "Active Customers", icon = icon("users"), color = "purple"
        )
    })
    
    output$sales_trend <- renderPlotly({
        # Make sure to use consistent spelling
        monthly_sales <- sales_data %>%
            mutate(month = floor_date(date, "month")) %>%
            group_by(month) %>%
            summarize(sales = sum(sales))  # Changed from summarise to summarize if that's what's used in your environment
        
        p <- ggplot(monthly_sales, aes(x = month, y = sales)) +
            geom_line(color = "#3c8dbc", size = 1.2) +
            geom_point(color = "#3c8dbc", size = 3) +
            scale_y_continuous(labels = scales::dollar_format()) +  # Use explicit scales:: prefix
            labs(x = "", y = "Monthly Sales") +
            theme_minimal() +
            theme(
                axis.text = element_text(size = 10),
                plot.background = element_rect(fill = "white")
            )
        
        ggplotly(p)
    })
    
    output$sales_by_channel <- renderPlotly({
        p <- ggplot(channel_data, aes(x = reorder(channel, sales), y = sales, fill = channel)) +
            geom_bar(stat = "identity") +
            scale_y_continuous(labels = scales::dollar_format()) +  # Use explicit scales:: prefix
            coord_flip() +
            labs(x = "", y = "Sales") +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_fill_brewer(palette = "Blues")
        
        ggplotly(p)
    })
    
    output$top_products <- renderPlotly({
        p <- ggplot(product_data, aes(x = reorder(category, sales), y = sales, fill = sales)) +
            geom_bar(stat = "identity") +
            scale_y_continuous(labels = scales::dollar_format()) +  # Use explicit scales:: prefix 
            coord_flip() +
            labs(x = "", y = "Sales") +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_fill_gradient(low = "#56B4E9", high = "#0072B2")
        
        ggplotly(p)
    })
    
    output$customer_segments <- renderPlotly({
        # Create a fresh data frame right here
        segments_df <- data.frame(
            segment = c("New", "Occasional", "Regular", "Loyal", "VIP"),
            count = c(12000, 8500, 6200, 3800, 1500)
        )
        
        # Create a very simple plot first
        p <- plot_ly(segments_df, labels = ~segment, values = ~count, type = 'pie',
                     textinfo = 'label+percent',
                     insidetextorientation = 'radial')
        
        p <- p %>% layout(title = "Customer Segments")
        
        return(p)
    })
    # Customer Activity tab outputs
    output$site_traffic <- renderPlotly({
        p <- ggplot(sales_data, aes(x = date)) +
            geom_line(aes(y = visitors, color = "Visitors"), size = 1) +
            geom_line(aes(y = transactions, color = "Transactions"), size = 1) +
            scale_color_manual(values = c("Visitors" = "#3c8dbc", "Transactions" = "#00a65a")) +
            labs(x = "", y = "Count", color = "") +
            theme_minimal()
        
        ggplotly(p)
    })
    
    output$engagement_metrics <- renderPlotly({
        metrics <- data.frame(
            metric = c("Bounce Rate", "Pages/Session", "Avg. Session"),
            value = c(32, 4.5, 3.2),
            max = c(100, 10, 10)
        )
        
        p <- ggplot(metrics, aes(x = metric, y = value, fill = metric)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = ifelse(metric == "Bounce Rate", 
                                         paste0(value, "%"), 
                                         ifelse(metric == "Avg. Session", 
                                                paste0(value, " min"), 
                                                as.character(value)))), 
                      vjust = -0.5) +
            ylim(0, max(metrics$max) * 1.1) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "") +
            scale_fill_brewer(palette = "Set2")
        
        ggplotly(p)
    })
    
    output$user_journey <- renderPlotly({
        stages <- data.frame(
            stage = c("Home Page", "Category Page", "Product Page", "Add to Cart", "Checkout", "Purchase"),
            users = c(10000, 7500, 5000, 2000, 1500, 1200)
        )
        
        p <- plot_ly(stages, x = ~stage, y = ~users, type = "funnel",
                     marker = list(color = colorRampPalette(c("#3c8dbc", "#00a65a"))(6)))
        
        p
    })
    
    output$abandoned_cart <- renderPlotly({
        cart_data <- data.frame(
            reason = c("Price too high", "Just browsing", "Found better deal", 
                       "Shipping cost", "Payment issues", "Other"),
            percentage = c(28, 22, 18, 15, 10, 7)
        )
        
        p <- ggplot(cart_data, aes(x = reorder(reason, percentage), y = percentage, fill = percentage)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_gradient(low = "#f39c12", high = "#dd4b39") +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "Percentage (%)")
        
        ggplotly(p)
    })
    
    output$time_on_page <- renderPlotly({
        time_data <- data.frame(
            category = product_categories,
            avg_time = c(120, 95, 105, 150, 85, 75, 90)
        )
        
        p <- ggplot(time_data, aes(x = category, y = avg_time, fill = category)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(avg_time, "s")), vjust = -0.5) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "Avg. Time (seconds)") +
            scale_fill_brewer(palette = "Set3")
        
        ggplotly(p)
    })
    
    # Purchase Behavior tab outputs
    output$purchase_frequency <- renderPlotly({
        freq_data <- data.frame(
            frequency = c("One-time", "2-3 times", "4-5 times", "6-10 times", "11+ times"),
            customers = c(8500, 6200, 4100, 2500, 800)
        )
        
        p <- ggplot(freq_data, aes(x = frequency, y = customers, fill = frequency)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Purchase Frequency", y = "Number of Customers") +
            scale_fill_brewer(palette = "Blues")
        
        ggplotly(p)
    })
    
    output$aov_time <- renderPlotly({
        aov_time <- sales_data %>%
            mutate(month = floor_date(date, "month")) %>%
            group_by(month) %>%
            summarize(
                sales = sum(sales),
                transactions = sum(transactions),
                aov = sales / transactions
            )
        
        p <- ggplot(aov_time, aes(x = month, y = aov)) +
            geom_line(color = "#00a65a", size = 1.2) +
            geom_point(color = "#00a65a", size = 3) +
            scale_y_continuous(labels = dollar_format()) +
            labs(x = "", y = "Average Order Value") +
            theme_minimal()
        
        ggplotly(p)
    })
    
    output$repurchase_rate <- renderPlotly({
        p <- plot_ly(
            type = "indicator",
            mode = "gauge+number",
            value = 45,
            title = list(text = "Repurchase Rate (%)"),
            gauge = list(
                axis = list(range = list(0, 100)),
                bar = list(color = "#00a65a"),
                steps = list(
                    list(range = c(0, 30), color = "#f56954"),
                    list(range = c(30, 70), color = "#f39c12"),
                    list(range = c(70, 100), color = "#00a65a")
                )
            )
        )
        
        p
    })
    
    output$product_combinations <- renderPlotly({
        combo_data <- data.frame(
            combo = c("Electronics + Accessories", "Clothing + Shoes", "Books + Electronics", 
                      "Beauty + Clothing", "Home + Kitchen"),
            frequency = c(850, 720, 580, 460, 390)
        )
        
        p <- ggplot(combo_data, aes(x = reorder(combo, frequency), y = frequency, fill = frequency)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "Frequency") +
            scale_fill_gradient(low = "#3c8dbc", high = "#605ca8")
        
        ggplotly(p)
    })
    
    output$purchase_heatmap <- renderPlotly({
        # Create purchase time heatmap data
        days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        hours <- 0:23
        
        # Generate some realistic purchase pattern data
        set.seed(123)
        heatmap_data <- expand.grid(day = factor(days, levels = days), hour = hours)
        
        # Business hours have more purchases
        heatmap_data$value <- ifelse(heatmap_data$hour >= 9 & heatmap_data$hour <= 20, 
                                     runif(nrow(heatmap_data), 10, 100),
                                     runif(nrow(heatmap_data), 1, 30))
        
        # Weekend patterns are different
        heatmap_data$value <- ifelse(heatmap_data$day %in% c("Saturday", "Sunday") & 
                                         heatmap_data$hour >= 10 & heatmap_data$hour <= 18,
                                     heatmap_data$value * 1.5,
                                     heatmap_data$value)
        
        # Lunch hour boost
        heatmap_data$value <- ifelse(heatmap_data$hour >= 12 & heatmap_data$hour <= 13,
                                     heatmap_data$value * 1.3,
                                     heatmap_data$value)
        
        # Evening boost on weekdays
        heatmap_data$value <- ifelse(!(heatmap_data$day %in% c("Saturday", "Sunday")) & 
                                         heatmap_data$hour >= 18 & heatmap_data$hour <= 22,
                                     heatmap_data$value * 1.4,
                                     heatmap_data$value)
        
        p <- plot_ly(
            x = heatmap_data$hour,
            y = heatmap_data$day,
            z = heatmap_data$value,
            type = "heatmap",
            colorscale = "Blues"
        ) %>%
            layout(
                xaxis = list(title = "Hour of Day"),
                yaxis = list(title = "")
            )
        
        p
    })
    
    output$clv_distribution <- renderPlotly({
        # Create CLV data
        set.seed(123)
        clv_data <- data.frame(
            customer_id = 1:1000,
            clv = exp(rnorm(1000, 5, 1))  # Lognormal distribution for CLV
        )
        
        p <- ggplot(clv_data, aes(x = clv)) +
            geom_histogram(bins = 50, fill = "#00a65a", color = "white", alpha = 0.8) +
            scale_x_continuous(labels = dollar_format()) +
            labs(x = "Customer Lifetime Value", y = "Number of Customers") +
            theme_minimal()
        
        ggplotly(p)
    })
    
    # Demographics tab outputs
    output$age_distribution <- renderPlotly({
        p <- ggplot(age_data, aes(x = age_group, y = count, fill = age_group)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Age Group", y = "Number of Customers") +
            scale_fill_brewer(palette = "Blues")
        
        ggplotly(p)
    })
    
    output$gender_breakdown <- renderPlotly({
        p <- plot_ly(gender_data, labels = ~gender, values = ~count, type = 'pie',
                     marker = list(colors = c("#3c8dbc", "#00a65a", "#605ca8")))
        
        p
    })
    
    output$geo_map <- renderLeaflet({
        # Sample data for geographic distribution
        set.seed(123)
        n <- 500
        geo_data <- data.frame(
            lat = runif(n, 25, 49),  # US latitude range
            lng = runif(n, -125, -70),  # US longitude range
            customers = rpois(n, 50)  # Random customer counts
        )
        
        leaflet(geo_data) %>%
            addTiles() %>%
            addCircleMarkers(
                lng = ~lng,
                lat = ~lat,
                radius = ~sqrt(customers) * 1.5,
                color = "#3c8dbc",
                fillOpacity = 0.7,
                popup = ~paste("Customers:", customers)
            )
    })
    
    output$device_usage <- renderPlotly({
        p <- plot_ly(device_data, labels = ~device, values = ~count, type = 'pie',
                     marker = list(colors = c("#3c8dbc", "#00a65a", "#605ca8")))
        
        p
    })
    
    output$customer_segmentation <- renderPlotly({
        # Create RFM (Recency, Frequency, Monetary) segmentation
        set.seed(123)
        n <- 1000
        rfm_data <- data.frame(
            customer_id = 1:n,
            recency = runif(n, 1, 100),
            frequency = rlnorm(n, 1, 1),
            monetary = rlnorm(n, 4, 1)
        )
        
        p <- plot_ly(rfm_data, x = ~recency, y = ~frequency, z = ~monetary, 
                     marker = list(size = 5, opacity = 0.6),
                     type = "scatter3d", mode = "markers",
                     color = ~monetary, colorscale = "Blues")
        
        p
    })
    
    # Product Analysis tab outputs
    output$category_performance <- renderPlotly({
        p <- plot_ly(product_data) %>%
            add_trace(x = ~category, y = ~sales, type = 'bar', name = 'Sales',
                      marker = list(color = '#3c8dbc')) %>%
            add_trace(x = ~category, y = ~views / 10, type = 'bar', name = 'Views (÷10)',
                      marker = list(color = '#00a65a')) %>%
            layout(yaxis = list(title = 'Value'), barmode = 'group')
        
        p
    })
    
    output$view_purchase_ratio <- renderPlotly({
        p <- ggplot(product_data, aes(x = reorder(category, conversion_rate), 
                                      y = conversion_rate, fill = conversion_rate)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(conversion_rate, 1), "%")), hjust = -0.2) +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "Conversion Rate (%)") +
            scale_fill_gradient(low = "#ffffcc", high = "#3c8dbc") +
            ylim(0, max(product_data$conversion_rate) * 1.2)
        
        ggplotly(p)
    })
    
    output$return_rates <- renderPlotly({
        p <- ggplot(product_data, aes(x = reorder(category, -return_rate), 
                                      y = return_rate, fill = return_rate)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(return_rate, 1), "%")), vjust = -0.5) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "", y = "Return Rate (%)") +
            scale_fill_gradient(low = "#00a65a", high = "#dd4b39")
        
        ggplotly(p)
    })
    
    output$seasonal_trends <- renderPlotly({
        # Create seasonal product data
        months <- month.abb
        
        # Wrap the visualization in tryCatch to prevent dashboard crashes
        tryCatch({
            # Create seasonal factors for ALL categories
            seasonal_factors <- list(
                "Electronics" = c(0.7, 0.7, 0.8, 0.9, 1.0, 1.1, 0.9, 1.0, 1.2, 1.1, 1.5, 2.1),
                "Clothing" = c(0.9, 0.8, 1.1, 1.2, 1.3, 1.2, 0.8, 1.3, 1.4, 1.0, 1.1, 1.9),
                "Home & Kitchen" = c(1.0, 0.9, 1.1, 1.3, 1.2, 1.0, 0.9, 1.0, 1.1, 1.2, 1.3, 1.8),
                "Books" = c(0.8, 0.9, 1.0, 0.9, 1.1, 1.2, 1.5, 1.2, 1.1, 1.0, 1.1, 2.0),
                "Beauty" = c(0.9, 1.0, 1.1, 1.2, 1.3, 1.2, 1.0, 0.9, 1.1, 1.2, 1.3, 1.8),
                "Sports" = c(0.8, 0.7, 1.0, 1.2, 1.4, 1.5, 1.3, 1.2, 1.0, 0.9, 0.8, 1.2),
                "Toys" = c(0.7, 0.6, 0.7, 0.8, 0.9, 1.0, 0.9, 1.0, 1.1, 1.2, 1.5, 2.6)
            )
            
            # Check that all product categories have seasonal factors
            missing_categories <- setdiff(product_categories, names(seasonal_factors))
            if (length(missing_categories) > 0) {
                # For any missing categories, create default seasonal patterns
                for (cat in missing_categories) {
                    seasonal_factors[[cat]] <- rep(1, 12) # Default to no seasonality
                }
            }
            
            # Base sales for each category (with error checking)
            base_sales <- setNames(
                product_data$sales / 12, 
                product_data$category
            )
            
            # Calculate monthly sales for each category
            monthly_sales <- data.frame()
            for (cat in product_categories) {
                cat_base <- base_sales[cat]
                if (is.na(cat_base)) {
                    # Skip if category not found in base_sales
                    next
                }
                
                cat_seasonal <- data.frame(
                    month = factor(months, levels = months),
                    category = cat,
                    sales = cat_base * seasonal_factors[[cat]]
                )
                monthly_sales <- rbind(monthly_sales, cat_seasonal)
            }
            
            # Create the plotly visualization
            plot_ly(
                data = monthly_sales, 
                x = ~month, 
                y = ~sales, 
                color = ~category, 
                type = "scatter", 
                mode = "lines+markers"
            ) %>%
                layout(
                    title = "Seasonal Product Trends",
                    xaxis = list(title = "Month"),
                    yaxis = list(title = "Sales"),
                    legend = list(title = list(text = "Product Category"))
                )
            
        }, error = function(e) {
            # Return a meaningful error visualization instead of crashing
            plot_ly() %>%
                add_annotations(
                    text = paste("Error in seasonal trends chart:", e$message),
                    showarrow = FALSE,
                    font = list(color = 'red', size = 14)
                ) %>%
                layout(title = "Seasonal Product Trends (Error)")
        })
    })
    # Raw Data tab outputs
    output$raw_data_table <- renderDT({
        # Filter the data based on the user's selections
        filtered_data <- tryCatch({
            # In a real application, you would filter your actual data
            # Here we're just using the sample data for demonstration
            if(input$data_category == "Transactions") {
                sales_data %>%
                    filter(date >= input$date_range[1], date <= input$date_range[2])
            } else if(input$data_category == "Customer Profiles") {
                # Join some demographic data for demonstration
                data.frame(
                    customer_id = 1:20,
                    age_group = sample(age_groups, 20, replace = TRUE),
                    gender = sample(gender_data$gender, 20, replace = TRUE, prob = gender_data$count/sum(gender_data$count)),
                    segment = sample(customer_segments$segment, 20, replace = TRUE, prob = customer_segments$count/sum(customer_segments$count)),
                    total_purchases = rpois(20, 5),
                    avg_order_value = round(rlnorm(20, 4, 0.5), 2)
                )
            } else if(input$data_category == "Product Views") {
                data.frame(
                    product_id = 1:20,
                    category = sample(product_categories, 20, replace = TRUE),
                    views = round(rlnorm(20, 6, 1)),
                    unique_viewers = round(rlnorm(20, 5, 1)),
                    avg_view_time = round(rlnorm(20, 3, 0.5), 2)
                )
            } else { # Cart Activity
                data.frame(
                    cart_id = 1:20,
                    customer_id = sample(1:1000, 20),
                    items = rpois(20, 3),
                    total_value = round(rlnorm(20, 4, 0.7), 2),
                    status = sample(c("Completed", "Abandoned", "In Progress"), 20, 
                                    replace = TRUE, prob = c(0.6, 0.3, 0.1)),
                    created_at = sample(seq(input$date_range[1], input$date_range[2], by="day"), 20, replace = TRUE)
                )
            }
        }, error = function(e) {
            # Return empty data frame if there's an error
            data.frame(Error = paste("An error occurred:", e$message))
        })
        
        datatable(filtered_data, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')),
                  rownames = FALSE,
                  filter = 'top')
    })
    
    # Download handler for the data
    output$download_data <- downloadHandler(
        filename = function() {
            paste("ecommerce-data-", input$data_category, "-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            # Get the same data as displayed in the table
            if(input$data_category == "Transactions") {
                data_to_download <- sales_data %>%
                    filter(date >= input$date_range[1], date <= input$date_range[2])
            } else if(input$data_category == "Customer Profiles") {
                data_to_download <- data.frame(
                    customer_id = 1:20,
                    age_group = sample(age_groups, 20, replace = TRUE),
                    gender = sample(gender_data$gender, 20, replace = TRUE, prob = gender_data$count/sum(gender_data$count)),
                    segment = sample(customer_segments$segment, 20, replace = TRUE, prob = customer_segments$count/sum(customer_segments$count)),
                    total_purchases = rpois(20, 5),
                    avg_order_value = round(rlnorm(20, 4, 0.5), 2)
                )
            } else if(input$data_category == "Product Views") {
                data_to_download <- data.frame(
                    product_id = 1:20,
                    category = sample(product_categories, 20, replace = TRUE),
                    views = round(rlnorm(20, 6, 1)),
                    unique_viewers = round(rlnorm(20, 5, 1)),
                    avg_view_time = round(rlnorm(20, 3, 0.5), 2)
                )
            } else { # Cart Activity
                data_to_download <- data.frame(
                    cart_id = 1:20,
                    customer_id = sample(1:1000, 20),
                    items = rpois(20, 3),
                    total_value = round(rlnorm(20, 4, 0.7), 2),
                    status = sample(c("Completed", "Abandoned", "In Progress"), 20, 
                                    replace = TRUE, prob = c(0.6, 0.3, 0.1)),
                    created_at = sample(seq(input$date_range[1], input$date_range[2], by="day"), 20, replace = TRUE)
                )
            }
            
            write.csv(data_to_download, file, row.names = FALSE)
        }
    )
}  # End of server function

# Run the application
shinyApp(ui = ui, server = server)