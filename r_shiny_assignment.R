library(shiny)
# for comma()
library(scales)
# for clean_names()
library(janitor)

ui <- fluidPage(
  
  titlePanel("Cumulative Paid Claims"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("claims_file", "Upload Claims CSV", accept = ".csv"),
      numericInput("tail_factor", "Tail Factor", value = 1.1, step = 0.01)
    ),
    
    mainPanel(
      h3("Cumulative Paid Claims ($) - Table"),
      tableOutput("output_table"),
      h3("Cumulative Paid Claims ($) - Graph"),
      plotOutput("output_plot", height = "500px")
    )
  )
)

server <- function(input, output) {
  
  raw_data <- reactive({
    
    req(input$claims_file)
    clean_names(read.csv(input$claims_file$datapath))
    
  })
  
  cumulative_triangle <- reactive({
    
    # faster to type rd instead of raw_data
    rd <- raw_data()
    
    # PREPARE CLAIMS DATA
    
    ## gsub() for removing all comma in numbers
    rd$amount_of_claims_paid <- as.numeric(gsub(",", "", rd$amount_of_claims_paid))
    rd <- rd[order(rd$loss_year, rd$development_year), ]
    rd$cumulative <- ave(rd$amount_of_claims_paid, rd$loss_year, FUN = cumsum)
    
    # MAX DEVELOPMENT YEAR (FROM USER INPUT) 
    # TARGET DEVELOPMENT YEAR (TO PROJECT UNTIL)
    
    max_dev <- max(rd$development_year, na.rm = TRUE)
    target_dev <- max_dev + 1
    
    # CUMULATIVE TABLE IN WIDE FORMAT
    
    wide_table <- rd[, c("loss_year", "development_year", "cumulative")]
    wide_table <- reshape(
      wide_table, 
      timevar = "development_year", 
      idvar = "loss_year", 
      direction = "wide"
      )
    
    ## rename colummn name from cumulative.1 to dev1
    colnames(wide_table) <- gsub("cumulative\\.", "dev", colnames(wide_table))
    wide_table <- wide_table[order(wide_table$loss_year), ]
    
    ## create column for target_dev, filling with NA
    wide_table[[paste0("dev", target_dev)]] <- NA
    
    # CONVERT TABLE TO MATRIX
    
    ## only take dev1, dev2,... columns
    matrix_ <- as.matrix(wide_table[, paste0("dev", 1:target_dev)])
    num_row <- nrow(matrix_)
    
    # CALCULATE DEVELOPMENT FACTORS 
    dev_factors <- numeric(max_dev)  
    
    for (j in 1:(max_dev - 1)) {
      
      ## keep rows where development_year is j or j+1
      store <- rd[rd$development_year %in% c(j, j + 1), ]
      store <- store[order(store$loss_year, store$development_year), ]
      
      ## keep only loss_years which have both j and j+1 dev years
      ## table() creates a freq table of each loss year
      ## which() returns the indices of TRUE result
      ## names() extract the name of the object
      accept <- names(which(table(store$loss_year) == 2))
      store <- store[store$loss_year %in% accept, ]
      
      ## split by the dev year and then sum them
      dev_j_1 <- store[store$development_year == j, ]
      dev_j_2 <- store[store$development_year == j + 1, ]
      sum_j_1 <- sum(dev_j_1$cumulative, na.rm = TRUE)
      sum_j_2 <- sum(dev_j_2$cumulative, na.rm = TRUE)
      
      ## calculate the development factor
      ## do > 0 to avoid division by zero
      dev_factors[j] <- ifelse(sum_j_1 > 0, sum_j_2 / sum_j_1 , 1)
    }
    
    # ADD TAIL FACTOR
    dev_factors[max_dev] <- input$tail_factor
    
    # FIND PROJECTED VALUES
    for (i in 1:num_row) {
      for (j in 2:target_dev) {
        if (is.na(matrix_[i, j]) && !is.na(matrix_[i, j - 1])) {
          matrix_[i, j] <- matrix_[i, j - 1] * dev_factors[j - 1]
        }
      }
    }
    
    projected_triangle <- as.data.frame(matrix_[, 1:target_dev])
    colnames(projected_triangle) <- paste0("dev", 1:target_dev)
    
    # add loss year column
    projected_triangle <- cbind(loss_year = wide_table$loss_year, projected_triangle)
    
    # round to whole number except for the loss year column
    projected_triangle[, -1] <- round(projected_triangle[, -1], 0)
    
    # make integer for table dsiplay
    projected_triangle[, -1] <- lapply(projected_triangle[, -1], as.integer)
    
    return(projected_triangle)
  })
  
  output$output_table <- renderTable({
    
    final_table <- cumulative_triangle()
    
    # rename columns to 1,2,3,...
    colnames(final_table) <- gsub("dev", "", colnames(final_table))
    
    # add comma at the thousand, except for the loss year column
    final_table[, -1] <- lapply(final_table[, -1], comma)
    final_table
  }, 
  bordered = TRUE
  )
  
  output$output_plot <- renderPlot({
    
    final_table <- cumulative_triangle()
    
    # reshape table to long format for plotting
    long_table <- reshape(
      final_table,
      varying = names(final_table)[-1],
      v.names = "Cumulative",
      timevar = "DevelopmentYear",
      times = as.integer(gsub("dev", "", names(final_table)[-1])),
      direction = "long"
    )
    
    # convert to factor means make it a category (for the legend)
    long_table$loss_year <- as.factor(long_table$loss_year)
    
    ggplot(long_table, aes(x = DevelopmentYear, y = Cumulative, color = loss_year)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
      
    # comma() adds comma at thousand mark for readability
    # vjust moves the label up
    # in the first run, "a" would appear above the legend, not sure why but setting show.legend = F fixes it
    geom_text(aes(label = comma(Cumulative)), vjust = -0.8, size = 3.2, show.legend = FALSE) +
    scale_y_continuous(labels = comma) +
    labs(
      x = "Development Year",
      y = "Cumulative Paid Claims ($)",
      color = "Loss Year"
    ) + 
      
    # tried different themes, this the best background for readability
    theme_minimal(base_size = 15)
  })
}

shinyApp(ui, server)
