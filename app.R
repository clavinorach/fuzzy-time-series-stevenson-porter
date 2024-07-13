# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load necessary libraries

install.packages('rsconnect')
install.packages(c("shiny", "dplyr", "lubridate"))
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(DT)
library(shinythemes)
library(bslib)
library(rsconnect)


# Define the UI
ui <- fluidPage(
  #theme = shinytheme("slate"),  # Apply a theme
  theme = bs_theme(preset = "vapor"),
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper {
        color: #E3E3E3;
      }
      table.dataTable thead th {
        color: #E3E3E3;
      }
      table.dataTable tbody tr {
        color: #E3E3E3;
      }
      table.dataTable tfoot th {
        color: #E3E3E3;
      },
      .credits {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 10px;
      }
      .credits img {
        width: 20px;
        height: 20px;
        vertical-align: middle;
      }
      .credits span {
        margin-left: 5px;
        margin-right: 15px;
      }
    "))
  ),
  titlePanel("Predictive Web App for Average Temperature Using a Multi-Model Fuzzy Time Series Algorithm"),
  # Credits below the title
  tags$div(
    class = "credits",
    tags$a(href="https://www.linkedin.com/in/achmadardanip/", target="_blank",
           tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"),
           tags$span("Achmad Ardani Prasha")
    ),
    tags$a(href="https://www.linkedin.com/in/clavinorachmadi/", target="_blank",
           tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"),
           tags$span("Clavino Ourizqi Rachmadi")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      tags$hr(),
      numericInput("period", "Prediction Period (days):", min = 1, max = 30, value = 7),
      selectInput("model", "Select Model:", choices = c("Stevenson Porter", "Markov Chain"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Prediction", DT::dataTableOutput("prediction_table")),
        tabPanel("Accuracy", tableOutput("accuracy")),
        tabPanel("Comparison Plot", plotOutput("comparison_plot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  bs_themer()
  data_reactive <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    data$date <- as.Date(data$date, format = "%d-%m-%Y")
    data$tavg <- na.locf(data$tavg)
    data
  })
  
  monthly_data <- reactive({
    data <- data_reactive()
    data %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarize(monthly_avg_temp = mean(tavg, na.rm = TRUE))
  })
  
  output$plot <- renderPlot({
    data <- monthly_data()
    ggplot(data, aes(x = month, y = monthly_avg_temp)) +
      geom_line(color = 'blue') +
      geom_smooth(method = "loess", span = 0.3, color = 'red', se = FALSE) +
      labs(title = "Time Series Plot of Average Monthly Temperature\nin Kolaka Regency Southeast Sulawesi from 2001-2024",
           x = "Period",
           y = "Average Temperature (°C)") +
      theme_minimal() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, margin = margin(t = 20, b = 20))
      )
  })
  
  intervals <- reactive({
    data <- data_reactive()
    n <- length(data$tavg)
    
    dt <- numeric(n-1)
    for (i in 1:(n-1)){
      dt[i] <- ((data$tavg[i+1] - data$tavg[i]) / data$tavg[i]) * 100
    }
    
    Umin <- floor(min(dt))
    Umax <- ceiling(max(dt))
    
    n_intervals <- round(1 + 3.322 * log10(length(dt)))
    interval_length <- (Umax - Umin) / n_intervals
    
    intervals <- data.frame(mins = numeric(n_intervals), maxs = numeric(n_intervals))
    for (i in 1:n_intervals) {
      intervals[i, 1] <- Umin + (i - 1) * interval_length
      intervals[i, 2] <- Umin + i * interval_length
    }
    intervals[n_intervals, 2] <- intervals[n_intervals, 2] + 0.01
    
    intervals
  })
  
  fuzifikasi <- reactive({
    data <- data_reactive()
    intervals <- intervals()
    n <- length(data$tavg)
    
    dt <- numeric(n-1)
    for (i in 1:(n-1)){
      dt[i] <- ((data$tavg[i+1] - data$tavg[i]) / data$tavg[i]) * 100
    }
    
    fuzifikasi <- numeric(n-1)
    for (i in 1:(n-1)){
      for (j in 1:nrow(intervals)){
        if (i != which.max(dt)){
          if (dt[i] >= intervals[j, 1] & dt[i] < intervals[j, 2]){
            fuzifikasi[i] <- j
            break
          }
        } else {
          if (dt[i] >= intervals[j, 1] & dt[i] <= intervals[j, 2]){
            fuzifikasi[i] <- j
            break
          }
        }
      }
    }
    
    fuzifikasi
  })
  
  flrg <- reactive({
    fuzifikasi <- fuzifikasi()
    intervals <- intervals()
    n <- length(fuzifikasi)
    
    flr <- data.frame(current_state = 0, next_state = 0)
    for (i in 1:n){
      if (i < n){
        flr[i,] <- c(fuzifikasi[i], fuzifikasi[i+1])
      } else {
        flr[i,] <- c(fuzifikasi[i], 0)
      }
    }
    
    flrg <- list()
    for (i in 1:nrow(intervals)){
      flrgi <- c()
      for (j in 1:(n-1)){
        if (flr[j,1] == i){
          flrgi <- c(flrgi, flr[j,2])
        }
      }
      flrg[[i]] <- flrgi
    }
    
    flrg
  })
  
  flrg_avg <- reactive({
    flrg <- flrg()
    intervals <- intervals()
    n <- nrow(intervals)
    
    flrg_avg <- numeric(n)
    for (i in 1:length(flrg)) {
      if (length(flrg[[i]]) > 0) {
        flrg_avg[i] <- mean(flrg[[i]], na.rm = TRUE)
      } else {
        flrg_avg[i] <- NA
      }
    }
    
    flrg_avg
  })
  
  prediction <- reactive({
    if (input$model == "Stevenson Porter") {
      flrg_avg <- flrg_avg()
      intervals <- intervals()
      m <- as.vector(rowMeans(intervals))
      
      hasil <- numeric(length(m))
      for (i in 1:length(m)){
        if (i == 1)
          hasil[i] <- (1.5) / ((1 / m[i]) + (0.5 / m[i+1]))
        else if (i == length(m))
          hasil[i] <- (1.5) / ((0.5 / m[i-1]) + (1 / m[i]))
        else
          hasil[i] <- (2) / ((0.5 / m[i-1]) + (1 / m[i]) + (0.5 / m[i+1]))
      }
      
      prediksi <- numeric(length(m))
      for (i in 1:length(fuzifikasi())){
        for (j in 1:length(hasil)){
          if (fuzifikasi()[i] == j){
            prediksi[i] <- hasil[j]
          }
        }
      }
      
      prediksi
    }
  })
  
  output$prediction_table <- DT::renderDataTable({
    prediksi <- prediction()
    data <- data_reactive()
    
    if (input$model == "Stevenson Porter") {
      t <- numeric(length(data$tavg) - 1)
      for (i in 2:length(data$tavg)){
        t[i - 1] <- data$tavg[i - 1] + (data$tavg[i - 1] * prediksi[i - 1] / 100)
      }
      
      datapakai <- data$tavg[-1]
      galat <- abs(datapakai - t )
      
      prediction_dates <- seq(min(data$date) + 1, by = "day", length.out = length(t))
      tabel <- data.frame(Date = format(prediction_dates, "%d-%m-%Y"), Actual = datapakai, Predicted = t, Error = galat)
      
      # Add 7-day future predictions
      last_date <- max(data$date)
      future_dates <- seq(last_date + 1, by = "day", length.out = input$period)
      future_predictions <- rep(NA, input$period)
      
      future_t <- numeric(input$period)
      for (i in 1:input$period) {
        if (i == 1) {
          future_t[i] <- t[length(t)] + (t[length(t)] * prediksi[i] / 100)
        } else {
          future_t[i] <- future_t[i - 1] + (future_t[i - 1] * prediksi[i] / 100)
        }
      }
      
      future_table <- data.frame(Date = format(future_dates, "%d-%m-%Y"), Actual = future_predictions, Predicted = future_t, Error = rep(NA, input$period))
      final_table <- rbind(tabel, future_table)
      
      datatable(final_table, options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      datauji <- data_reactive()
      data <- datauji$tavg
      n <- length(data)
      minimum <- min(data)
      maximum <- max(data)
      new.min <- minimum - 0.2
      new.max <- maximum + 0.3
      n_intervals <- round(1 + (3.3 * logb(length(data), base = 10)))
      L <- (new.max - new.min) / n_intervals
      intrv.1 <- seq(new.min, new.max, len = n_intervals + 1)
      box1 <- data.frame(NA, nrow = length(intrv.1) - 1, ncol = 3)
      names(box1) <- c("bawah", "atas", "kel")
      for (i in 1:(length(intrv.1) - 1)) {
        box1[i, 1] <- intrv.1[i]
        box1[i, 2] <- intrv.1[i + 1]
        box1[i, 3] <- i
      }
      n.tengah <- data.frame(tengah = (box1[, 1] + box1[, 2]) / 2, kel = box1[, 3])
      fuzifikasi <- c()
      for (i in 1:length(data)) {
        for (j in 1:nrow(box1)) {
          if (i != which.max(data)) {
            if (data[i] >= (box1[j, 1]) & data[i] < (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          } else {
            if (data[i] >= (box1[j, 1]) & data[i] <= (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          }
        }
      }
      fuzzyfy <- cbind(data, fuzifikasi)
      FLR <- data.frame(fuzzifikasi = 0, left = NA, right = NA)
      for (i in 1:length(fuzifikasi)) {
        FLR[i, 1] <- fuzifikasi[i]
        FLR[i + 1, 2] <- fuzifikasi[i]
        FLR[i, 3] <- fuzifikasi[i]
      }
      FLR <- FLR[-nrow(FLR), ]
      FLR <- FLR[-1, ]
      FLRG <- table(FLR[, 2:3])
      bobot <- round(prop.table(table(FLR[, 2:3]), 1), 5)
      diagonal <- diag(bobot)
      m.diagonal <- diag(diagonal)
      pinggir <- bobot - m.diagonal
      ramal <- NULL
      for (i in 1:(length(fuzifikasi))) {
        for (j in 1:(nrow(bobot))) {
          if (fuzifikasi[i] == j) {
            ramal[i + 1] <- (diagonal[j] * data[i]) + sum(pinggir[j, ] * n.tengah[, 1])
          } else if (fuzifikasi[i] == j) {
            ramal[i] <- 0
          }
        }
      }
      ramal <- ramal[-length(ramal)]
      adjusted <- rep(0, nrow(FLR))
      selisih <- FLR[, 3] - FLR[, 2]
      for (i in 1:nrow(FLR)) {
        if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] == 0) {
          adjusted[i] <- selisih[i] * (L / 2)
        } else if (selisih[i] == 1 && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- L
        } else if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- selisih[i] * L / 2
        }
      }
      ramal <- ramal[c(2:length(ramal))]
      adj.forecast <- adjusted + ramal
      datapakai <- data[c(2:length(data))]
      galat <- abs(datapakai - adj.forecast)
      tabel <- cbind(datapakai, ramal, adjusted, adj.forecast, galat)
      adj.forecast2 <- rep(0, length(datapakai))
      for (i in 1:length(datapakai)) {
        if ((ramal[i] - datapakai[i]) < (adj.forecast[i] - datapakai[i])) {
          adj.forecast2[i] <- ramal[i] + (L / 2)
        } else {
          adj.forecast2[i] <- adj.forecast[i]
        }
      }
      tabel2 <- cbind(datapakai, ramal, adj.forecast, adj.forecast2)
      datauji_excluded <- datauji[-1, ]
      tabel_prediksi <- data.frame(date = datauji_excluded$date, datapakai, ramal, adj.forecast, adj.forecast2)
      periode_peramalan <- input$period
      ramalan_7_hari <- forecast_ftsmc(data = adj.forecast2, n.tengah = n.tengah, bobot = bobot, L = L, box1 = box1, periode = periode_peramalan)
      tabel2_new <- rbind(tabel2, data.frame(datapakai=NA, ramal=NA, adj.forecast=NA, adj.forecast2=ramalan_7_hari))
      
      tanggal_terakhir <- max(tanggal)
      tanggal_lanjut <- seq(tanggal_terakhir + 1, by = "day", length.out = nrow(tabel2_new) - length(tanggal))
      tabel2_new$Date <- c(tanggal, tanggal_lanjut)
      colnames(tabel2_new) <- c("Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Date")
      
      tabel2_new$Error <- abs(tabel2_new$Actual - tabel2_new$`Final Adjusted Forecast`)
      
      tabel2_new <- tabel2_new[, c("Date", "Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Error")]
      
      # Display the corrected tabel2_new
      tabel2_new
    }
  })
  
  output$accuracy <- renderTable({
    prediksi <- prediction()
    data <- data_reactive()
    
    if (input$model == "Stevenson Porter") {
      t <- numeric(length(data$tavg) - 1)
      for (i in 2:length(data$tavg)){
        t[i - 1] <- data$tavg[i - 1] + (data$tavg[i - 1] * prediksi[i - 1] / 100)
      }
      
      datapakai <- data$tavg[-1]
      galat <- abs(datapakai - t)
      
      MSE <- mean(galat^2, na.rm = TRUE)
      MAE <- mean(abs(galat), na.rm = TRUE)
      MAPE <- mean(abs(galat / datapakai * 100), na.rm = TRUE)
      
      ketepatan <- data.frame(MSE = MSE, MAE = MAE, MAPE = MAPE)
      ketepatan
    } else {
      datauji <- data_reactive()
      data <- datauji$tavg
      n <- length(data)
      minimum <- min(data)
      maximum <- max(data)
      new.min <- minimum - 0.2
      new.max <- maximum + 0.3
      n_intervals <- round(1 + (3.3 * logb(length(data), base = 10)))
      L <- (new.max - new.min) / n_intervals
      intrv.1 <- seq(new.min, new.max, len = n_intervals + 1)
      box1 <- data.frame(NA, nrow = length(intrv.1) - 1, ncol = 3)
      names(box1) <- c("bawah", "atas", "kel")
      for (i in 1:(length(intrv.1) - 1)) {
        box1[i, 1] <- intrv.1[i]
        box1[i, 2] <- intrv.1[i + 1]
        box1[i, 3] <- i
      }
      n.tengah <- data.frame(tengah = (box1[, 1] + box1[, 2]) / 2, kel = box1[, 3])
      fuzifikasi <- c()
      for (i in 1:length(data)) {
        for (j in 1:nrow(box1)) {
          if (i != which.max(data)) {
            if (data[i] >= (box1[j, 1]) & data[i] < (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          } else {
            if (data[i] >= (box1[j, 1]) & data[i] <= (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          }
        }
      }
      fuzzyfy <- cbind(data, fuzifikasi)
      FLR <- data.frame(fuzzifikasi = 0, left = NA, right = NA)
      for (i in 1:length(fuzifikasi)) {
        FLR[i, 1] <- fuzifikasi[i]
        FLR[i + 1, 2] <- fuzifikasi[i]
        FLR[i, 3] <- fuzifikasi[i]
      }
      FLR <- FLR[-nrow(FLR), ]
      FLR <- FLR[-1, ]
      FLRG <- table(FLR[, 2:3])
      bobot <- round(prop.table(table(FLR[, 2:3]), 1), 5)
      diagonal <- diag(bobot)
      m.diagonal <- diag(diagonal)
      pinggir <- bobot - m.diagonal
      ramal <- NULL
      for (i in 1:(length(fuzifikasi))) {
        for (j in 1:(nrow(bobot))) {
          if (fuzifikasi[i] == j) {
            ramal[i + 1] <- (diagonal[j] * data[i]) + sum(pinggir[j, ] * n.tengah[, 1])
          } else if (fuzifikasi[i] == j) {
            ramal[i] <- 0
          }
        }
      }
      ramal <- ramal[-length(ramal)]
      adjusted <- rep(0, nrow(FLR))
      selisih <- FLR[, 3] - FLR[, 2]
      for (i in 1:nrow(FLR)) {
        if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] == 0) {
          adjusted[i] <- selisih[i] * (L / 2)
        } else if (selisih[i] == 1 && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- L
        } else if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- selisih[i] * L / 2
        }
      }
      ramal <- ramal[c(2:length(ramal))]
      adj.forecast <- adjusted + ramal
      datapakai <- data[c(2:length(data))]
      galat <- abs(datapakai - adj.forecast)
      tabel <- cbind(datapakai, ramal, adjusted, adj.forecast, galat)
      adj.forecast2 <- rep(0, length(datapakai))
      for (i in 1:length(datapakai)) {
        if ((ramal[i] - datapakai[i]) < (adj.forecast[i] - datapakai[i])) {
          adj.forecast2[i] <- ramal[i] + (L / 2)
        } else {
          adj.forecast2[i] <- adj.forecast[i]
        }
      }
      tabel2 <- cbind(datapakai, ramal, adj.forecast, adj.forecast2)
      galat2 <- abs(datapakai - adj.forecast2)
      MSE <- mean(galat2^2, na.rm = TRUE)
      MAE <- mean(abs(galat2), na.rm = TRUE)
      MAPE <- mean(abs(galat2 / datapakai * 100), na.rm = TRUE)
      ketepatan <- data.frame(MSE, MAE, MAPE)
      ketepatan
    }
  })
  
  output$comparison_plot <- renderPlot({
    prediksi <- prediction()
    data <- data_reactive()
    
    if (input$model == "Stevenson Porter") {
      t <- numeric(length(data$tavg) - 1)
      for (i in 2:length(data$tavg)) {
        t[i - 1] <- data$tavg[i - 1] + (data$tavg[i - 1] * prediksi[i - 1] / 100)
      }
      
      last_few_points <- tail(data$tavg, 5)
      average_change <- mean(diff(last_few_points), na.rm = TRUE)
      
      first_date <- min(data$date)
      
      prediksi_dates <- seq(from = first_date + 1, by = "day", length.out = input$period)
      
      prediksi_7_hari <- numeric(input$period)
      for (i in 1:input$period) {
        if (i == 1) {
          prediksi_7_hari[i] <- last_few_points[length(last_few_points)] + average_change
        } else {
          prediksi_7_hari[i] <- prediksi_7_hari[i - 1] + average_change
        }
      }
      predicted_7_days <- prediksi_7_hari
      
      predicted_t <- numeric(input$period)
      predicted_t[1] <- t[length(t)] + (t[length(t)] * predicted_7_days[1] / 100)
      for (i in 2:input$period) {
        predicted_t[i] <- predicted_t[i - 1] + (predicted_t[i - 1] * predicted_7_days[i] / 100)
      }
      
      t <- c(t, predicted_7_days)
      
      datapakai <- c(data$tavg[-1], rep(NA, input$period))  # Adjust the length to match the predicted data
      
      # Ensure both vectors are of the same length
      if (length(datapakai) < length(t)) {
        datapakai <- c(datapakai, rep(NA, length(t) - length(datapakai)))
      } else if (length(datapakai) > length(t)) {
        t <- c(t, rep(NA, length(datapakai) - length(t)))
      }
      
      tabel_final <- data.frame(datapakai = datapakai, t = t)
      
      datauji <- data
      datauji$date <- as.Date(datauji$date, format = "%d-%m-%Y")
      
      datauji_excluded <- datauji[-1, ]
      datauji_excluded$date <- as.Date(datauji_excluded$date, format = "%Y-%m-%d")
      
      monthly_data_actual <- datauji_excluded %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(month) %>%
        summarize(monthly_avg_temp = mean(tavg, na.rm = TRUE))
      
      tabel_prediksi <- data.frame(tanggal = c(datauji$date[-1], prediksi_dates), adj.forecast2 = t)
      
      monthly_data_predicted <- tabel_prediksi %>%
        mutate(month = floor_date(tanggal, "month")) %>%
        group_by(month) %>%
        summarize(monthly_avg_temp = mean(adj.forecast2, na.rm = TRUE))
      
      combined_data <- bind_rows(
        monthly_data_actual %>% mutate(type = "Actual"),
        monthly_data_predicted %>% mutate(type = "Predicted")
      )
      
      ggplot(combined_data, aes(x = month, y = monthly_avg_temp, color = type)) +
        geom_line(aes(linetype = type)) +
        scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange")) +
        labs(title = "Time Series Plot of Average Monthly Temperature",
             x = "Period",
             y = "Average Temperature (°C)",
             color = "Type") +
        theme_minimal() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      datauji <- data_reactive()
      data <- datauji$tavg
      n <- length(data)
      minimum <- min(data)
      maximum <- max(data)
      new.min <- minimum - 0.2
      new.max <- maximum + 0.3
      n_intervals <- round(1 + (3.3 * logb(length(data), base = 10)))
      L <- (new.max - new.min) / n_intervals
      intrv.1 <- seq(new.min, new.max, len = n_intervals + 1)
      box1 <- data.frame(NA, nrow = length(intrv.1) - 1, ncol = 3)
      names(box1) <- c("bawah", "atas", "kel")
      for (i in 1:(length(intrv.1) - 1)) {
        box1[i, 1] <- intrv.1[i]
        box1[i, 2] <- intrv.1[i + 1]
        box1[i, 3] <- i
      }
      n.tengah <- data.frame(tengah = (box1[, 1] + box1[, 2]) / 2, kel = box1[, 3])
      fuzifikasi <- c()
      for (i in 1:length(data)) {
        for (j in 1:nrow(box1)) {
          if (i != which.max(data)) {
            if (data[i] >= (box1[j, 1]) & data[i] < (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          } else {
            if (data[i] >= (box1[j, 1]) & data[i] <= (box1[j, 2])) {
              fuzifikasi[i] <- j
              break
            }
          }
        }
      }
      fuzzyfy <- cbind(data, fuzifikasi)
      FLR <- data.frame(fuzzifikasi = 0, left = NA, right = NA)
      for (i in 1:length(fuzifikasi)) {
        FLR[i, 1] <- fuzifikasi[i]
        FLR[i + 1, 2] <- fuzifikasi[i]
        FLR[i, 3] <- fuzifikasi[i]
      }
      FLR <- FLR[-nrow(FLR), ]
      FLR <- FLR[-1, ]
      FLRG <- table(FLR[, 2:3])
      bobot <- round(prop.table(table(FLR[, 2:3]), 1), 5)
      diagonal <- diag(bobot)
      m.diagonal <- diag(diagonal)
      pinggir <- bobot - m.diagonal
      ramal <- NULL
      for (i in 1:(length(fuzifikasi))) {
        for (j in 1:(nrow(bobot))) {
          if (fuzifikasi[i] == j) {
            ramal[i + 1] <- (diagonal[j] * data[i]) + sum(pinggir[j, ] * n.tengah[, 1])
          } else if (fuzifikasi[i] == j) {
            ramal[i] <- 0
          }
        }
      }
      ramal <- ramal[-length(ramal)]
      adjusted <- rep(0, nrow(FLR))
      selisih <- FLR[, 3] - FLR[, 2]
      for (i in 1:nrow(FLR)) {
        if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] == 0) {
          adjusted[i] <- selisih[i] * (L / 2)
        } else if (selisih[i] == 1 && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- L
        } else if (FLR[i, 2] != FLR[i, 3] && diagonal[FLR[i, 2]] > 0) {
          adjusted[i] <- selisih[i] * L / 2
        }
      }
      ramal <- ramal[c(2:length(ramal))]
      adj.forecast <- adjusted + ramal
      datapakai <- data[c(2:length(data))]
      galat <- abs(datapakai - adj.forecast)
      tabel <- cbind(datapakai, ramal, adjusted, adj.forecast, galat)
      adj.forecast2 <- rep(0, length(datapakai))
      for (i in 1:length(datapakai)) {
        if ((ramal[i] - datapakai[i]) < (adj.forecast[i] - datapakai[i])) {
          adj.forecast2[i] <- ramal[i] + (L / 2)
        } else {
          adj.forecast2[i] <- adj.forecast[i]
        }
      }
      # Combine the relevant columns into 'tabel2'
      tabel2 <- cbind(datapakai, ramal, adj.forecast, adj.forecast2)
      
      datauji_excluded <- datauji[-1, ]
      tabel_prediksi <- data.frame(date = datauji_excluded$date, datapakai, ramal, adj.forecast, adj.forecast2)
      
    
      periode_peramalan <- input$period
      ramalan_7_hari <- forecast_ftsmc(data = adj.forecast2, n.tengah = n.tengah, bobot = bobot, L = L, box1 = box1, periode = periode_peramalan)
      
      tabel2_new <- rbind(tabel2, data.frame(datapakai = NA, ramal = NA, adj.forecast = NA, adj.forecast2 = ramalan_7_hari))
      
    
      tanggal_terakhir <- max(tanggal)
      tanggal_lanjut <- seq(tanggal_terakhir + 1, by = "day", length.out = nrow(tabel2_new) - length(tanggal))
      
      tabel2_new$Date <- as.Date(tabel2_new$Date, format = "%Y-%m-%d")
      
      colnames(tabel2_new) <- c("Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Date")
      
      tabel2_new$Error <- abs(tabel2_new$Actual - tabel2_new$`Final Adjusted Forecast`)
      
      tabel2_new <- tabel2_new[, c("Date", "Actual", "Initial Forecast", "Adjusted Forecast", "Final Adjusted Forecast", "Error")]
      
      # Display the corrected 'tabel2_new'
      tabel2_new
      
      # Convert date columns to Date format if not already done
      datauji_excluded <- datauji[-1,]
      datauji_excluded$date <- as.Date(datauji_excluded$date, format = "%d-%m-%Y")
      tabel2_new$tanggal <- as.Date(tabel2_new$tanggal, format = "%Y-%m-%d")
      # Aggregate data by month for better readability
      monthly_data_actual <- datauji_excluded %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(month) %>%
        summarize(monthly_avg_temp = mean(tavg, na.rm = TRUE))
      
      
      monthly_data_predicted <- tabel2_new %>%
        mutate(month = floor_date(Date, "month")) %>%
        group_by(month) %>%
        summarize(monthly_avg_temp = mean(`Final Adjusted Forecast`, na.rm = TRUE))
      
      # Display the corrected monthly_data_predicted
      monthly_data_predicted
      
      # Combine actual and predicted data
      combined_data <- bind_rows(
        monthly_data_actual %>% mutate(type = "Actual"),
        monthly_data_predicted %>% mutate(type = "Predicted")
      )
      
      # Plot the time series data using ggplot2
      ggplot(combined_data, aes(x = month, y = monthly_avg_temp, color = type)) +
        geom_line(aes(linetype = type)) +
        scale_color_manual(values = c("Actual" = "blue", "Predicted" = "orange")) +
        labs(title = "Time Series Plot of Average Monthly Temperature",
             x = "Period",
             y = "Average Temperature (°C)",
             color = "Type") +
        theme_minimal() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set x-axis breaks every year
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    }
  })
}

# Function to perform forecasting using the FTSMC model
forecast_ftsmc <- function(data, n.tengah, bobot, L, box1, periode) {
  # Fuzifikasi data
  fuzifikasi <- rep(NA, length(data))
  for (i in 1:length(data)) {
    for (j in 1:nrow(box1)) {
      if (data[i] >= (box1[j,1]) & data[i] < (box1[j,2])) {
        fuzifikasi[i] = j
        break
      } else if (i == length(data) && data[i] == box1[j,2]) {
        fuzifikasi[i] = j
        break
      }
    }
  }
  
  # Memastikan tidak ada nilai NA dalam fuzifikasi
  if (any(is.na(fuzifikasi))) {
    stop("Fuzifikasi menghasilkan nilai NA. Pastikan semua data berada dalam rentang interval yang benar.")
  }
  
  # Inisialisasi variabel ramalan
  ramal <- numeric(length(data) + periode)
  ramal[1:length(data)] <- data
  
  # Melakukan peramalan untuk periode ke depan
  for (i in (length(data) + 1):(length(data) + periode)) {
    for (j in 1:(nrow(bobot))) {
      if (!is.na(fuzifikasi[i-1]) && fuzifikasi[i-1] == j) {
        ramal[i] = (diagonal[j] * ramal[i-1]) + sum(pinggir[j,] * n.tengah[,1])
      }
    }
    # Fuzifikasi nilai ramalan untuk periode berikutnya
    for (j in 1:nrow(box1)) {
      if (ramal[i] >= (box1[j,1]) & ramal[i] < (box1[j,2])) {
        fuzifikasi[i] = j
        break
      } else if (i == length(ramal) && ramal[i] == box1[j,2]) {
        fuzifikasi[i] = j
        break
      }
    }
  }
  
  # Mengembalikan hasil ramalan
  return(ramal[(length(data) + 1):(length(data) + periode)])
}

# Run the application 
shinyApp(ui = ui, server = server)