#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(coefplot) 

# Melakukan estimasi regresi linear
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
# Mengestimasi model regresi linear berganda
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)

# Menampilkan ringkasan model
summary(model)

# Mengekstrak koefisien
koefisien <- coef(model)
koefisien

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Penjualan Bulanan - E-Commerce"),
  dashboardSidebar(
    sliderInput("x1", "Jumlah Pengunjung", min = 150000, max = 260000, value = 200000),
    sliderInput("x2", "Jumlah Transaksi", min = 8000, max = 15000, value = 12000),
    sliderInput("x3", "Rata-rata Item per Transaksi", min = 4.5, max = 5.5, value = 5),
    sliderInput("x4", "Rating Kepuasan", min = 8.2, max = 9.0, value = 8.7),
    sliderInput("x5", "Jumlah Iklan Online", min = 20000, max = 60000, value = 35000),
    actionButton("predictButton", "Prediksi Penjualan")
  ),
  dashboardBody(
    fluidRow(
      # Menambahkan diagram scatterplot untuk visualisasi hubungan antara variabel
      box(
        plotOutput("scatterplot", height = 250),
        title = "Hubungan Antara Variabel",
        background = "blue"
      ),
      
      box(
        plotOutput("boxplot", height = 250),
        title = "Boxplot Distribusi Penjualan",
        background = "purple"
      )
    ),
    
    fluidRow(
      # Menambahkan diagram residu untuk evaluasi regresi
      box(
        plotOutput("residualplot", height = 250),
        title = "Diagram Residu",
        background = "orange"
      ),
      
      box(
        plotOutput("correlationHeatmap", height = 250),
        title = "Peta Panas Korelasi",
        background = "green"
      )
    ),
    
    fluidRow(
      # Menambahkan teks hasil prediksi
      box(
        textOutput("predictionText"),
        title = "Hasil Prediksi",
        background = "black"
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Membuat prediksi berdasarkan input dari pengguna
  prediction <- reactive({
    predict(model, newdata = data.frame(
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5
    ))
  })
  
  # Menampilkan diagram scatterplot untuk visualisasi hubungan antara variabel
  output$scatterplot <- renderPlot({
    ggplot(data, aes(x = x1, y = y)) +
      geom_point() +
      labs(title = "Hubungan Antara Jumlah Pengunjung dan Penjualan",
           x = "Jumlah Pengunjung",
           y = "Penjualan")
  })
  
  # Menampilkan boxplot untuk distribusi variabel
  output$boxplot <- renderPlot({
    # Ganti 'variabel' dengan variabel yang ingin Anda lihat distribusinya
    ggplot(data, aes(x = 1, y = y)) +
      geom_boxplot() +
      labs(title = "Boxplot Distribusi Penjualan Bulanan")
  })
  
  # Menampilkan diagram residu untuk evaluasi regresi
  output$residualplot <- renderPlot({
    plot(model, which = 1)
  })
  
  # Menampilkan correlation heatmap untuk melihat korelasi antar variabel
  output$correlationHeatmap <- renderPlot({
    # Ganti 'vars' dengan variabel yang ingin Anda lihat korelasinya
    corr <- cor(data[, c("x1", "x2", "x3", "x4", "x5", "y")])
    heatmap(corr, 
            xlab = "Variabel", ylab = "Variabel",
            main = "Peta Panas Korelasi")
  })
  
  # Menampilkan hasil prediksi dalam bentuk teks
  output$predictionText <- renderText({
    paste("Berdasarkan input yang diberikan, perkiraan volume penjualan bulanan adalah:", round(prediction(), 2), "ribu USD.")
  })
}

# Menjalankan aplikasi Shiny
shinyApp(ui, server)
