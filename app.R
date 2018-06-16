#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Dự báo chi phí điều trị HCMVC bằng mô hình mạng thần kinh"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("gioitinh", h3("Giới tính"), 
                    choices = list("Nam" = 1, "Nu" = 2), selected = 1),
        numericInput("tuoi",
                      h3("Tuổi"),
                      value = 0),
        selectInput("nhomBN", h3("Nhóm bệnh nhân"), 
                    choices = list("1" = 1, 
                                   "2" = 2,
                                   "3" = 3,
                                   "4" = 4,
                                   "5" = 5), 
                    selected = 1),
        selectInput("muchuongBHYT", h3("Mức hưởng BHYT"), 
                    choices = list("80%" = "80%", 
                                   "95%" = "95%",
                                   "100%" = "100%"),
                    selected = 1),
        numericInput("ThoiGianDT",
                     h3("Thời gian điều trị"),
                     value = 0),
        selectInput("tinhtrangxuatvien", h3("Tình trạng xuất viện"), 
                    choices = list("Giam benh" = 1, 
                                   "On dinh" = 2), 
                    selected = 1),
        numericInput("sl_bkt",
                     h3("Số lượng bệnh kèm theo"),
                     value = 0),
        numericInput("sl_ytnc",
                     h3("Số lượng yếu tố nguy cơ"),
                     value = 0)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("predictValue")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  value <- reactive({
    nmct.test <- read.csv("F:/KhoaLuan/Data2/nmct-test-app.csv", stringsAsFactors = FALSE)
    
    gioitinh <- input$gioitinh
    tuoi <- input$tuoi
    # nhomBN <- input$nhomBN
    # muchuongBHYT <- input$muchuongBHYT
    # thoigianDT <- input$ThoiGianDT
    # tinhtrangxuatvien <- input$tinhtrangxuatvien
    # sl_bkt <- input$sl_bkt
    # sl_ytnc <- input$sl_ytnc

    nmct.test$GioiTinh[1] <- gioitinh
    nmct.test$Tuoi[1] <- tuoi
    # nmct.test$NhomBN[1] <- nhomBN
    # nmct.test$MucHuongBHYT[1] <- muchuongBHYT
    # nmct.test$ThoiGianDT[1] <- thoigianDT
    # nmct.test$TinhTrangBenhRaVen_MaHoa[1] <- tinhtrangxuatvien
    # nmct.test$sl_bkt[1] <- sl_bkt
    # nmct.test$sl_ytnc[1] <- sl_ytnc
    
    # nmct.test$GioiTinh[1] <- 1
    # nmct.test$Tuoi[1] <- 35
    nmct.test$NhomBN[1] <- 2
    nmct.test$MucHuongBHYT[1] <- "80%"
    nmct.test$ThoiGianDT[1] <- 10
    nmct.test$TinhTrangBenhRaVen_MaHoa[1] <- 1
    nmct.test$sl_bkt[1] <- 1
    nmct.test$sl_ytnc[1] <- 2
    
    #write.csv(nmct.test, "nmct-test-app.csv")
    #nmct.test <- h2o.importFile("F:/KhoaLuan/Data2/nmct-test-app.csv")
    nmct.test <- as.h2o(nmct.test)
    
    predictedValue <- h2o.predict(dl, nmct.test[1,])
    paste("Tong chi phi du doan la: ", predictedValue[1,1])
  })
   
  output$predictValue <- renderText({
    value()
     #paste("Tong chi phi du doan la: ", predictedValue[1,1])
     #  gioitinh <- input$gioitinh
     #  tuoi <- input$tuoi
     #  nhomBN <- input$nhomBN
     #  muchuongBHYT <- input$muchuongBHYT
     #  thoigianDT <- input$thoigianDT
     #  tinhtrangxuatvien <- input$tinhtrangxuatvien
     #  sl_bkt <- input$sl_bkt
     #  sl_ytnc <- input$sl_ytnc
     #  
     #  nmct.test$GioiTinh[1] <- gioitinh
     #  nmct.test$Tuoi[1] <- tuoi
     #  nmct.test$NhomBN[1] <- nhomBN
     #  nmct.test$MucHuongBHYT[1] <- muchuongBHYT
     #  nmct.test$ThoiGianDT[1] <- thoigianDT
     #  nmct.test$TinhTrangBenhRaVen_MaHoa[1] <- tinhtrangxuatvien
     #  nmct.test$sl_bkt[1] <- sl_bkt
     #  nmct.test$sl_ytnc[1] <- sl_ytnc
     #  
     #  nmct.test <- read.csv("F:/KhoaLuan/Data2/nmct-test-app.csv", stringsAsFactors = FALSE)
     #  
     #  nmct.test$GioiTinh[1] <- 1
     #  nmct.test$Tuoi[1] <- 35
     #  nmct.test$NhomBN[1] <- 2
     #  nmct.test$MucHuongBHYT[1] <- "80%"
     #  nmct.test$ThoiGianDT[1] <- 10
     #  nmct.test$TinhTrangBenhRaVen_MaHoa[1] <- 1
     #  nmct.test$sl_bkt[1] <- 1
     #  nmct.test$sl_ytnc[1] <- 2
     #  
     #  write.csv(nmct.test, "nmct-test-app.csv")
     #  nmct.test <- h2o.importFile("F:/KhoaLuan/Data2/nmct-test-app.csv")
     #  
     #  predictedValue <- h2o.predict(dl, nmct.test[1,])
     # paste("Tổng chi phí dự đoán là: ", predictedValue[1,1])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

