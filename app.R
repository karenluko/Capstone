
library(shiny)
library(tm)
library(RWeka)
library(shinyWidgets)
library("shinythemes")
source("Prediction_database.R")

ui <- navbarPage("🆆🅷🅰🆃'🆂 🅽🅴🆇🆃?",
           tabPanel("Let's predict!",
                      fluidPage(
                      theme = shinytheme("sandstone"),
                      tags$head(tags$style(HTML(".form-control {border-radius: 4px 4px 4px 4px;} #prediction {font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;font-size: 14px;
        width: 750px;max-width: 100%; height: 165px; padding: 6px 12px; white-space: pre-wrap;}"))),
                      tags$head(tags$style(HTML('<style type="text/css"> .row-fluid { width: 50%; }.well {opacity:0.7; }</style>'))),
                      tags$head(tags$style(HTML(".navbar .navbar-header {float: left; } 
      .navbar-default .navbar-brand { color: #ff3368;font-size: 30px;}"))),
                      tags$head(tags$style(HTML("#prediction {font-size:16px;}"))),
                      tags$head(tags$style(HTML("input[type=\"text\"] {width:500px}"))),
                      tags$head(tags$style('p {color:black;font:12pt "Arial"}')),
                      tags$head(tags$style('h6 {color:white;background:black; opacity:0.5; font:14pt "Arial"}')),
                      tags$head(tags$style('h5 {color:white; font:14pt "Arial"}')),
                      tags$head(tags$style('h4 {color:black;font:14pt "Arial"}')),
                      tags$head(tags$style('h3 {color:#FFFACD;font:16pt "Arial"}')),
                      tags$head(tags$style('h2 {color:white;font:20pt "Arial"}')),
                      tags$head(tags$style('body {color:white;font:14pt "Arial"}')),
                      setBackgroundImage(
                        src = "https://raw.githubusercontent.com/karenluko/Capstone/main/background.jpg"
                      ),
                      headerPanel("Ｐｒｅｄｉｃｔ ｔｈｅ ｎｅｘｔ ｗｏｒｄ"),
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Instructions")),
                          tags$br(),
                          p("1.Type a few words into the text box"),
                          tags$br(),
                          p("2. The next word will be predicted based on your entry"),
                          tags$br(),
                          p("3. Thus, your last one, two, three or four words will be used to predict the output "),
                          tags$br(),
                          p("4. The output with your entry and the prediction will be displayed automatically"),
                          tags$br(),
                          p("5. Let's start!"),
                          tags$br()
                          ),
                        mainPanel(
                          tags$br(),
                          textInput("txt",label=h3(strong("Enter your text here:"))),
                          h3(strong("Your output:")),
                          verbatimTextOutput("prediction", placeholder = T) 
                              )
                          )
                       ),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    hr(),
                    h6(em(strong("Ｄｏｎ＇ｔ   ｅｖｅｒ   ｄｉｍｉｎｉｓｈ   ｔｈｅ   ｐｏｗｅｒ   ｏｆ   ｗｏｒｄｓ． Ｗｏｒｄｓ   ｍｏｖｅ   ｈｅａｒｔｓ   ａｎｄ   ｈｅａｒｔｓ   ｍｏｖｅ   ｌｉｍｂｓ."))),
                    h5("-Hamza Yusuf", align="right"),
                    ),
                    
           tabPanel("About the app",
                    h3("About 🆆🅷🅰🆃'🆂 🅽🅴🆇🆃?", align="center"),
                    tags$br(),
                    h5("The dataset was obtained from Twitter, News, and Blogs texts in English Language.", align="center"),
                    h5("The raw dataset has gone through many data mining processing and a prediction model was built for this project.", align="center"),
                    h5("The source code used to build this application can be found in",  a(strong("GitHub Repository"), href= "https://github.com/karenluko/Capstone"), align="center")
           ),
                    
           tabPanel("Acknowledgements",
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  h5("This application was created as part of the assignments of the Capstone Project of the Data Science from", a("Coursera", href="https://www.coursera.org/specializations/jhu-data-science"), align="center"),
                  h5("The raw dataset was provided by SwiftKey company.", align="center"),
                  tags$br(),
                  h2("Thank you!", align="center"),
                  tags$br(),
                   div(
                    img(src="https://d3njjcbhbojbot.cloudfront.net/api/utilities/v1/imageproxy/https://s3.amazonaws.com/coursera_assets/xdp/jhu_v3.svg?auto=format%2Ccompress&dpr=1&h=50",  style= "padding: 25px;"),
                    img(src="https://upload.wikimedia.org/wikipedia/en/3/32/Coursera_logo.svg", style= "padding: 25px;"),
                    img(src="https://logodownload.org/wp-content/uploads/2015/02/unesp-logo-6.png", style= "padding: 25px;"),
                    img(src="https://d3njjcbhbojbot.cloudfront.net/api/utilities/v1/imageproxy/https://s3.amazonaws.com/coursera_assets/specialization_capstone_promotion/swiftkey.png?auto=format%2Ccompress&dpr=2&w=100&h=100", style= "padding: 25px;"),
                    style="text-align: center;")
           )
)

server<-shinyServer(
  
  function(input, output) {
    output$txt <- renderPrint({input$txt})  
    output$prediction <- reactive(paste(input$txt,pred()))
    pred<- reactive({
      PNW(sentence=input$txt) 
    })
  }
)

# Run the application 
shinyApp(ui = ui, server = server)
