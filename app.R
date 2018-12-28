require(DT)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)
library(DT)
source('functions.R')
input = rules[2]

custIds = final$CustomerID
prodNames = rules$input
clusters = seg[2]

ui <- navbarPage (theme= shinytheme('cerulean'),"Recommendation System",
                  setBackgroundImage(src = "background.jpg"),
                
                  tabPanel("Product Grouping",
                          tags$head(
                              tags$style(HTML("
                                             .shiny-output-error-validation {
                                             color: red;
                                             }
                                             "))
                             
                             ),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "prod_cat", label = "Product", prodNames ),
                               
                               tags$head(
                                 tags$style(HTML('#button1{background-color:green}'))
                               ),
                               actionButton(inputId = 'button1', label = 'Show')
                             ),
                             
                             mainPanel(
                               tags$head(
                                 tags$style(HTML('#text2{font-size: 20px; font-weight: Bold; font-style: italic; background-color: wheat}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#text1{font-size: 20px; font-style: italic; background-color: whitesmoke}'))
                               ),
                               htmlOutput('text2'),
                               htmlOutput('text1'),
                               tags$head(
                                 tags$style(HTML('#table_1{font-size: 20px; font-style: italic; background-color: wheat}'))
                               ),
                               # result
                               DT::dataTableOutput('table_1')
                               
                               
                             )
                           )           
                          ),
                  tabPanel("Customer Recommendations",
                           tags$head(
                             tags$style(HTML("
                                             .shiny-output-error-validation {
                                             color: red;
                                             }
                                             "))
                             ),
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput(inputId = "customerID", label = "Customer ID", custIds),
                               tags$head(
                                 tags$style(HTML('#button2{background-color:green}'))
                               ),
                               actionButton(inputId = 'button2', label = 'Get Customer Details'),
                               selectInput(inputId = "prodDesc", label = "Enter Product", prodNames),
                               tags$head(
                                 tags$style(HTML('#button3{background-color:green}'))
                               ),

                               actionButton(inputId = 'button3', label = 'Show')
                             ),
                             
                             mainPanel(
                               htmlOutput('text3'),
                               tags$head(
                                 tags$style(HTML('#text3{font-size: 20px; font-weight: Bold; font-style: italic; background-color: wheat}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#showClust{font-size: 20px; font-style: italic; background-color: whitesmoke}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#table_2{font-size: 20px; font-style: italic; background-color: wheat}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#table_3{font-size: 20px; font-style: italic; background-color: wheat}'))
                               ),
                               textOutput('showClust'),
                               
                               # result
                               DT::dataTableOutput('table_2'),
                               DT::dataTableOutput('table_3')
                               
                               
                             )
                           )           
                  ),
                  tabPanel("Group Recommendations",
                           tags$head(

                             tags$style(HTML("
                                             .shiny-output-error-validation {
                                             color: red;
                                             }
                                             "))
                             ),
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput(inputId = "clustDesc", label = "Select Cluster", clusters),
                               tags$head(
                                 tags$style(HTML('#button4{background-color:green}'))
                               ),
                               actionButton(inputId = 'button4', label = 'Get Group Recommendations')),
                        
                             mainPanel(
                               tags$head(
                                 tags$style(HTML('#text4{font-size: 20px; font-weight: Bold; font-style: italic; background-color: wheat}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#text5{font-size: 20px; font-style: italic; background-color: whitesmoke}'))
                               ),
                               tags$head(
                                 tags$style(HTML('#table_4{font-size: 20px; font-style: italic; background-color: wheat}'))
                               ),
                               
                               htmlOutput('text4'),
                               htmlOutput('text5'),
                               # result
                               DT::dataTableOutput('table_4'),
                               DT::dataTableOutput('table_5')
                               
                               
                             )
                           )           
                         )
)

server <- function(input, output){
  
                    output$text1 <- renderUI ({
                    str5 <- paste("_____________________________________________________________________________________________________________________________________________________________________ ")
                    str6 <- paste("If a customer bought the product, he/she might buy the recommended products")
                    str7 <- paste("You only need to know customer type and product the customer bought in the past")
                    str8 <- paste("_____________________________________________________________________________________________________________________________________________________________________ ")
                    HTML(paste(str5, str6, str7, str8, sep = '<br/>'))
                  })
                   
                    df_result_1 <- eventReactive(
                    input$button1,{
                      MBA(input$prod_cat)
                    })
                  
                  
                  output$table_1 <- DT::renderDataTable({
                    df_result_1()
                  },escape=FALSE,options = list(lengthChange = FALSE)
                  ) 
                  
                  
                   df_result_2 <- eventReactive(
                    input$button2,{
                       getPastPurcahses(input$customerID)
                    })
                  
                  output$table_2 <- DT::renderDataTable({
                    df_result_2()
                  },escape=FALSE,options = list(lengthChange = FALSE)
                  ) 
                  
                  clust <- eventReactive(
                    input$button2,{
                      as.character(paste0("Customer belongs to group : ",getClustAndDesc(input$customerID)))
                    }
                  )
                  output$showClust <- renderText({
                    clust()
                  })
                  
                  df_result_3 <- eventReactive(
                    input$button3,{
                      getRecoForCustandProd(custId=input$customerID, clustDesc = '', prodDesc=input$prodDesc)
                    })
                  
                  output$table_3 <- DT::renderDataTable({
                    df_result_3()
                  },escape=FALSE,options = list(lengthChange = FALSE)
                  )
                  
                  df_result_4 <- eventReactive(
                    input$button4,{
                      getTopReco(input$clustDesc)
                    })
                  
                  output$table_4 <- DT::renderDataTable({
                    df_result_4()
                  },escape=FALSE,options = list(lengthChange = FALSE)
                  )
                  
                  df_result_5 <- eventReactive(
                    input$button5,{
                      getRecoForCustandProd(custId=0, clustDesc = input$clustDesc, prodDesc=input$prodDescForClust)
                   })
                  
                  output$table_5 <- DT::renderDataTable({
                    df_result_5()
                  },escape=FALSE,options = list(lengthChange = FALSE)
                  )
                  
                  output$text5 <- renderUI ({
                      str9 <- paste("________________________________________________________________________________________________________________________________________________________________ ")
                      str10 <- paste("Cluster Name --------------- Cluster Description")
                      str11 <- paste("Upscale Buyers ------------- Buy most expensive items (high price affinity), but infrequent, low vertical diversity")
                      str12 <- paste("On the brink customers --- Inactive customers with lowest spend and have the highest probability of lapsing ")
                      str13 <- paste("Average Joe	----------------- Active buyers with medium frequency and spend")
                      str14 <- paste("Bottom of the Barrel------- Infrequent single time buyers with lowest GMV and transaction frequency, no high value purchase")
                      str15 <- paste("Top of the Hill--------------- Most frequent and active transactors with highest revenue contributing cluster")
                      str16 <- paste("_______________________________________________________________________________________________________________________________________________________________ ")
                      HTML(paste(str9, str10, str11, str12,str13, str14, str15, str16, sep = '<br/>'))
                    })
                  
                  output$text2 <- renderUI({
                    str17 <- paste("Product specific recommendations about which products have affinity of being purchased together.")
                    str18 <- paste("Product specific recommendations to give targeted discounts & to bundle products together")
                  })
                  
                  output$text3 <- renderUI({
                    str19 <- paste("Customer specific recommendations based on the cluster he belongs to and the purchase pattern of that customer.")
                   })
                  
                  output$text4 <- renderUI({
                    str20 <- paste("Cluster specific recommendations that can be used for Mass marketing campaigns for the all the customers that fall into a specific cluster. ")
                  })
  
}

shinyApp(ui = ui, server = server)
                           