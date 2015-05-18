library(shiny)


shinyUI(fluidPage(
      titlePanel("Issue Area Percentages of All Cases Over Time"),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("issue_var", "Issue Area Variables",
                             c("Civil Rights", "Criminal Procedure",
                               "Due Process", "Economic Activity",
                               "Federal Taxation", "Federalism",
                               "First Amendment", "Interstate Relations",
                               "Judicial Power", "Privacy", "Unions",
                               "Attorneys", "Miscellaneous", "Private Action"),
                             selected = c("Civil Rights"))        
                        ),
        mainPanel(plotOutput("plot"))
          )
        )
      )
