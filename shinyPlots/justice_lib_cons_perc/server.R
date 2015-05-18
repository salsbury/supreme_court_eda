library(ggplot2)
library(shiny)
library(dplyr)

#uses data issueA_justices (load it up)

shinyServer(function(input, output){
  output$plot <- renderPlot({
        justice <- issueA_justices %>%
                      filter(justiceName == input$name)
        gg <- justice %>%
                ggplot(aes(issueArea, perc * 100, fill = direction)) +
                  geom_bar(stat = "identity", position = "dodge") +
                    theme(axis.text.x = element_text(angle = 90)) +
                      labs( x = "Issue Area",
                            y = "Percentage of Cases",
                            title = paste("Voting Direction Percentages for",
                                          input$name, sep = " "))
              print(gg)
  })
  
})