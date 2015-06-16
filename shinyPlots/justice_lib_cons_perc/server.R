library(ggplot2)
library(shiny)
library(dplyr)

load("../../data/issueA_justices.RData")

shinyServer(function(input, output){
  output$plot <- renderPlot({
        justices <- issueA_justices %>%
            mutate(Percentage = round(perc * 100)) %>%
                      filter(justiceName == input$name)
        justice_var <- switch(input$perc_count,
                              Percentage = justices[, c("issueArea", "direction", "Percentage")],
                              Counts = justices[, c("issueArea", "direction", "n")])
        names(justice_var) <- c("issueArea", "direction", "Var")
        gg <- justice_var %>%
                ggplot(aes(issueArea, Var, fill = direction)) +
                  geom_bar(stat = "identity", position = "dodge") +
                  geom_text(aes(label = Var), vjust = -0.67, 
                            size = 3, position = position_dodge(width = 1.1)) +
                    theme(axis.text.x = element_text(angle = 90)) +
                      labs( x = "Issue Area",
                            y = paste(input$perc_count, "of Cases"),
                            title = paste("Voting Direction", input$perc_count, "for",
                                          input$name, sep = " "))
              print(gg)
  })
  
})