library(shiny)
library(dplyr)
library(ggplot2)


shinyServer(function(input, output){
      output$plot <- renderPlot({
        filt_issue <- issue_perc_year %>% filter(issueArea %in% input$issue_var)
        ggp <- filt_issue %>% 
                    ggplot(aes(des_term_year, issue_perc * 100, 
                               colour = issueArea, group = issueArea)) +
                                  geom_line() + geom_point()
                                  labs(x = "Term Year of Case Decision",
                                       y = "Percentage of Overall Cases (0-100%)"
                                       )
        print(ggp)
      })
  
})