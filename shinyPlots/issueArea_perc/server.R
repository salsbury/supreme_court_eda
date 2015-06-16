library(shiny)
library(dplyr)
library(ggplot2)

load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/scdb_mod.RData")

des_dates <- sc_justices$dateDecision
sc_justices$des_term_year <- ifelse(month(des_dates) %in% c(1:8), year(des_dates) - 1, year(des_dates))
issue_count_year<- sc_justices %>% 
  group_by(des_term_year, issueArea) %>% 
  summarise(counts = n())
perc_year <- issue_count_year %>% group_by(des_term_year) %>%
  summarise(sum_count = sum(counts))

issue_perc_year <- inner_join(issue_count_year, perc_year, by= c("des_term_year")) %>%
  mutate(issue_perc = round(counts/sum_count, 3))


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