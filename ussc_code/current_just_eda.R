library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

# focusing on data analysis for the cases resided over by all current justices
load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/current_justices.RData")

#
#looking at the issue areas of the cases for the USSC
#
current_justices$date <- as.Date(current_justices$date)
current_justices %>%
    mutate(year = as.factor(year(date))) %>% 
        group_by(year, issueArea) %>%
          summarise(count = n()) %>% 
      ggplot(aes(issueArea, count, fill = year)) + 
        geom_bar(stat = "identity") +
          theme(axis.text.x = element_text(angle = 90))
#
#looking at Percentage of majority votes for case decisions
#
unique(current_justices[, c("caseName", "dateDecision", "majVotes", "minVotes")]) %>%
    mutate(maj_fac = as.factor(majVotes)) %>%
      group_by(maj_fac) %>% summarise(count = n()) %>%
        mutate(perc = round(count/(sum(count)), 3)) %>%
      ggplot(aes(maj_fac, perc)) + 
        geom_bar(stat = "identity") +
        labs(x = "Number of Votes for the Majority in the Case Decision",
          y = "Percentage of Overall Cases",
          title = "Percentage of Majority Votes for Cases of the Current USSC")
#
#looking at the percentages the justices end up being on the majority side in 5-4 decisions
#
tight_cases <- current_justices %>% filter(majVotes == 5)
vote_fac <- c("Dissent", rep("Other", 5), "Voted with Majority")
names(vote_fac) <- levels(tight_cases$vote)
tight_cases$Vote_Type <- factor(vote_fac[tight_cases$vote], 
                                levels = c("Voted with Majority",
                                           "Dissent",
                                           "Other"))
tight_cases %>% 
      group_by(justiceName, Vote_Type) %>% 
        tally() %>%
        group_by(justiceName) %>%
          mutate(perc = round(n/sum(n), 3)) %>%
            arrange(justiceName, desc(perc)) %>%
        ggplot(aes(justiceName, perc, fill = Vote_Type)) + 
          geom_bar(stat = "identity", position = "dodge") +
          theme(axis.text.x = element_text(angle = 90)) +
            labs(x = "Justice Name",
                y = "Percentage of 5-4 Decision Cases",
                title = "Vote Type Percentages of 5-4 Cases\nfor Current Justices")

#
#looking at Anthony Kennedy's 5-4 voting decisions
#
tight_cases %>% 
      filter(justiceName == "AMKennedy", vote == "Voted with Majority or Plurality") %>%
            group_by(direction) %>% 
            summarise(count = n()) %>%
              mutate(perc = round(count/sum(count), 3)) %>%
          ggplot(aes(direction, perc)) + 
            geom_bar(stat = "identity") +
            labs(x = "Vote Direction",
                  y = "Percentage of 5-4 Decision Cases",
                  title = "Anthony Kennedy's Vote in 5-4 Decision Cases")

#
# looking at cases where the decisions had either 8 or 9 majority votes
#

issue_count_89 <- current_justices %>% 
                    filter(majVotes %in% c(8,9)) %>%
                      group_by(issueArea) %>% 
                        summarise(count = n()) %>%
                            rbind.data.frame(list("Interstate Relations", 0))

issue_count <- current_justices %>% 
                group_by(issueArea) %>% 
                    summarise(all_count = n())

issue_area_agree_perc <- 
        inner_join(issue_count_89, issue_count, by = "issueArea") %>%
            mutate(perc_89 = round(count/sum(count), 3),
                    perc_all = round(all_count/sum(all_count), 3),
                      ratio = round(count/all_count, 3)) %>%
              melt(id = c("issueArea", "count", "all_count"))

issue_area_agree_perc %>%
          ggplot(aes(issueArea, value, fill = variable)) +
            geom_bar(stat = "identity", position = "dodge") +
              theme(axis.text.x = element_text(angle = 90)) +
              labs(x = "Issue Area",
                   y = "Percentages",
                   title = "Comparing Issue Area Frequencies for\nAgreeable Cases (8 or 9 MajVotes), All Cases, and the Ratio\nBetween the Two For Cases of Current Justices")

# same code as above but looking at issue Area freqs for 5-4 decisions instead
issue_count_5 <- current_justices %>% 
    filter(majVotes == 5) %>%
      group_by(issueArea) %>% 
        summarise(count = n()) %>%
          rbind.data.frame(list("Interstate Relations", 0), 
                            list("Miscellaneous", 0))

issue_count <- current_justices %>% 
                group_by(issueArea) %>% 
                  summarise(all_count = n())

issue_area_agree_perc <- 
  inner_join(issue_count_5, issue_count, by = "issueArea") %>%
      mutate(perc_5 = round(count/sum(count), 3),
                perc_all = round(all_count/sum(all_count), 3),
                  ratio = round(count/all_count, 3)) %>%
                melt(id = c("issueArea", "count", "all_count"))

issue_area_agree_perc %>%
        ggplot(aes(issueArea, value, fill = variable)) +
          geom_bar(stat = "identity", position = "dodge") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(x = "Issue Area",
              y = "Percentages",
                title = "Comparing Issue Area Frequencies for 5 MajVote Cases, All Cases,\nand the Ratio Between the Two For Cases of Current Justices")
