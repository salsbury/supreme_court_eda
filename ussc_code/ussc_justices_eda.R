library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

#
#
#looking at all SC Justices in this database
#
load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/scdb_mod.RData")

#
#
#
# USSC EDA
#
#
#

#
#looking at months the cases are decided
#
sc_justices$dateDecision <- as.Date(sc_justices$dateDecision)
sc_justices %>% mutate(month = as.factor(month(dateDecision))) %>% 
                group_by(month) %>% summarise(count = n()) %>%
                    ggplot(aes(month, count)) + geom_bar(stat = "identity") +
                        ggtitle("Number of Cases Decided by the USSC in Each Month")
#
#looking at months cases are orally argued
#
sc_justices %>% mutate(month = as.factor(month(dateArgument))) %>% 
                  group_by(month) %>% summarise(count = n()) %>%
                    ggplot(aes(month, count)) + 
                        geom_bar(stat = "identity") +
                          ggtitle("Number of Cases Argued in Front of the USSC in Each Month")

#
#looking at the number of days it takes to decide cases for the USSC
#
cases_dates<- sc_justices %>% 
                mutate(days_to_decide = as.numeric(dateDecision - dateArgument)) %>% 
                  select(caseId, issueArea, dateDecision, dateArgument, days_to_decide)
case_date <- unique(cases_dates)
case_date <- case_date[-(which(case_date$days_to_decide > 1500)),]

#
#days the USSC takes to decide cases separated by month of date argued
#
na.omit(case_date) %>% mutate(month = as.factor(month(dateArgument))) %>% 
                ggplot(aes(month, days_to_decide)) + geom_boxplot() +
                    labs( x = "The Month the Case was Orally Argued",
                        y = "Number of Days Until Decision",
                          title = "Number of Days the USSC Takes to\nDecide Cases Separated by Month")

#
#days the USSC takes to decide cases separated by month of date decided
#
na.omit(case_date) %>% mutate(month = as.factor(month(dateDecision))) %>% 
              ggplot(aes(month, days_to_decide)) + geom_boxplot() +
                labs( x = "The Month the Case was Decided",
                    y = "Number of Days Until Decision",
                      title = "Number of Days the USSC Takes to\nDecide Cases Separated by Month")
#
#days the USSC takes to decide cases separated by issue area
#
na.omit(case_date) %>% 
        ggplot(aes(issueArea, days_to_decide)) + 
            geom_boxplot() +
              labs( x = "Area of Case Issue",
                y = "Number of Days Until Decision",
                  title = "Number of Days the USSC Takes to\nDecide Cases Separated by Issue Area") +
                    theme(axis.text.x = element_text(angle = 90))

#
# looking at the cases with a long decision time (greater or equal to 180 days)
#
# looking at the issue areas of these cases to see if any area is obviously more
# likely to take longer to decide

long_cases <- case_date$caseId[case_date$days_to_decide >= 180]
long_case_just <- sc_justices[sc_justices$caseId %in% long_cases,]
long_case_counts <- long_case_just %>%
                      group_by(issueArea) %>% summarise(long_count = n())
long_case_counts <- rbind.data.frame(long_case_counts, list("Priv. Act.", 0))

sc_just_count <- sc_justices %>% 
                    group_by(issueArea) %>% 
                      summarise(sc_count = n())

case_counts_perc <- inner_join(long_case_counts, sc_just_count) %>% 
            mutate(Perc_in_LD_Cases = round(long_count/(sum(long_count)), 3),
                   Perc_in_All_Cases= round(sc_count/(sum(sc_count)), 3),
                   Ratio_LD_Over_All= round(long_count/sc_count,3)
                   ) %>% 
                    melt(id = c("issueArea", "long_count", "sc_count"))
names(case_counts_perc) <- c("issue_Area", "long_count", "sc_count", "Variables_for_Perc", "perc")

case_counts_perc %>% 
            ggplot(aes(issue_Area, perc, fill = Variables_for_Perc)) + 
                geom_bar(stat = "identity", position = "dodge") +
                  theme(axis.text.x = element_text(angle = 90)) +
                    labs(x = "Issue Area", y = "Percentage",
                         title = "Comparing Issue Area Frequencies for Long\nDecision Cases, All Cases, and the Ratio Between the Two")


#converting the dates of the cases into term periods
#shiny plot created using issue_perc_year
des_dates <- sc_justices$dateDecision
sc_justices$des_term_year <- ifelse(month(des_dates) %in% c(1:8), year(des_dates) - 1, year(des_dates))
issue_count_year<- sc_justices %>% 
                      group_by(des_term_year, issueArea) %>% 
                          summarise(counts = n())
perc_year <- issue_count_year %>% group_by(des_term_year) %>%
                      summarise(sum_count = sum(counts))

issue_perc_year <- inner_join(issue_count_year, perc_year, by= c("des_term_year")) %>%
                        mutate(issue_perc = round(counts/sum_count, 3))

# data wrangling to create data frame for shiny plot justice_lib_cons_perc
justice_dir_per <- sc_justices %>%
                      group_by(justiceName, )

