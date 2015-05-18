library(rvest)
library(ggplot2)
library(magrittr)
library(dplyr)

load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/scdb_2014.RData")
sc_justices <- SCDB_2014_01_justiceCentered_Citation
rm(SCDB_2014_01_justiceCentered_Citation)

# for the issue variable, need to scrap the issue table from this website
# http://supremecourtdatabase.org/documentation.php?var=issue
# done so that instead of just an ID, you can see the general idea of the case

issues_html <- html("http://supremecourtdatabase.org/documentation.php?var=issue") %>%
                  html_nodes("table")

#using the html from above, extract the 3rd table and then take the 2nd table child
issues_table <- issues_html %>% extract2(3) %>% html_nodes("table") %>% 
                    extract2(2) %>% html_table()
names(issues_table) <- c("issueID", "issue_descrip")
issues_table$issue_descrip <- sapply(strsplit(issues_table$issue_descrip, "((\t)+|\\()"), function(i) i[1])
iss_descrip <- issues_table$issue_descrip
names(iss_descrip) <- as.character(issues_table$issueID)
sc_justices$issue <- iss_descrip[as.character(sc_justices$issue)]

#
#
# changing variables that are integer numbers into named factors
#
#

#issue area
short_issue <- c("Crim. Proc.", "Civil Rights", "1st Amd", "Due Process",
                 "Privacy", "Attorneys", "Unions", "Econ. Act.", "Jud. Power",
                 "Federalism", "Interstate Rel.", "Fed. Tax", "Misc.", "Priv. Act.")
names(short_issue) <- as.character(1:14)
sc_justices$issueArea <- short_issue[as.character(sc_justices$issueArea)]

#decision Direction
desc_dir <- c("Conservative", "Liberal", "Unspecified")
names(desc_dir) <- as.character(1:3)
sc_justices$decisionDirection <- as.factor(desc_dir[as.character(sc_justices$decisionDirection)])

#party winning
party_win <- c("Unfavorable for Petitioner", "Favorable for Petitioner", "Unclear")
names(party_win) <- as.character(0:2)
sc_justices$partyWinning <- as.factor(party_win[as.character(sc_justices$partyWinning)])

#vote
vote_vals <- c("Voted with Majority or Plurality", "Dissent",
               "Regular Concurrence", "Special Concurrence", "Judgement of the Court",
               "Dissent from a Denail of Cert.", "Jurisdictional Dissent", "Justice in Equally Divided Vote")
names(vote_vals) <- as.character(1:8)
sc_justices$vote <- as.factor(vote_vals[as.character(sc_justices$vote)])

#opinion
op <- c("Did not Write Opinion", "Wrote Opinion", "Co-Authored Opinion")
names(op) <- as.character(1:3)
sc_justices$opinion <- as.factor(op[as.character(sc_justices$opinion)])

#direction
direct <- c("Conservative", "Liberal")
names(direct) <- as.character(1:2)
sc_justices$direction <- as.factor(direct[as.character(sc_justices$direction)])

#majority
maj <- c("Dissent", "Majority")
names(maj) <- as.character(1:2)
sc_justices$majority <- as.factor(maj[as.character(sc_justices$majority)])


#looking at the justices currently on the USSC
# includes only cases since Nov 8, 2010 (Kagan's first case/decision)
current_justices <- sc_justices %>% 
  filter(justiceName %in% c("AScalia", "AMKennedy", "CThomas",
                            "RBGinsburg", "SGBreyer", "JGRoberts", 
                            "SAAlito", "SSotomayor", "EKagan")) %>%
  select(justiceName, justice, caseName ,dateDecision, issue, issueArea, decisionDirection,
         partyWinning, majOpinWriter, majVotes, minVotes, vote, opinion, direction, majority) %>%
  filter(dateDecision >= "2010-11-08")

#majOpinwriter
justices <- c("AScalia", "AMKennedy", "CThomas",
              "RBGinsburg", "SGBreyer", "JGRoberts", 
              "SAAlito", "SSotomayor", "EKagan")
names(justices) <- as.character(c(105, 106, 108, 109, 110, 111, 112, 113, 114))
current_justices$majOpinWriter <- justices[as.character(current_justices$majOpinWriter)]
current_justices$majOpinWriter[is.na(current_justices$majOpinWriter)] = "No Opinion Writer"
