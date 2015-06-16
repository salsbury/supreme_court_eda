library(ggplot2)
library(shiny)

shinyUI(fluidPage(
      titlePanel("Voting Percentages for the Justice"),
      sidebarLayout(
        sidebarPanel(
          selectInput("name", "Supreme Court Justice",
                      c("AFortas", "AJGoldberg", "AMKennedy", "AScalia", "BRWhite", "CEWhittaker",
                        "CThomas", "DHSouter", "EKagan", "EWarren", "FFrankfurter", "FMVinson",
                        "FMurphy", "HABlackmun", "HHBurton", "HLBlack", "JGRoberts", "JHarlan2",
                        "JPStevens", "LFPowell", "PStewart", "RBGinsburg", "RHJackson", "SAAlito",
                        "SDOConnor", "SFReed", "SGBreyer", "SMinton", "SSotomayor", "TCClark",
                        "TMarshall", "WBRutledge", "WEBurger", "WHRehnquist", "WJBrennan", "WODouglas"),
                      selected = c("AFortas")),
          radioButtons("perc_count", "Select either Percentage of Cases or Counts.",
                       choices = c("Percentage", "Counts"))
                      ),
        mainPanel(plotOutput("plot"))
        )
      )
      )