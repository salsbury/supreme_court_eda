library(ggplot2)
library(plyr)
library(dplyr)
library(png)
library(grid)

load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/current_justices.RData")


#looking at change in liberalism/conservatism of the justices over time 
cj_dir_rmna<- current_justices[!is.na(current_justices$direction),]
cj_dir_arrange <- cj_dir_rmna %>% mutate(dir_num = ifelse(direction == "Liberal", 1, -1)) %>%
                      arrange(justiceName, dateDecision)
cj_dir_arrange$dir_cumsum <- unlist(by(cj_dir_arrange, cj_dir_arrange$justiceName, function(x) cumsum(x$dir_num)))


img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/s_soto.png")
s_soto <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/rbg2.png")
rbg <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/e_kag.png")
e_kag <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/s_brey.png")
s_brey <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/a_ken.png")
a_ken <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/j_roberts.png")
j_rob <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/scalia.png")
scal <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/thomas.png")
thomas <- rasterGrob(img, interpolate=TRUE)

img <- readPNG("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/sc_just_pics/alito.png")
alito <- rasterGrob(img, interpolate=TRUE)

cj_dir_arrange %>% ggplot(aes(dateDecision, dir_cumsum, 
                              colour = justiceName, group = justiceName)) +
                            geom_line() + 
                              scale_x_date(limits=c(as.Date("2010-11-08"), 
                                                    as.Date("2014-09-01"))) +
                          labs(x = "Date When Case Decision was Made (2010-11-08, 2014-07-01)",
                               y = "Direction of Vote (+1 for Liberal, -1 for Conservative)",
                               title = "Cumulative Sum of Direction of Votes Over Time\n# of Cases: 309") +
                          annotation_custom(s_soto, xmax = 17700,
                                            ymin = 67, ymax = 79) +
                          annotation_custom(rbg, xmax = 17700,
                                            ymin = 54, ymax = 68) +
                          annotation_custom(e_kag, xmax = 17700,
                                            ymin = 43, ymax = 58) +
                          annotation_custom(s_brey, xmax = 17700,
                                            ymin = 20, ymax = 34) +
                          annotation_custom(a_ken, xmax = 17700,
                                            ymin = -45, ymax = -31) +
                          annotation_custom(j_rob, xmax = 17700,
                                            ymin = -52, ymax = -43) +
                          annotation_custom(scal, xmax = 17700,
                                            ymin = -68, ymax = -53) +
                          annotation_custom(thomas, xmax = 17700,
                                            ymin = -87, ymax = -72) +
                          annotation_custom(alito, xmax = 17700,
                                            ymin = -97, ymax = -84)