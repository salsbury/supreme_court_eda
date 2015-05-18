library(ggplot2)
library(plyr)
library(dplyr)
library(ggvis)

load("C:/Users/Sally/Desktop/data_analysis/supreme_court_eda/data/scdb_mod.RData")

issueA_justices <- 
          sc_justices %>%
            group_by(justiceName, issueArea, direction) %>%
              tally() %>% na.omit() %>%
              group_by(justiceName, issueArea) %>%
                mutate(perc = round(n/sum(n), 3))

#
# code to add in missing directions (when the opposite direction is 100%)
# i.e. if 100% of a justice's decisions in an issue area is conservative, liberal
# decisions will be missing
#
other_dir_missing<- issueA_justices[which(issueA_justices$perc == 1.000),]

add_missing_dir <- function(df_rw){
          
          if(df_rw$direction == "Liberal"){
            df_rw[3] = "Conservative"
            df_rw[4] = 0
            df_rw[5] = 0
            return(df_rw)
          }
          else{
            df_rw[3] = "Liberal"
            df_rw[4] = 0
            df_rw[5] = 0
            return(df_rw)
          }
        }

new_row_list <- lapply(1:nrow(other_dir_missing), 
                    function(i) add_missing_dir(other_dir_missing[i,]))
new_row_df <- do.call(rbind, new_row_list)
issueA_justices <- rbind.data.frame(issueA_justices, new_row_df)

# adding in the missing issueAreas for the justices
# missing b/c the justice did not have any case with that issue Area
add_missing_iss <- function(name){
  # input: a character vector of length one containing the justice's name
  # output: a data frame with 5 columns containing the justice's name,
  #         the missing issue areas, direction of conservative/liberal,
  #         the counts (which would be 0), and the perc (which is also 0)
  #
            just_issue_df <- issueA_justices %>% 
                            filter(justiceName == name) %>%
                            select(issueArea) %>%
                              unique()
            just_issue <- unlist(just_issue_df[,2])
            uniq_issue <- unique(issueA_justices$issueArea)
            missing_iss <- uniq_issue[!(uniq_issue %in% just_issue)]
            len_mi <- length(missing_iss)
            cbind.data.frame(justiceName = rep(name, len_mi),
                             issueArea = rep(missing_iss, 2),
                             direction = c(rep("Conservative", len_mi),
                                           rep("Liberal", len_mi)),
                             n = rep(0, len_mi *2),
                             perc = rep(0, len_mi *2))
                            
}

justice_name_miss <- names(which(table(issueA_justices$justiceName) < 26))
justice_row_list <- lapply(justice_name_miss, function(i) add_missing_iss(i))
justice_row_df <- do.call(rbind, justice_row_list) %>% tbl_df()
issueA_justices <- rbind.data.frame(issueA_justices, justice_row_df) %>%
                    as.data.frame() %>%
                    arrange(justiceName, issueArea, direction)

save(issueA_justices, file = "issueA_justices.RData")

iaj_dir_perc <- reshape(issueA_justices, idvar = c("justiceName", "issueArea"), 
                        timevar = c("direction"), direction = "wide") %>%
                mutate(case_count = n.Conservative + n.Liberal) %>%
                select(justiceName, issueArea, case_count, 
                        conserv_perc = perc.Conservative)

iaj_cons_weight <- iaj_dir_perc %>% group_by(justiceName) %>%
            mutate(case_perc = round(case_count/sum(case_count),3)) %>%
            mutate(conserv_perc_weighted = case_perc * conserv_perc) %>%
            select(justiceName, issueArea, con_per_w = conserv_perc_weighted) %>%
            as.data.frame()

iaj_votes_wide<- reshape(iaj_cons_weight, idvar = "justiceName",
                         timevar = "issueArea", direction = "wide")


#code to find principal components of the justice vote data set
iaj_pca <- princomp(iaj_votes_wide[, -1], scale = TRUE)
plot(iaj_pca, type ='l')

data_times_pca <- as.matrix(iaj_votes_wide[, -1]) %*% iaj_pca$loadings[, 1:2]
iaj_data_pca <- cbind.data.frame(justiceNames = iaj_votes_wide[, 1],data_times_pca)

# code to cluster the justices based on the first two principal components
iaj_pca_kmeans <- kmeans(data_times_pca, 4)
iaj_data_pca$cluster <- as.factor(iaj_pca_kmeans$cluster)
iaj_data_pca$id <- 1:nrow(iaj_data_pca)
cluster_center <- cbind.data.frame(iaj_pca_kmeans$centers,cluster_fac = as.factor(1:nrow(iaj_pca_kmeans$centers)))

ggplot() + geom_point(aes(Comp.1, Comp.2, color = cluster), iaj_data_pca) +
  geom_point(aes(Comp.1, Comp.2), 
             data = cluster_center, size = 10, shape = '*') +
  labs(x = "Principal Component 1 (72.98% of Var)",
       y = "Principal Component 2 (18.49% of Var)",
       title = "k-Mean Clustering of SC Justices\n(Principal Components of their Voting Stats)")

#code for ggvis version
tool_func <- function(df){
  if(is.null(df)) return(NULL)
  newdf <- iaj_data_pca[iaj_data_pca$id == df$id, ] 
        newdf$justiceName}

iaj_data_pca %>% ggvis(~Comp.1, ~Comp.2, key := ~id) %>% 
  layer_points(fill = ~cluster, size = 2) %>% add_tooltip(tool_func)