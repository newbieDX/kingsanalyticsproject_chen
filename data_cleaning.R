library(tidyverse)
library(dplyr)
library(Hmisc)
library(stringr)
library(magrittr)
library(eeptools)
# Read csv files

inter_player <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/international_box_player_season.csv") %>% as.data.frame()
nba_player <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/nba_box_player_season.csv") %>% as.data.frame()
player_lst <- read.csv(file = "C:/Users/Derek Chu/Desktop/application_project_chen/csv_files/player.csv")%>% as.data.frame()


# Capitalize name for inter_player and player_lst

inter_player$first_name <- capitalize(inter_player$first_name)
inter_player$last_name <- capitalize(inter_player$last_name)

player_lst$first_name <- capitalize(player_lst$first_name)
player_lst$last_name <- capitalize(player_lst$last_name)

# Exclude NA columns

df1 <- inter_player[, colSums(is.na(inter_player)) != nrow(inter_player)]
df2 <- nba_player[, colSums(is.na(nba_player)) != nrow(nba_player)]

# Exclude players only play in the NBA


df1 <- left_join(df1, player_lst, by = "first_name")
df1 <- df1[c(1, 53, 54, 3:52)]
colnames(df1)[2] <- "last_name"
df1$name <- paste(df1$first_name, df1$last_name)


df2 <- inner_join(df2, player_lst, by = "first_name")
df2 <- df2[c(1, 56, 57, 3:55)]
colnames(df2)[2] <- "last_name"
df2$name <- paste(df2$first_name, df2$last_name)


# Uniform features for both data frames in order to use together

df2 <- df2[-c(12,42,43)]


# Subset players who play for European leagues as test set

df_euro_test <- subset(df1, df1$season == "2021")
df_euro_train <- subset(df1, df1$season != "2021")

df_total <- rbind(df_euro_train, df2) %>% arrange(name, season)
df_total <- df_total[c(54, 3:53)]

rownames(df_total) <- NULL

df_success <- left_join(df_euro_train, df2, by = "name")

df3 <- as.data.frame(table(df_success$name))

namelist <- df3$Var1

# Create a dummy variable to label success players

df_66 <- subset(df_total, name %in% namelist)
df_66$is_nba <- df_66$league == "NBA"
name_unique <- unique(df_66$name)

success_player_train <- data.frame()

# Select the most recent season before go to NBA

for (player in name_unique){
  sub_df <- subset(df_66, df_66$name == player)
  sub_df %<>% arrange(season)
  cumsum_vec <- cumsum(sub_df$is_nba)
  first_nba <- min(which(cumsum_vec == 1))
  if ((first_nba > 1) & (first_nba != Inf)){
    sub_df_year  = sub_df[first_nba-1,]$season
    sub_df_same_year <- sub_df %>% filter(season==sub_df_year)
    success_player_train <- rbind(success_player_train,sub_df_same_year)
  }
}

success_player_train <- subset(success_player_train, success_player_train$league != "NBA")

df_euro_train <- df_euro_train[c(54, 3:53)]

success_player_train$is_nba <- 1 

df77 <- left_join(df_euro_train, success_player_train[c(1, 3, 5, 53)], by = c("name","season", "league"))

df77$is_nba[is.na(df77$is_nba)] <-  0

df_train <- df77

# Replace NAs with 0

df_train[is.na(df_train)] <- 0
df_test <- df_euro_test[c(54, 3:53)]

df_train$is_nba <- as.factor(df_train$is_nba)

df_train <- df_train %>% mutate(age = age_calc(as.Date(birth_date),as.Date(as.character(df_train$season), format = "%Y"), units = "years")) 
df_train$age <- round(df_train$age, digits = 0)
df_train <- df_train[c(1, 54, 3, 5, 7:53)]

df_test <- df_test %>% mutate(age = age_calc(as.Date(birth_date),as.Date(as.character(df_test$season), format = "%Y"), units = "years")) 
df_test$age <- round(df_test$age, digits = 0)
df_test <- df_test[c(1, 53, 3, 5, 7:52)]

# Another way to get the training set

# df_euro_train$name_bool=FALSE
# df_euro_train <- df_euro_train  %>% mutate(name_bool=ifelse(df_euro_train$name %in% unique(success_player_train$name),1,0))
# 
# df_euro_train$test=FALSE
# update_cri=df_euro_train$test
# 
# for (player in unique(df_euro_train$name)){
# 
#   sub_df<-df_euro_train %>% filter(name==player)
#   name_bool_vec<-sub_df %>% `$`(name_bool)
#   
#   if ((0 %in% name_bool_vec) == FALSE){
# 
#     cri1=success_player_train %>% filter(name==player) %>% `$`(season)
#     cri2<-((df_euro_train$season %in% unique(cri1)) & (df_euro_train$name==player)) 
#     update_cri=update_cri | cri2
#     #print(sum(update_cri))
#   }
# }
# 
# df_euro_train <- df_euro_train  %>% mutate(is_nba=ifelse(update_cri,1,0))

# df_euro_train <- df_euro_train[-c(54:56)]

