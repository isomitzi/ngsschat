#-----------------------
# 1. setting up
#-----------------------

setwd("~/Dropbox/research/ngsschat") # change to local dir

options(stringsAsFactors = F)

data <- read.csv("data/all_storify_data.csv", stringsAsFactors = F)

str(data)

data$time <- lubridate::ymd_hms(data$time)

data$year <- lubridate::year(data$time)

data <- dplyr::filter(data, year == 2014 | year == 2015)

data <- dplyr::mutate(data, year_bool = ifelse(year == 2014, 0, 
                                               ifelse(year == 2015, 1, NA)))

str(data)

data_2014 <- data[data$year_bool == 0, ]
data_2015 <- data[data$year_bool == 1, ]

nrow(data_2014)
nrow(data_2015)

bool <- data_2015$username %in% data_2014$username
table(bool)

data_2015 <- data_2015[bool, ]

data_all <- rbind(data_2014, data_2015)

# for 2014
edges <- c()
for (i in 1:length(data_2014$text)) {
      #for each tweet get a list of accounts mentioned
      #just extract any string that matches a username regex
      mentions = unlist(str_extract_all(tolower(data_2014$text[i]),"@[a-z0-9_]{2,15}"))
      if (length(mentions)!=0) {
            for (j in 1:length(mentions)) {
                  if(data_2014$username[i]!="" && substring(mentions[j],2)!="" && !is.na(mentions)) { #needed for when parser borks
                        #add the tweeter and the mentionee to the edge list
                        edges = c(edges, c(tolower(data_2014$username[i]), substring(mentions[j],2)))
                  }
            }
      }
}
edgematrix_2014 <- as.matrix(t(matrix(edges, nrow = 2)))
df_2014 <- data.frame(sort(table(edgematrix_2014[, 2])), stringsAsFactors = F)
names(df_2014) <- c("username", "count")
df_2014$username <- as.character(df_2014$username)
data_2014$username <- tolower(data_2014$username)
data_2014_merge <- dplyr::left_join(data_2014, df_2014, by = "username")

# for 2015
edges <- c()
for (i in 1:length(data_2015$text)) {
      #for each tweet get a list of accounts mentioned
      #just extract any string that matches a username regex
      mentions = unlist(str_extract_all(tolower(data_2015$text[i]),"@[a-z0-9_]{2,15}"))
      if (length(mentions)!=0) {
            for (j in 1:length(mentions)) {
                  if(data_2015$username[i]!="" && substring(mentions[j],2)!="" && !is.na(mentions)) { #needed for when parser borks
                        #add the tweeter and the mentionee to the edge list
                        edges = c(edges, c(tolower(data_2015$username[i]), substring(mentions[j],2)))
                  }
            }
      }
}
edgematrix_2015 <- as.matrix(t(matrix(edges, nrow = 2)))
df_2015 <- data.frame(sort(table(edgematrix_2015[, 2])))
names(df_2015) <- c("username", "count")
df_2015$username <- as.character(df_2015$username)
data_2015$username <- tolower(data_2015$username)
data_2015_merge <- dplyr::left_join(data_2015, df_2015, by = "username")

str(data_2014_merge)
data_2014_merge
str(data_2015_merge)

data_all <- rbind(data_2014_merge, data_2015_merge)

str(data_all)

out<- 
      data_all %>% 
            group_by(username, year_bool) %>% 
            summarize(count = n())

str(out)

View(out)

str(df_2014)

names(df_2015) <- c("username", "mentioned_2015")

out_ss <- dplyr::left_join(out, df_2014, by = "username")

out_ss1 <- dplyr::left_join(out, df_2015, by = "username")
str(out_ss1)
View(out_ss1)

tmp_bool <- is.na(out_ss1$mentioned_2015)
tmp_bool
str(tmp_bool)
out_ss1 <- data.frame(out_ss1)
str(out_ss1)
out_ss1$mentioned_2015[tmp_bool] <- 0

out_ss1 %>% 
      group_by(year_bool) %>% 
      summarize(count = mean(count))

summary(lm(count ~ year_bool, data = out_ss1))

# library(stringr)
# edges <- c()
# for (i in 1:length(data_all$text)) {
#       #for each tweet get a list of accounts mentioned
#       #just extract any string that matches a username regex
#       mentions = unlist(str_extract_all(tolower(data_all$text[i]),"@[a-z0-9_]{2,15}"))
#       if (length(mentions)!=0) {
#             for (j in 1:length(mentions)) {
#                   if(data_all$username[i]!="" && substring(mentions[j],2)!="") { #needed for when parser borks
#                         #add the tweeter and the mentionee to the edge list
#                         edges = c(edges, c(tolower(data_all$username[i]), substring(mentions[j],2)))
#                   }
#             }
#       }
# }

data_all %>% 
      group_by(username, year) %>% 
      summarize(count = n())

adlibrary(dplyr)

data %>% 
      group_by(username, year_bool) %>% 
      summarize(count = n()) %>% 
      group_by(year_bool) %>% 
      summarize(m_count = mean(count))
      

data$year_bool

data <- read.csv("~/Dropbox/research/ngsschat/Wanli Result/34 topics and 28 topics/28 topics/assignment.csv")

str(data)

topic_and_words <- read.csv("~/Dropbox/research/ngsschat/Wanli Result/34 topics and 28 topics/28 topics/topic and words.csv")

topic_and_words

data <- read.csv("~/Dropbox/research/ngsschat/Wanli Result/34 topics and 28 topics/34 topics/assignment.csv")

data %>% 
      group_by(topic) %>%
      
      

topic_and_words <- read.csv("~/Dropbox/research/ngsschat/Wanli Result/34 topics and 28 topics/28 topics/topic and words.csv")

topic_and_words
