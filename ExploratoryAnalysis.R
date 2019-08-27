library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggtern)
library(Cairo)
library(magick)
library(stringr)
library(purrrlyr)
library(botcheck)
library(httr)
library(xml2)
library(RJSONIO)


#import data
data <- read_csv("woTrump1018 - trump_red [Nodes].csv")
tweets <- read_csv("pol_clust_together.csv")

#create objects for each modularity grouping of users
left <- data[data$modularity_class == 13, 2]
right <- data[data$modularity_class == 1, 2]
suspect <- data[data$modularity_class == 8, 2]
#add to list
groups <- list("left" = left, "right" = right, "suspect" = suspect)

#cleaning up the tweets
tweets$retweet_count[is.na(tweets$retweet_count)] <- 0
tweets$favorite_count[is.na(tweets$favorite_count)] <- 0

#getting a count of all the hashtags used
#make a function because we'll be using this several times
hashex <- function(x){
  temp <- separate(x, col = hashes, into = c("hash1", "hash2", "hash3", 
                                                     "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", 
                                                     "hash12", "hash13", "hash14", "hash15"), sep = ";", extra = "merge", fill = "right")
  temp <- apply(temp[,24:38], 2, FUN=function(y) dplyr::count(temp, hashtag = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(hashtag) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$hashtag), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n)))
  return(temp)
}
tweetsh <- hashex(tweets)

#lets look at the hashtag breakdown for each modularity group

tweetsL <- tweets[tweets$screen_name %in% groups$left$label, ]
tweetsLh <- hashex(tweetsL)
tweetsR <- tweets[tweets$screen_name %in% groups$right$label, ]
tweetsRh <- hashex(tweetsR)
tweetsSus <- tweets[tweets$screen_name %in% groups$suspect$label, ]
tweetsSush <- hashex(tweetsSus)

tweetsh <- left_join(tweetsh, tweetsLh, by = "hashtag") %>% left_join(., tweetsRh, by = "hashtag") %>% 
  left_join(., tweetsSush, by = "hashtag")
names(tweetsh) <- c("hashtag", "total", "ranking_total", "Left", "ranking_left", "Right", "ranking_right", "Suspect", "ranking_suspect")


#getting a count of all the users mentioned
#make a function because we'll be using this several times
mentionex <- function(x){
  temp <- separate(x, col = mentioned_users, into = c("ment1", "ment2", "ment3", 
                                             "ment4", "ment5", "ment6", "ment7", "ment8", "ment9", "ment10", "ment11", 
                                             "ment12", "ment13", "ment14"), sep = ";", extra = "merge", fill = "right")
  temp <- apply(temp[,22:35], 2, FUN=function(y) dplyr::count(temp, mention = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(mention) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$mention), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n)))
  return(temp)
}
tweetsm <- mentionex(tweets)

#lets look at the hashtag breakdown for each modularity group

tweetsLm <- mentionex(tweetsL)
tweetsRm <- mentionex(tweetsR)
tweetsSusm <- mentionex(tweetsSus)

tweetsm <- left_join(tweetsm, tweetsLm, by = "mention") %>% left_join(., tweetsRm, by = "mention") %>% 
  left_join(., tweetsSusm, by = "mention")
names(tweetsm) <- c("mention", "total", "ranking_total", "Left", "ranking_left", "Right", "ranking_right", "Suspect", "ranking_suspect")


#check postnig times of each group
par(mfrow=c(3,1))
hist(as.POSIXlt(tweetsL$datetime)$hour, breaks = rep(0:23, each = 1), main = "Left", xlab = "hour of the day")
hist(as.POSIXlt(tweetsR$datetime)$hour, breaks = rep(0:23, each = 1), main = "Right", xlab = "hour of the day")
hist(as.POSIXlt(tweetsSus$datetime)$hour, breaks = rep(0:23, each = 1), main = "Suspect", xlab = "hour of the day")

tweetsh <- arrange(tweetsh, (ranking_total))

tweetsh$Left[is.na(tweetsh$Left)] <- 0
tweetsh$Right[is.na(tweetsh$Right)] <- 0
tweetsh$Suspect[is.na(tweetsh$Suspect)] <- 0

#top 50 hashtags
ggtern(data = tweetsh[1:50, ], aes(x = Left, y = Right, z = Suspect, label = hashtag)) + 
  geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point()


tweetsm <- arrange(tweetsm, (ranking_total))

tweetsm$Left[is.na(tweetsm$Left)] <- 0
tweetsm$Right[is.na(tweetsm$Right)] <- 0
tweetsm$Suspect[is.na(tweetsm$Suspect)] <- 0

#top 50 mentions
usergroupings <- left
usergroupings$group <- "Left"
usergroupings <- full_join(usergroupings, transform(right, group = "Right"))
usergroupings <- full_join(usergroupings, transform(suspect, group = "Suspect"))

tweetsm <- left_join(tweetsm, usergroupings, by = c("mention" = "label"))

ggtern(data = tweetsm[1:50, ], aes(x = Left, y = Right, z = Suspect, label = mention, color = group)) + 
  geom_text(aes(label = mention), check_overlap = T) + theme_nomask() + geom_point()


#toplist of hashtags
grid.arrange(
ggtern(data = tweetsh, aes(x = Left, y = Right, z = Suspect, label = hashtag)) + 
   theme_nomask() + geom_point() + ggtitle("All Hashtags") + theme(plot.title = element_text(hjust = 0.5)),

ggtern(data = tweetsh[1:50, ], aes(x = Left, y = Right, z = Suspect, label = hashtag)) + 
  geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point() + 
  ggtitle("Top 50 Hashtags") + theme(plot.title = element_text(hjust = 0.5)),

ggtern(data = tweetsh[1:20, ], aes(x = Left, y = Right, z = Suspect, label = hashtag)) + 
  geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point() + 
  ggtitle("Top 20 Hashtags") + theme(plot.title = element_text(hjust = 0.5)), nrow = 1)

#toplists of mentioned users
grid.arrange(
ggtern(data = tweetsm, aes(x = Left, y = Right, z = Suspect, label = mention, color = group)) + 
  theme_nomask() + geom_point() + ggtitle("All Mentioned Users") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification=c(0,1), legend.position=c(0,1)),

ggtern(data = tweetsm[1:50, ], aes(x = Left, y = Right, z = Suspect, label = mention, color = group)) + 
  geom_text(aes(label = mention), check_overlap = T, show.legend=F) + theme_nomask() + geom_point() + 
  ggtitle("Top 50 Mentioned Users") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification=c(0,1), legend.position=c(0,1)),

ggtern(data = tweetsm[1:20, ], aes(x = Left, y = Right, z = Suspect, label = mention, color = group)) + 
  geom_text(aes(label = mention), check_overlap = T, show.legend= F) + theme_nomask() + geom_point() + 
  ggtitle("Top 20 Mentioned Users") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification=c(0,1), legend.position=c(0,1)), nrow =1 ) 



#hashtag density
ggtern(data = tweetsh, aes(x = Left, y = Right, z = Suspect)) +
  stat_density_tern(geom="polygon",color='black',
                    base='identity',
                    aes(fill   = ..level..),
                    na.rm = TRUE)  +
  geom_point() + ggtitle("All Hashtags") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient(low="yellow",high="red") + theme(legend.justification=c(0,1), legend.position=c(0,1))

#creating an animation of the density chagnes of hashtags as you approach the top 20 most commonly used hashtags
#densities are calcualted using kde2d.
#writing the images, then reingesting them and combining them into an animation takes a considerable amount of time
#it's much, much quicker to just open the images in Photoshop and make the .gif there.  Photoshop will also optimize it.
#in this example, Photoshop's compression makes the .gif roughly 1/6th the size of this R implementation.
n  = 7400
for(i in 1:370){
  z = (7400-((20*i) - 20))
  ggtern(data = tweetsh[1:z, ], aes(x = Left, y = Right, z = Suspect)) +
    stat_density_tern(geom="polygon",color='black',
                      base='identity',
                      aes(fill   = ..level..),
                      na.rm = TRUE)  +
    geom_point() + ggtitle(paste0("The Top ", z, " Hashtags")) + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_gradient(low="yellow",high="red") + theme(legend.justification=c(0,1), legend.position=c(0,1))
  
  ggsave(filename = paste0("image", str_pad(i, 4, pad = "0"),".png"), width = 8, height = 8, dpi = 150, path = "C:/Users/Ryan/Documents/trump/anim", units = "in")
}

list.files(path = "./anim", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 25) %>%
  image_write("hashtagchanges.gif")

#generating samples. can sample UP TO a given number of samples, getting around the problem
#of sample_n requiring there to be >= x number of samples to be able to take a sample
getsamp <- function(x, n, s){
  #x is list of handles
  #n is number of handles to sample
  #s is number of tweets to sample from each handle
  #samples up to s number of tweets
  
  c <- data.frame()
  a <- sample_n(x, n)
  a <- tweets[tweets$screen_name %in% a$label, ] %>% count(screen_name) %>%  arrange(desc(n))
  while(nrow(a) < 20) {
    a <- sample_n(x,n)
    a <- tweets[tweets$screen_name %in% a$label, ] %>% count(screen_name) %>% arrange(desc(n))
  }
  
  for(i in 1:n ){
    if(a[i,2] <= s){
      b <- tweets[tweets$screen_name %in% a[i,1], ]
      c <- rbind(c, b)
    }
    else{
      b <- tweets[tweets$screen_name %in% a[i,1], ]
      b <- sample_n(b, s)
      c <- rbind(c, b)
    }
    
  }
  return(c)
}


#count the number of capitalized cahracters 
countcaps <- function(x){length(
  str_extract_all(
    string = paste0(
      gsub("http[[:alnum:][:punct:]]*", "", x$text)
      ), pattern = "[A-Z]", simplify = T))/
#count characters w/o whitespaces and urls
nchar(
  paste0(
    gsub(" ", "", gsub("http[[:alnum:][:punct:]]*", "", x$text), fixed = T)
    )
)}


trump_forml %>% rowwise() %>% do(CapsRate = countcaps(.)) %>% bind_cols(trump_forml, .) %>% View()


count_matches <- function(string, matchto, description, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) & length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}

emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  #https://github.com/today-is-a-good-day/emojis/blob/master/emoji_analysis.R
  texts %>% 
    lapply(count_matches, matchto = matchto, description = description, sentiment = sentiment) %>%
    bind_rows
  
}


# a <- "banana banana bongo pig table cat banana ban"
# b <- c("ban", "pig", "cat")
# str_count(a, b, fixed = T)

usergroupingsbackup <- apply(usergroupings[,1], 1, function(x) botcheck(x))
ug1 <- sapply(usergroupingsbackup, function(x) ifelse(x == "NULL", NA, x))
ug1 <- t(plyr::rbind.fill.matrix(lapply(ug1, t)))
ug1 <- t(ug1)
usergroupings1 <- usergroupings %>% mutate(botometer = ug1)

#usergroupings1 %>% filter(is.na(botometer) == T) %>% select(label) %>% by_row(map(., function(x) botcheck(x)))
#some accounts didn't get parsed correctly, testing for missed accounts
a <- apply(usergroupings1[is.na(usergroupings1$botometer), 1], 1, function(x) botcheck(x))
a <- sapply(a, function(x) ifelse(x == "NULL", NA, x))
a <- t(plyr::rbind.fill.matrix(lapply(a, t)))
a <- t(a)
a <- usergroupings1 %>% filter(is.na(botometer) == T) %>% mutate(botometer1 = a)
b <- apply(a[is.na(a$botometer1), 1], 1, function(x) botcheck(x))
b <- sapply(b, function(x) ifelse(x == "NULL", NA, x))
b <- t(plyr::rbind.fill.matrix(lapply(a, t)))
b <- t(b)


# 
# #parsing botornot
# b <- usergroupings1[!is.na(usergroupings1$botometer), 1]
# names(b) <- "screen_name"
# b <- botornot(b$screen_name, fast = TRUE)
# b1 <- data.frame()
# 
# for(i in (nrow(b1) +1):nrow(b)){
#   
#   t <- botornot(b$screen_name[i])
#   b1 <- rbind(b1, t)
#  # print(rate_limits(query = "get_timeline") )
# }
# 
# 
# 
# #ratelimittry
# for(i in (nrow(b1) +1):nrow(b)){
#   rtlimit <- rate_limit()[11,]
#   remaining <- rtlimit[["remaining"]]
#   reset <- rtlimit[["reset"]]
#   reset <- as.numeric(reset, "secs")
#   
#   if (remaining < 2){
#     message(paste0(
#       "retry on rate limit...\n",
#       "waiting about ",
#       round(reset / 60, 0),
#       " minutes..."))
#     Sys.sleep(reset + 2)
#   }
#   
#   else{
#   t <- botornot(b$screen_name[i])
#   b1 <- rbind(b1, t)
#   print(remaining)
#       }
#   }
# #19 missing users
# 
# b2 <- usergroupings1 %>% left_join(b1, by = c("label" = "user")) %>% filter(!is.na(botometer) & is.na(prob_bot)) %>%
#   select(label)
# b3 <- data.frame()
# 
# for(i in (nrow(b3) +1):nrow(b2)){
#   rtlimit <- rate_limit()[11,]
#   remaining <- rtlimit[["remaining"]]
#   reset <- rtlimit[["reset"]]
#   reset <- as.numeric(reset, "secs")
#   
#   if (remaining < 2){
#     message(paste0(
#       "retry on rate limit...\n",
#       "waiting about ",
#       round(reset / 60, 0),
#       " minutes..."))
#     Sys.sleep(reset + 2)
#   }
#   
#   else{
#     t <- botornot(b2$label[i])
#     b3 <- rbind(b3, t)
#     print(remaining)
#   }
# }
# 
# b1 <- b1 %>% left_join(b3)
# 
# 
# usergroupings1 <- usergroupings1 %>% left_join(b1,by = c("label" = "user"))
# 
# rate_limits() %>% mutate(diff = limit - remaining) %>% View()
# 
# usergroupings1 %>% filter(!is.na(botometer) & is.na(prob_bot)) %>% View()
# 
# 
# 
# 
# 
# #violin boxplots for groups
# ggplot(usergroupings1, aes(x=group, y=botometer))+geom_boxplot()+
#   geom_violin(fill='lightblue', alpha=0.5) + 
#   geom_jitter(alpha =.2, position = position_jitter(width = .2)) +
#   geom_hline(yintercept = .43, col = "red")
# 
# #histogram boxplot
# plt1 <- ggplot(usergroupings1, aes(x = "", y = botometer)) +
#   geom_boxplot(fill = "lightblue", color = "black") + 
#   coord_flip() +
#   theme_classic() +
#   xlab("") +geom_hline(yintercept = .43, col = "red") +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# plt2 <- ggplot(usergroupings1, aes(x = botometer)) + geom_histogram(fill = "lightblue", 
#                                                                     color = "black", bins = 20) +
#   ylab("Frequency") + geom_vline(xintercept = .43, col = "red") +
#   theme_classic()
# 
# cowplot::plot_grid(plt2, plt1, 
#                    ncol = 1, rel_heights = c(2, 1),
#                    align = 'v', axis = 'lr') 
# 
# 
# 
# 
# conctext <- paste(trump_forml$text, collapse = "")
# unique(conctext)
# 
# 
# test <- trump_forml[1:5,]
# conctext <- paste(trump_forml$text, collapse = "")
# uniquechars <- unique(strsplit(tolower(conctext), "")[[1]])
