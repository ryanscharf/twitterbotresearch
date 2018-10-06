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
#data <- read_csv("woTrump1018 - trump_red [Nodes].csv")
#tweets <- read_csv("pol_clust_together.csv")


#create objects for each modularity grouping of users
left <- filter(rtweetwobots, clus == "Cluster 1.wo.bot")
right <- filter(rtweetwobots, clus == "Cluster 2.wo.bot")
suspect <- filter(rtweetwobots, clus == "Cluster 3.wo.bot")
#add to list
groups <- list("left" = left, "right" = right, "suspect" = suspect)

#cleaning up the tweets
#tweets$retweet_count[is.na(tweets$retweet_count)] <- 0
#tweets$favorite_count[is.na(tweets$favorite_count)] <- 0

#getting a count of all the hashtags used
#make a function because we'll be using this several times
hashex <- function(x){
  temp <- separate(x, col = hashtags, into = c("hash1", "hash2", "hash3", 
                                             "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", 
                                             "hash12", "hash13", "hash14", "hash15", "hash16"), sep = " ", extra = "merge", fill = "right")
  temp <- apply(temp[,16:31], 2, FUN=function(y) dplyr::count(temp, hashtag = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(hashtag) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$hashtag), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n)))
  return(temp)
}
tweetsh <- hashex(rtweet_updated_dset_nobots_093018)

#lets look at the hashtag breakdown for each modularity group

tweetsL <- rtweet_updated_dset_nobots_093018[rtweet_updated_dset_nobots_093018$screen_name %in% groups$left$screen_name, ]
tweetsLh <- hashex(tweetsL)
tweetsR <- rtweet_updated_dset_nobots_093018[rtweet_updated_dset_nobots_093018$screen_name %in% groups$right$screen_name, ]
tweetsRh <- hashex(tweetsR)
tweetsSus <- rtweet_updated_dset_nobots_093018[rtweet_updated_dset_nobots_093018$screen_name %in% groups$suspect$screen_name, ]
tweetsSush <- hashex(tweetsSus)

tweetsh <- left_join(tweetsh, tweetsLh, by = "hashtag") %>% left_join(., tweetsRh, by = "hashtag") %>% 
  left_join(., tweetsSush, by = "hashtag")
names(tweetsh) <- c("hashtag", "total", "ranking_total", "Cluster 1.wo.bot", "ranking_C1", "Cluster 2.wo.bot", "ranking_C2", "Cluster 3.wo.bot", "ranking_C3")


#getting a count of all the users mentioned
#make a function because we'll be using this several times
mentionex <- function(x){
  temp <- separate(x, col = mentions_screen_name, into = c("ment1", "ment2", "ment3", 
                                                      "ment4", "ment5", "ment6", "ment7", "ment8", "ment9", "ment10", "ment11", 
                                                      "ment12", "ment13", "ment14"), sep = " ", extra = "merge", fill = "right")
  temp <- apply(temp[,30:43], 2, FUN=function(y) dplyr::count(temp, mention = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(mention) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$mention), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n)))
  return(temp)
}
tweetsm <- mentionex(rtweet_updated_dset_nobots_093018)

#lets look at the hashtag breakdown for each modularity group

tweetsLm <- mentionex(tweetsL)
tweetsRm <- mentionex(tweetsR)
tweetsSusm <- mentionex(tweetsSus)

tweetsm <- left_join(tweetsm, tweetsLm, by = "mention") %>% left_join(., tweetsRm, by = "mention") %>% 
  left_join(., tweetsSusm, by = "mention")
names(tweetsm) <- c("mention", "total", "ranking_total", "Cluster 1.wo.bot", "ranking_C1", "Cluster 2.wo.bot", "ranking_C2", "Cluster 3.wo.bot", "ranking_C3")


#check postnig times of each group
#par(mfrow=c(3,1))
#hist(as.POSIXlt(tweetsL$datetime)$hour, breaks = rep(0:23, each = 1), main = "Left", xlab = "hour of the day")
#hist(as.POSIXlt(tweetsR$datetime)$hour, breaks = rep(0:23, each = 1), main = "Right", xlab = "hour of the day")
#hist(as.POSIXlt(tweetsSus$datetime)$hour, breaks = rep(0:23, each = 1), main = "Suspect", xlab = "hour of the day")

tweetsh <- arrange(tweetsh, (ranking_total))

tweetsh$`Cluster 1.wo.bot`[is.na(tweetsh$`Cluster 1.wo.bot`)] <- 0
tweetsh$`Cluster 2.wo.bot`[is.na(tweetsh$`Cluster 2.wo.bot`)] <- 0
tweetsh$`Cluster 3.wo.bot`[is.na(tweetsh$`Cluster 3.wo.bot`)] <- 0

#top 50 hashtags
ggtern(data = tweetsh[1:50, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = hashtag)) + 
  geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point()

#savingsavingsaving
ggsave("Top50hashtagswobots.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo") ###


tweetsm <- arrange(tweetsm, (ranking_total))

tweetsm$`Cluster 1.wo.bot`[is.na(tweetsm$`Cluster 1.wo.bot`)] <- 0
tweetsm$`Cluster 2.wo.bot`[is.na(tweetsm$`Cluster 2.wo.bot`)] <- 0
tweetsm$`Cluster 3.wo.bot`[is.na(tweetsm$`Cluster 3.wo.bot`)] <- 0

#top 50 mentions
usergroupings <- left
usergroupings$group <- "Cluster 1.wo.bot"
usergroupings <- full_join(usergroupings, transform(right, group = "Cluster 2.wo.bot"))
usergroupings <- full_join(usergroupings, transform(suspect, group = "Cluster 3.wo.bot"))

tweetsm <- left_join(tweetsm, usergroupings, by = c("mention" = "screen_name"))

ggtern(data = tweetsm[1:50, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = mention)) + 
  geom_text(aes(label = mention), check_overlap = T) + theme_nomask() + geom_point()
ggsave("Top50mentionswobots.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo") ###


#toplist of hashtags
grid.arrange(
  ggtern(data = tweetsh, aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = hashtag)) + 
    theme_nomask() + geom_point() + ggtitle("All Hashtags") + theme(plot.title = element_text(hjust = 0.5)),
  
  ggtern(data = tweetsh[1:50, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = hashtag)) + 
    geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point() + 
    ggtitle("Top 50 Hashtags") + theme(plot.title = element_text(hjust = 0.5)),
  
  ggtern(data = tweetsh[1:20, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = hashtag)) + 
    geom_text(aes(label = hashtag), check_overlap = T, vjust = 1) + theme_nomask() + geom_point() + 
    ggtitle("Top 20 Hashtags") + theme(plot.title = element_text(hjust = 0.5)), nrow = 1)

ggsave("hashtaggorwthwobots.png", width = 18, height = 6, units = "in", dpi = 300, type = "cairo")

#toplists of mentioned users
grid.arrange(
  ggtern(data = tweetsm, aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = mention, color = group)) + 
    theme_nomask() + geom_point() + ggtitle("All Mentioned Users") + 
    theme(plot.title = element_text(hjust = 0.5), legend.justification=c(0,1), legend.position=c(0,1)),
  
  ggtern(data = tweetsm[1:50, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = mention, color = group)) + 
    geom_text(aes(label = mention), check_overlap = T, show.legend=F) + theme_nomask() + geom_point() + 
    ggtitle("Top 50 Mentioned Users") + 
    theme(plot.title = element_text(hjust = 0.5), legend.justification=c(0,1), legend.position=c(0,1)),
  
  ggtern(data = tweetsm[1:20, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`, label = mention, color = group)) + 
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
#n  = 7400
#for(i in 1:370){
#  z = (7400-((20*i) - 20))
#  ggtern(data = tweetsh[1:z, ], aes(x = Left, y = Right, z = Suspect)) +
#    stat_density_tern(geom="polygon",color='black',
#                      base='identity',
#                      aes(fill   = ..level..),
#                      na.rm = TRUE)  +
#    geom_point() + ggtitle(paste0("The Top ", z, " Hashtags")) + theme(plot.title = element_text(hjust = 0.5)) + 
#    scale_fill_gradient(low="yellow",high="red") + theme(legend.justification=c(0,1), legend.position=c(0,1))
#  
#  ggsave(filename = paste0("image", str_pad(i, 4, pad = "0"),".png"), width = 8, height = 8, dpi = 150, path = "C:/Users/Ryan/Documents/trump/anim", units = "in")
#}
#
#list.files(path = "./anim", pattern = "*.png", full.names = T) %>% 
#  map(image_read) %>%
#  image_join() %>%
#  image_animate(fps = 25) %>%
#  image_write("hashtagchanges.gif")


ggtern(data = tweetsh[1:60, ], aes(x = `Cluster 1.wo.bot`, y = `Cluster 2.wo.bot`, z = `Cluster 3.wo.bot`)) +
  stat_density_tern(geom="polygon",color='black',
                    base='identity',
                    aes(fill   = ..level..),
                    na.rm = TRUE)  +
  geom_point() + ggtitle("The Top 60 Hashtags") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient(low="yellow",high="red") + theme(legend.justification=c(0,1), legend.position=c(0,1))
ggsave("top60hashtopowobots.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")