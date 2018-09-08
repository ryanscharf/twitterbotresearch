library(tidyverse)
library(botcheck)
library(httr)
library(xml2)
library(RJSONIO)
library(readr)
library(tweetbotornot)
library(rtweet)
library(ggExtra)
library(ggthemes)

retweet_trump_graph2 <- read_csv("New folder/retweet.trump_graph2.csv", 
                                 col_types = cols(Id = col_skip()))
##botometer
retweet_trump_graph2backup <- apply(retweet_trump_graph2[,1], 1, function(x) botcheck(x))
ug1 <- sapply(retweet_trump_graph2backup, function(x) ifelse(x == "NULL", NA, x))
ug1 <- t(plyr::rbind.fill.matrix(lapply(ug1, t)))
ug1 <- t(ug1)
retweet_trump_graph2 <- retweet_trump_graph2 %>% mutate(botometer = ug1)

retweet_trump_graph2 <- retweet_trump_graph2 %>% 
  mutate(botclass = ifelse(botometer > .43, 1, 0), botclassf = ifelse(botometer > .43, "bot", "human"))

rtg2 <- retweet_trump_graph2 %>% filter(modularity_class == 12 | modularity_class == 13 | modularity_class == 19)
rtg2r <- rtg2[!is.na(rtg2$botometer), ]
###


#botornot fast
b <- rtg2r[!is.na(rtg2r$botometer), 1]
names(b) <- "screen_name"
b1 <- data.frame()

#b2 <- botornot(b$screen_name, fast = T)

#ratelimitretry
rtlimit <- rate_limit()[11,]  #49 for fast, 11 for normal
remaining <- rtlimit[["remaining"]]
reset <- rtlimit[["reset"]]
reset <- as.numeric(reset, "secs")

#for(i in (nrow(b1) +1):nrow(b)){
#  if (remaining < 2){
    message(paste0(
      "retry on rate limit...\n",
      "waiting about ",
      round(reset / 60, 0),
      " minutes..."))
    Sys.sleep(reset + 2)
  }
#  
#  else{
#    suppressWarnings(tryCatch(
#      t <- botornot(b$screen_name[i], fast = T), 
#      warning=function(w) {
#        message("handled warning: ", conditionMessage(w))
#        invokeRestart("muffleWarning")},
#      error = t <- c(b$screen_name[i],NA,NA)))
#    b1 <- rbind(b1, t)
#    print(remaining)
#  }
#}#for
#}#while


 #working but keeps runnning  
#for(i in (nrow(b1) + 1):nrow(b)) {
#  t <- tryCatch(
#    botornot(b$screen_name[i], fast = T), 
#    warning = function(w){return(c(b$screen_name[i],NA,NA))}#,
#    #error =  function(e){return(c(b$screen_name[i],NA,NA))}
#    )
#
#  b1 <- rbind(b1, t)
#  remaining <- remaining - 1
#  print(remaining)
#}

b1 <- data.frame()  

#trying smater rate limit
while(nrow(b1) < nrow(b)){
  for(i in (nrow(b1)+1):(nrow(b1) +179)){
  t <- tryCatch(
    botornot(b$screen_name[i], fast = F), 
    warning = function(w){return(c(b$screen_name[i],NA,NA))}#,
    #error =  function(e){return(c(b$screen_name[i],NA,NA))}
  )
  b1 <- rbind(b1, t)
  }#for
  
  rtlimit <- rate_limit()[11,]  #49 for fast, 11 for normal
  reset <- as.numeric(rtlimit[["reset"]], "secs")
  message(paste0(
    "retry on rate limit...\n",
    "waiting about ",
    round(reset / 60, 0),
    " minutes..."))
  Sys.sleep(reset + 2)
}#while

b1 <- b1 %>% select(screen_name, botornots)
rtg2r <- rtg2r %>% left_join(b1, by = c("Label" = "screen_name"))
rtg2r <- rtg2r %>% mutate(botornotsc = case_when(botornots < .5 ~ "human",
                                                 botornots >= .5 ~ "bot"))




#violin boxplots for groups
ggplot(rtg2r, aes(x=as.character(modularity_class), y=botometer))+geom_boxplot()+
  geom_violin(fill='lightblue', alpha=0.5) + 
  geom_jitter(alpha =.2, position = position_jitter(width = .2)) +
  geom_hline(yintercept = .43, col = "red")

ggsave("botometer2.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")

#histogram boxplot
plt1 <- ggplot(rtg2r, aes(x = "", y = botometer)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  coord_flip() +
  theme_classic() +
  xlab("") +geom_hline(yintercept = .43, col = "red") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(rtg2r, aes(x = botometer)) + geom_histogram(fill = "lightblue", 
                                                                    color = "black", bins = 20) +
  ylab("Frequency") + geom_vline(xintercept = .43, col = "red") +
  theme_classic()

cowplot::plot_grid(plt2, plt1, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr') 

ggsave("bothisbox2.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")

#hashtags
hashex <- function(x){
  temp <- x
  temp$hashtags <- gsub("c\\(|\\)|\"|\\s", "", temp$hashtags)
  temp <- separate(temp, col = hashtags, into = c("hash1", "hash2", "hash3", 
                                             "hash4", "hash5", "hash6", "hash7", "hash8", "hash9", "hash10", "hash11", 
                                             "hash12", "hash13", "hash14", "hash15", "hash16", "hash17", "hash18", "hash19",
                                             "hash20", "hash21", "hash22", "hash23", "hash24"), sep = ",", extra = "merge", fill = "right")
  temp <- apply(temp[,15:38], 2, FUN=function(y) dplyr::count(temp, hashtag = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(hashtag) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$hashtag), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n))) %>% arrange(desc(n))
  return(temp)
}

rtfh <- hashex(rtf)


#mentions
mentionex <- function(x){
  temp <- x
  temp$mentions_screen_name <- gsub("c\\(|\\)|\"|\\s", "", temp$mentions_screen_name)
  temp <- separate(temp, col = mentions_screen_name, into = c("ment1", "ment2", "ment3", 
                                                      "ment4", "ment5", "ment6", "ment7", "ment8", "ment9", "ment10", "ment11", 
                                                      "ment12", "ment13", "ment14", "ment15"), sep = ",", extra = "merge", fill = "right")
  temp <- apply(temp[,29:43], 2, FUN=function(y) dplyr::count(temp, mention = tolower(y)))
  temp <- do.call(rbind.data.frame, temp)
  temp <- temp %>% group_by(mention) %>% summarise(n = sum(n))
  temp <- temp[!is.na(temp$mention), ]  
  temp$n[is.na(temp$n)] <- 0
  temp <- temp %>% mutate(ranking = min_rank(desc(n))) %>% arrange(desc(n))
  return(temp)
}
rtfm <- mentionex(rtf)




#rtg2r %>% filter(duplicated(rtg2r$Label) == TRUE) %>% View()
rtg2ru <- rtg2r[-130:-133, ]
rtg2ru <- rtg2ru[-706:-709, ]

rtg2ru <- rtg2ru %>% mutate(ulength = nchar(Label))

rtg2ru <- rtg2ru %>% mutate(clus = case_when(
  modularity_class == 12 ~ "Cluster 3",
  modularity_class == 13 ~ "Cluster 2",
  modularity_class == 19 ~ "Cluster 1"
))

rtg2ru %>% group_by(clus) %>% summarise( mean = mean(ulength), median = median(ulength))

library(stringr)
cntwht <- function(q){
  if(is.na(q) == T){
    return(0)
  }
  else{
    return(1 + str_count(q, " "))
  }
}

rtweettable <- rtweet_041118 %>% select(screen_name, mentions_screen_name, hashtags, is_retweet)

test <- rtweettable %>% group_by(screen_name) %>% 
  summarise(retweets = sum(is_retweet),
            tweets = abs(sum(1 - is_retweet)))

test <- rtweettable %>% rowwise() %>% 
  mutate(numhash = cntwht(hashtags), numment = cntwht(mentions_screen_name)) %>%
  group_by(screen_name) %>% summarize(hashtags = sum(numhash), mentions = sum(numment)) %>% right_join(test) 

rtg2ru <- left_join(rtg2ru, test, by = c("Label" = "screen_name"))      

rtg2ru %>% group_by(clus) %>% summarize(tweets = sum(tweets, na.rm = T), 
                                        retweets = sum(retweets, na.rm = T), 
                                        ment = sum(mentions, na.rm = T),
                                        hash = sum(hashtags, na.rm = T))

rtg2ru %>% group_by(clus) %>% summarize(tweets = mean(tweets, na.rm = T), 
                                        retweets = mean(retweets, na.rm = T), 
                                        ment = mean(mentions, na.rm = T),
                                        hash = mean(hashtags, na.rm = T),
                                        ulength = mean(ulength, na.rm = T))


rtg2ru <- rtg2ru %>% mutate_at(c("tweets", "retweets", "hashtags", "mentions", "ulength"), 
                     .funs = funs(zsc = scale))

#rtg2ru %>% group_by(clus) %>% summarize(tweets = mean(tweets, na.rm = T), 
#                                        retweets = mean(retweets, na.rm = T), 
#                                        ment = mean(mentions, na.rm = T),
#                                        hash = mean(hashtags, na.rm = T),
#                                        ulength = mean(ulength, na.rm = T)) %>% 
#  mutate_at(c("tweets", "retweets", "hash", "ment", "ulength"), scale) %>% View()
