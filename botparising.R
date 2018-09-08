#sources: retweet_trump_graph2, newfile
library(rtweet)
library(botrnot)
library(botcheck)
library(httr)
library(xml2)
library(RJSONIO)
library(tidyverse)

#parsing botometer
retweet_trump_graph2backup <- apply(retweet_trump_graph2[,1], 1, function(x) botcheck(x))
ug1 <- sapply(retweet_trump_graph2backup, function(x) ifelse(x == "NULL", NA, x))
ug1 <- t(plyr::rbind.fill.matrix(lapply(ug1, t)))
ug1 <- t(ug1)
retweet_trump_graph2 <- retweet_trump_graph2 %>% mutate(botometer = ug1)


#parsing botornot
b <- retweet_trump_graph2[!is.na(retweet_trump_graph2$botometer), 1]
names(b) <- "screen_name"
b1 <- data.frame()

#ratelimittry
for(i in (nrow(b1) +1):nrow(b)){
  rtlimit <- rate_limit()[11,]
  remaining <- rtlimit[["remaining"]]
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  
  if (remaining < 2){
    message(paste0(
      "retry on rate limit...\n",
      "waiting about ",
      round(reset / 60, 0),
      " minutes..."))
    Sys.sleep(reset + 2)
  }
  
  else{
    t <- botornot(b$screen_name[i])
    b1 <- rbind(b1, t)
    print(remaining)
  }
}
#19 missing users

b2 <- retweet_trump_graph2 %>% left_join(b1, by = c("Label" = "user")) %>% filter(!is.na(botometer) & is.na(prob_bot)) %>%
  select(Label)
b3 <- data.frame()

for(i in (nrow(b3) +1):nrow(b2)){
  rtlimit <- rate_limit()[11,]
  remaining <- rtlimit[["remaining"]]
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  
  if (remaining < 2){
    message(paste0(
      "retry on rate limit...\n",
      "waiting about ",
      round(reset / 60, 0),
      " minutes..."))
    Sys.sleep(reset + 2)
  }
  
  else{
    t <- botornot(b2$Label[i])
    b3 <- rbind(b3, t)
    print(remaining)
  }
}

b4 <- b3[!(b3$user %in% b1$user), ]

b1 <- b1 %>% full_join(b4)


retweet_trump_graph2 <- retweet_trump_graph2 %>% left_join(b1,by = c("Label" = "user"))

retweet_trump_graph2 <- retweet_trump_graph2 %>% mutate(botornot_isbot = ifelse(botornot < .5, 0, 1), 
                                            botornot_hob = ifelse(botornot_isbot == 1, "bot", "human"),
                                            botclass = ifelse(botometer > .43, 1, 0),
                                            botclassf = ifelse(botometer > .43, "bot", "human"))





