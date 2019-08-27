library(readr)
library(tidyverse)
library(ggthemes)

#import data for updated network & url data
pos_freq_zscaled <- read_csv("pos_freq_zscaled.csv")
#rtweetwobots <- read_csv("C:/Users/Ryan/Downloads/rtweet_botomer_removed_network083118 [Nodes].csv", 
#                         col_types = cols(Id = col_skip()))
rtweetwobots <- read_csv("rtweet_botomer_removed_network083118.update.csv")
pos_freq_zscaled <- pos_freq_zscaled %>% filter(pos == "U") %>% select(screen_name, n ) %>% rename(URLs = n)

#outing the accoutns that weren't avaliable for botometer
rtweetwobots <- rtg2ru %>% select(screen_name, botometer) %>% right_join(rtweetwobots, by=  c("screen_name" = "Label"))
rtweetwobots <- rtweetwobots %>% filter(!is.na(botometer) == T)
rtweetwobots <- rtweetwobots %>% filter(`Modularity Class` %in% c("13", "16", "20"))


rtweetwobots <- rtweetwobots %>% mutate(clus = case_when(
  `Modularity Class` == 13 ~ "L1.wo.bot",
  `Modularity Class` == 16 ~ "L2.wo.bot",
  `Modularity Class` == 20 ~ "R.wo.bot"))

rttablewobots <- rtweettable %>% filter(screen_name %in% rtweetwobots$screen_name)

test <- rttablewobots %>% group_by(screen_name) %>% 
  summarise(retweets = sum(is_retweet),
            tweets = abs(sum(1 - is_retweet)))

test <- rttablewobots %>% rowwise() %>% 
  mutate(numhash = cntwht(hashtags), numment = cntwht(mentions_screen_name)) %>%
  group_by(screen_name) %>% summarize(hashtags = sum(numhash), mentions = sum(numment)) %>% right_join(test) 

rtweetwobots <- left_join(rtweetwobots, test)    

rtweetwobots <- rtg2ru %>% select(screen_name, ulength, account_created_at) %>% right_join(rtweetwobots)
rtweetwobots <- pos_freq_zscaled %>% right_join(rtweetwobots)
rtweetwobots %>% group_by(clus) %>% summarize(tweets = sum(tweets, na.rm = T), 
                                        retweets = sum(retweets, na.rm = T), 
                                        ment = sum(mentions, na.rm = T),
                                        hash = sum(hashtags, na.rm = T),
                                        ulen = sum(ulength, na.rm = T),
                                        #accd = sum(account_created_at, na.rm = T),
                                        URLs = sum(URLs, na.rm = T))

rtweetwobots %>% filter(!is.na(tweets_zsc))%>% 
  group_by(clus) %>% summarize(Tweets = mean(tweets, na.rm = T), 
                                        Retweets = mean(retweets, na.rm = T), 
                                        Ment = mean(mentions, na.rm = T),
                                        Hash = mean(hashtags, na.rm = T),
                                        ulen = mean(ulength, na.rm = T),
                                        Accd = mean(account_created_at_zsc, na.rm = T),
                                        URLs = mean(URLs, na.rm = T),
                                        TR = Tweets/Retweets,
                               nusers = n())# %>% View()

rtweetwobots <- rtweetwobots %>% rename(Tweets = tweets, Retweets = retweets, Hashtags = hashtags, Mentions = mentions, `Account Age` = account_created_at)


rtweetwobots <- rtweetwobots %>% mutate_at(c("Tweets", "Retweets", "Hashtags", "Mentions", "ulength", "Account Age", "URLs"), 
                               .funs = funs(zsc = scale))

#median not used for paper
#rtweetwobots %>% group_by(clus) %>% 
#  summarize_at(c("tweets_zsc", "retweets_zsc", "hashtags_zsc", "mentions_zsc", "account_created_at_zsc", "URLs_zsc"), median, na.rm = T) %>% 
#  gather(attribute, tweets_zsc, retweets_zsc, hashtags_zsc, mentions_zsc, account_created_at_zsc, URLs_zsc) %>%
#  ggplot(aes(x = attribute, y = tweets_zsc, fill = clus)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
#  scale_y_continuous(limits = c(-1,1)) + theme_fivethirtyeight(base_size = 18) + labs(y = "Z-score", x = "") + 
#  scale_fill_discrete(name = "") + guides(fill = guide_legend(ncol = 2, byrow = T)) +

#ggsave("zscoremediannobots.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo") ###


rtweetwobots %>% filter(!is.na(Tweets_zsc))%>% group_by(clus) %>% 
  summarize_at(c("Tweets_zsc", "Retweets_zsc", "Hashtags_zsc", "Mentions_zsc", "Account Age_zsc", "URLs_zsc"), mean, na.rm = T) %>% 
  gather(attribute, Tweets_zsc, Retweets_zsc, Hashtags_zsc, Mentions_zsc, `Account Age_zsc`, URLs_zsc) %>%
  ggplot(aes(x = attribute, y = Tweets_zsc, fill = clus)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  scale_y_continuous(limits = c(-1,1)) + 
  theme_fivethirtyeight(base_size = 18) + 
  labs(y = "Z-score", x = "") + 
  guides(fill = guide_legend(ncol = 2, byrow = T)) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.background  = element_rect(fill = "white"),
        plot.title = element_text(size=18)) +
  scale_fill_manual(values=c("#660099", "#e7d4e8", "#006600"), name = "Clusters  ") +
  scale_x_discrete(labels = c("Account Age", 'Hashtags', 'Mentions', 'Retweets', 'URLs')) + 
  ggtitle("Mean Z-Score per Cluster") 


ggsave("zscoremeannobots2.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")

trumpnet_acc_created_calc <- trumpnet_acc_created %>% mutate(acc_age = ymd('2018-09-01') - date(account_created_at)) %>% select(-account_created_at)

y <- rtweetwobots %>% filter(!is.na(tweets_zsc)) %>% left_join(trumpnet_acc_created_calc, by = 'screen_name') %>%
  group_by(clus) %>% summarize(
    nusers = n(),
    Tpu = sum(tweets) / n(),
    Rpu = sum(retweets) / n(),
    Mpu = sum(mentions) / n(),
    Hpu = sum(hashtags) / n(),
    ulenpu = sum(ulength) / n(),
    URLs = sum(URLs, na.rm = T) / n(),
    TR = sum(tweets) / sum(retweets),
    acc_age = round(mean(acc_age),0)
  )

z <- rtg2ru %>% filter(!is.na(tweets)) %>% left_join(trumpnet_acc_created_calc, by = 'screen_name') %>%
  group_by(clus) %>% summarize(
    Users = n(),
    tweets = mean(tweets, na.rm = T),
    retweets = mean(retweets, na.rm = T),
    ment = mean(mentions, na.rm = T),
    hash = mean(hashtags, na.rm = T),
    ulength = mean(ulength, na.rm = T),
    URLs = mean(URLs, na.rm = T),
    tr = tweets / retweets,
    acc_age = round(mean(acc_age),0)
  )

ggplot() + geom_histogram(data= z, aes(x=acc_age, fill = 'w/bots')) + geom_histogram(data = y, aes(x=acc_age, fill = 'wobots'))
