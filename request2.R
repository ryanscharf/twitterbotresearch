rtweetwobots <- read_csv("C:/Users/Ryan/Downloads/rtweet_botomer_removed_network083118 [Nodes].csv", 
                         +     col_types = cols(Id = col_skip()))

rtweetwobots <- rtweetwobots %>% filter(`Modularity Class` %in% c("13", "16", "20"))

rtweetwobots <- rtweetwobots %>% mutate(clus = case_when(
  `Modularity Class` == 13 ~ "Cluster 1.wo.bot",
  `Modularity Class` == 16 ~ "Cluster 3.wo.bot",
  `Modularity Class` == 20 ~ "Cluster 2.wo.bot"))

rttablewobots <- rtweettable %>% filter(screen_name %in% rtweetwobots$Label)

test <- rttablewobots %>% group_by(screen_name) %>% 
  summarise(retweets = sum(is_retweet),
            tweets = abs(sum(1 - is_retweet)))

test <- rttablewobots %>% rowwise() %>% 
  mutate(numhash = cntwht(hashtags), numment = cntwht(mentions_screen_name)) %>%
  group_by(screen_name) %>% summarize(hashtags = sum(numhash), mentions = sum(numment)) %>% right_join(test) 

rtweetwobots <- left_join(rtweetwobots, test, by = c("Label" = "screen_name"))      

rtweetwobots %>% group_by(clus) %>% summarize(tweets = sum(tweets, na.rm = T), 
                                        retweets = sum(retweets, na.rm = T), 
                                        ment = sum(mentions, na.rm = T),
                                        hash = sum(hashtags, na.rm = T))

rtweetwobots %>% group_by(clus) %>% summarize(tweets = mean(tweets, na.rm = T), 
                                        retweets = mean(retweets, na.rm = T), 
                                        ment = mean(mentions, na.rm = T),
                                        hash = mean(hashtags, na.rm = T),
                                        )

rtweetwobots <- rtg2ru %>% select(Label, ulength, account_created_at) %>% right_join(rtweetwobots)

rtweetwobots <- rtweetwobots %>% mutate_at(c("tweets", "retweets", "hashtags", "mentions", "ulength", "account_created_at"), 
                               .funs = funs(zsc = scale))

