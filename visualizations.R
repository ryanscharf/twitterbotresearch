#violin boxplots for groups
ggplot(rtg2r, aes(x=as.character(clus), y=botornots)) + ###
  geom_boxplot() +
  geom_violin(fill='lightblue', alpha=0.5) + 
  geom_jitter(alpha =.2, position = position_jitter(width = .2)) +
  theme_classic() +
  ggtitle("botornot(normal)") +
  labs(x = "Modularity Class", y = "Bot Probability")

ggsave("botornotnormal.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")

#histogram boxplot
plt1 <- ggplot(rtg2r, aes(x = "", y = botometer)) + ###
  geom_boxplot(fill = "lightblue", color = "black") + 
  coord_flip() +
  scale_y_discrete(position = "top", limits = c(0,1), breaks = c(.25, .5, .75), expand = c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  theme_classic() +
  xlab("") + ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(rtg2r, aes(x = botometer)) +   ###
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  ylab("Frequency") + 
  theme_classic() +
  ggtitle("Botometer") + ###
  xlab("Bot Probability") +  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
  

cowplot::plot_grid(plt2, plt1, 
                   ncol = 1, rel_heights = c(2, 1),
                   align = 'v', axis = 'lr') 

ggsave("botometerhistbox.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo") ###



#No JITTER
#violin boxplots for groups
ggplot(rtg2r, aes(x=as.character(clus), y=botornotf)) + ###
  geom_boxplot() +
  geom_violin(fill='lightblue', alpha=0.5) + 
  theme_classic() +
  ggtitle("botornot(fast)") +
  labs(x = "Modularity Class", y = "Bot Probability")

ggsave("botornotfastnj.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")


#median noot used
#rtg2ru %>% group_by(clus) %>% 
#  summarize_at(c("tweets_zsc", "retweets_zsc", "hashtags_zsc", "mentions_zsc", "acc_create_zsc", "URLs_zsc"), median, na.rm = T) %>% 
#  gather(attribute, tweets_zsc, retweets_zsc, hashtags_zsc, mentions_zsc, acc_create_zsc, URLs_zsc) %>%
#  ggplot(aes(x = attribute, y = tweets_zsc, fill = clus)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
#  scale_y_continuous(limits = c(-1,1)) + theme_fivethirtyeight(base_size = 18) + labs(y = "Z-score", x = "") + 
#  scale_fill_discrete(name = "")

#ggsave("zscoremedian.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo") ###


rtg2ru %>% filter(!is.na(tweets_zsc)) %>% group_by(clus) %>% 
  summarize_at(c("tweets_zsc", "retweets_zsc", "hashtags_zsc", "mentions_zsc", "acc_create_zsc", "URLs_zsc"), mean, na.rm = T) %>% 
  gather(attribute, tweets_zsc, retweets_zsc, hashtags_zsc, mentions_zsc, acc_create_zsc, URLs_zsc) %>%
  ggplot(aes(x = attribute, y = tweets_zsc, fill = clus)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  scale_y_continuous(limits = c(-1,1)) + theme_fivethirtyeight(base_size = 18) + labs(y = "Z-score", x = "") + 
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.background  = element_rect(fill = "white"),
        plot.title = element_text(size=18)) +
  scale_fill_manual(values=c("#660099", "#006600", "#ccffcc"), labels = c("L","R1","R2"), name = "Clusters  ") +
  scale_x_discrete(labels = c("Account Age", 'Hashtags', 'Mentions', 'Retweets', 'URLs')) + 
  ggtitle("Mean Z-Score per Cluster") 

ggsave("zscoremean2.png", width = 6, height = 6, units = "in", dpi = 300, type = "cairo")

rtg2ru %>% select(Label, tweets_zsc, retweets_zsc, hashtags_zsc, mentions_zsc, ulength_zsc) %>%
  gather(attribute, tweets_zsc, retweets_zsc, hashtags_zsc, mentions_zsc, ulength_zsc) %>%
  ggplot(aes(x = tweets_zsc, color = attribute)) + geom_density()


for(i in 38:42){
  print(range(rtg2ru[,i], na.rm=T))
}
