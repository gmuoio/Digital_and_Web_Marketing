## Market Basket Analysis

## compute how many times an article has been purchased (only consider the first 100)
count_tickets <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_max(count, n = 100)

## plot the top 10 best sellers
top_articles <- (count_tickets %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(ID_ARTICOLO, count), y = count)) +
  geom_bar(stat= "identity",fill='#ffa500') +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Article",
       y = "Total Purchase",
       title = "Top 10 Best Sellers")
)

top_articles

## show the dataframe df_7_tic_clean_final orderd by ID_CLI
tickets_ordered <- df_7_tic_clean_final[order(df_7_tic_clean_final$ID_CLI),]

## split df_7_tic_clean_final by ID_CLI and TIC_DATE and create a list of items
itemList <- plyr::ddply(df_7_tic_clean_final, c("ID_CLI", "TIC_DATE"),
                        function(df1)paste(df1$ID_ARTICOLO, 
                                           collapse = ","))
itemList$ID_ARTICOLO <- NULL
itemList$TIC_DATE <- NULL
colnames(itemList) <- c("id_cli","items")

## save in a csv
write.csv(itemList, file= "market_basket.csv",
         quote = FALSE, row.names = TRUE)

## create a transaction object
tr <- arules::read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

## best 20 articles according to the support of the ID_ARTICLE
itemFrequencyPlot(tr, topN = 20, type = 'absolute')

## find the best association rules for the items 
rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.8))
rules <- sort(rules, by = 'confidence', decreasing = TRUE)
summary(rules)
inspect(rules)

library(RColorBrewer)
topRules <- rules[1:10]
plot(topRules)
plot(topRules, method = "graph")+
  scale_color_gradient(low = 'lightgrey',high='#ffa500')

inspect(topRules)
