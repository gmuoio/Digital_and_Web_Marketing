# The RFM model is based on three quantitative factors:

# Recency: How recently a customer has made a purchase;
# Frequency: How often a customer makes a purchase;
# Monetary Value: How much money a customer spends on purchases.

# The output of the RFM model is primarily used to differentiate the marketing 
# actions accross the customer base. 

# Lets consider the active clients: we consider a client active if he purchased 
# after 01/01/2019, active client = purchased in the last 4 months

rfm_study_period <- df_7_tic_clean_final %>%
                    filter(TIC_DATE > as.Date("01/01/2019",
                            format = "%d/%m/%Y"))

## Recency, only consider the direction = 1 (purchase)
rfm_recency <- rfm_study_period %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI)%>% 
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

# compute difference between "last" date and last purchase date
rfm_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                format = "%d/%m/%Y"),        
                                rfm_recency$LAST_PURCHASE_DATE,
                                units = "days")

# Divide the type of recency in 3 different classes
# LOW: below the 25th percentile
# MEDIUM: between the 25th and 75th percentile
# HIGH: above the 75th percentile

rfm_recency <- within(rfm_recency,
                      REC_CLASS <- cut(as.numeric(rfm_recency$RECENCY),
                                       breaks = quantile(rfm_recency$RECENCY,
                                                         probs = c(0, .25, .75, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         

# compute frequency of the 3 different classes
rec_label <- as.data.frame(table(rfm_recency$REC_CLASS))

# plot the distribution of the 3 recency classes
plot_recency <- (ggplot(data = rec_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity") +                  
  labs(title = "Recency Distribution",
       x= "Recency Classes",
       y= "Total Purchase") +                
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.4)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)
)

plot_recency
 
## Frequency, only consider the direction = 1 (purchase)

rfm_frequency <- rfm_study_period %>%
  filter(DIREZIONE == 1)%>% 
  group_by(ID_CLI) %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))

# Divide the type of frequency in 3 different classes:
# Low: below 2 total purchases
# Medium: from 2 to 5 purchases
# High: above 5 (to 101) purchases

# (choose these values after seeing that the median of the number of purchases per client
# is equal to 2)
rfm_frequency <- within(rfm_frequency,
                        FREQ_CLASS <- cut(rfm_frequency$TOT_PURCHASE,
                                          breaks = c(0, 2, 5, 101),           
                                          include.lowest = T,
                                          right = F,
                                          labels = c("low", "medium", "high")))

# compute frequency of the 3 different classes
table(rfm_frequency$FREQ_CLASS)
freq_label <- as.data.frame(table(rfm_frequency$FREQ_CLASS))

# plot the distribution of the 3 frequency classes
plot_frequency <-  (ggplot(data = freq_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                  
  labs(title = "Frequency Distribution",
       x     = "Frequency Classes",
       y     = "Total Purchase") +                
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)
)

plot_frequency

## Monetary, only consider the direction = 1 (purchase)
rfm_monetary <- rfm_study_period %>%
  filter(DIREZIONE == 1)%>% 
  group_by(ID_CLI)%>% 
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup()  %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))

# Divide the type of monetary in 3 different classes
# LOW: below the 25th percentile
# MEDIUM: between the 25th and 75th percentile
# HIGH: above the 75th percentile

rfm_monetary <- within(rfm_monetary,
                       MON_CLASS <- cut(rfm_monetary$SPESA,
                                        breaks = quantile(rfm_monetary$SPESA,
                                                          probs = c(0, .25, .75, 1)),
                                        include.lowest = T,
                                        labels = c("low", "medium", "high"))) 

# compute frequency of the 3 different classes
table(rfm_monetary$MON_CLASS)
mon_label <- as.data.frame(table(rfm_monetary$MON_CLASS))

# plot the distribution of the 3 monetary classes
plot_monetary <- (ggplot(data = mon_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        #-- Dataset to Plot
  geom_bar(stat = "identity") +                   #-- Bar Plot
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "Monetary Distribution",
       x     = "Monetary Classes",
       y     = "Total Amount") +                  #-- Labs
  theme_minimal() +                               #-- ggplot Theme
  theme(plot.title = element_text(hjust = 0.5)) + #-- Centering Title
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)
)

plot_monetary

## Merge all three RFM dataset in one:
rfm <- merge(rfm_frequency, 
             rfm_recency,  
             by = "ID_CLI") 
rfm <- merge(rfm,           
             rfm_monetary,   
             by = "ID_CLI") 

# The recency and frequency groups are combined to define new classes 
# describing the customer loyalty status:

rfm$RF <- NA
for(i in c(1:nrow(rfm))){
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Leaving Top"
}

table(rfm$RF)

# construct the dataframe to be plotted with the RF categories
rf_df <- as.data.frame(rbind(c("Top",         "High",   "Low",    16248),
                             c("Top",         "High",   "Medium", 16248),
                             c("Leaving Top", "High",   "High",   592),
                             c("Engaged",     "Medium", "Low",    36316),
                             c("Engaged",     "Medium", "Medium", 36316),
                             c("Leaving",     "Medium", "High",   27187),
                             c("One Timer",   "Low",    "Low",    32763),
                             c("One Timer",   "Low",    "Medium", 32763),
                             c("Leaving",     "Low",    "High",   27187)))

# Renaming of the columns
colnames(rf_df) <-  c("Level", "Frequency", "Recency", "Value")

rf_df$Frequency <- factor(rf_df$Frequency,
                          levels = c("High", "Medium", "Low"))
rf_df$Recency <- factor(rf_df$Recency,
                        levels = c("High", "Medium", "Low"))
rf_df$Value <- as.numeric(rf_df$Value)

plot_RF <- (ggplot(rf_df, aes(x = Frequency, y = Recency, fill = Value)) + 
  geom_tile() +
    labs(title='RF Model')+
  geom_text(aes(label = Level)) +
  scale_fill_viridis(name='Total customer',option = 'plasma') +
  theme_minimal()+
    theme(plot.title = element_text(size=15,hjust = 0.5),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10, hjust=1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))
)

plot_RF

# create a dataframe containing only the categories and its frequencies
rf <- as.data.frame(table(rfm$RF))
rf

# create a bar plot categories vs num.clients
plot_RF_numcli <- (ggplot(data = rf,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RF Distribution",
       x     = "RF Classes",
       y     = "Total Clients") +                 
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Engaged", "Leaving", "Leaving Top",
                              "One Timer", "Top")) + 
  guides(fill = FALSE)
)

plot_RF_numcli

# Add the variable monetary in order to obtain the categories of the RFM model
rfm$RFM <- NA
for(i in c(1:nrow(rfm))){
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Cheap"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Silver"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Gold"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Gold"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Diamond"
}

table(rfm$RFM)

# construct the dataframe to be plotted with the RFM categories
rfm_df <- as.data.frame(rbind(c("Top", "High", "Diamond", 10984),
                              c("Top", "Medium", "Gold", 5585),
                              c("Top", "Low", "Silver", 10306),
                              c("Leaving Top", "High", "Gold", 5585),
                              c("Leaving Top", "Medium", "Silver", 10306),
                              c("Leaving Top", "Low", "Bronze", 25932),
                              c("Engaged", "High", "Silver", 10306),
                              c("Engaged", "Medium", "Bronze", 25932),
                              c("Engaged", "Low", "Copper", 20938),
                              c("Leaving", "High", "Bronze", 25932),
                              c("Leaving", "Medium", "Copper", 20938),
                              c("Leaving", "Low", "Tin", 24967),
                              c("One Timer", "High", "Copper", 20938),
                              c("One Timer", "Medium", "Tin", 24967),
                              c("One Timer", "Low", "Cheap", 14394)))

# Renaming of the columns
colnames(rfm_df) <- c("RF", "Monetary", "Level", "Value")

rfm_df$RF <- factor(rfm_df$RF,
                    levels = c("Top", "Leaving Top",
                               "Engaged", "Leaving", "One Timer"))
rfm_df$Monetary <- factor(rfm_df$Monetary,
                          levels = c("Low", "Medium", "High"))
rfm_df$Value <- as.numeric(rfm_df$Value)

plot_RFM <-  (ggplot(rfm_df, aes(x = RF, y = Monetary, fill = Value)) + 
  geom_tile() +
    labs(title='RFM Model')+
  geom_text(aes(label = Level)) +
    scale_fill_viridis(name='Total customer',option = 'plasma') +
    theme_minimal()+
    theme(plot.title = element_text(size=15,hjust = 0.5),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10, hjust=1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))
)

plot_RFM

# create a dataframe containing only the categories RFM and its frequencies
rfm_plot <- as.data.frame(table(rfm$RFM))

# create a bar plot categories vs num.clients
plot_RFM_numcli <-  (ggplot(data = rfm_plot,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RFM Distribution",
       x     = "RFM Classes",
       y     = "Total Clients") +                
  theme_minimal() +                             
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper", "Diamond",
                              "Gold", "Silver", "Tin")) + 
  guides(fill = FALSE)
)

plot_RFM_numcli 
