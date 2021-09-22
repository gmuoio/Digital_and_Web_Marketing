#### FIRST LOOK of df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI) # Numero di clienti totali
            , TOT_ID_FIDs = n_distinct(ID_FID) # Numero di programmi fedeltà
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n()) # numero di righe totali

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))  # formattazione delle date

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID), # numero di fidelity distinte
            NUM_DATEs = n_distinct(DT_ACTIVE)) # numero di date distinte

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution of the variable LAST_COD_FID
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution of the variable LAST_COD_FID
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#### ???? TO DO df_1 ???? ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

## compute distribution of the variable LAST_STATUS_FID (attivo o non attivo) 
df1_dist_status <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_status

## plot distribution of the variable LAST_STATUS_FID
plot_df1_dist_status <- (
  ggplot(data=df1_dist_status
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_status


## compute distribution of the variable FIRST_ID_NEG 
df1_dist_id_neg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_id_neg 

## plot distribution of the variable FIRST_ID_NEG
plot_df1_dist_idneg <- (
  ggplot(data=df1_dist_id_neg
         , aes(x=FIRST_ID_NEG, y=TOT_CLIs)
  ) +
    labs(title="Total customers per store",
         x = "ID store",
         y= "Total customers")+
    geom_bar(stat="identity"
             , fill="#1c3d86") +
    theme_minimal()+
    theme(plot.title = element_text(size=15,hjust =0.5),
          axis.text.x = element_text(size=10, hjust=1),
          axis.text.y = element_text(size=10, hjust=1))+
    geom_text(aes(label=FIRST_ID_NEG), position=position_dodge(width=0.9), vjust=-0.25)
)

plot_df1_dist_idneg

# neg 1 bring many clients to subscript to a fidelity program

 
# combined analysis of status_fidelity (active, inactive) and type_client
tab1 <- table(df_1_cli_fid_clean$LAST_STATUS_FID, df_1_cli_fid_clean$LAST_TYP_CLI_FID)
colnames(tab1) <- c("other","main")
rownames(tab1) <- c("inactive","active")
prop.table(tab1) * 100

# find how many subscriptions are active over the total number of subscriptions
tab2 <- table(df_1_cli_fid_clean$LAST_STATUS_FID)
rownames(tab2) <- c("inactive","active")
prop.table(tab2)*100

# find how many clients are "main" over the total number of clients
tab3 <- table(df_1_cli_fid_clean$LAST_TYP_CLI_FID)
rownames(tab3) <- c("other","main")
prop.table(tab3)*100

## compute distribution of the variable NUM_FIDs
df1_dist_num_fids <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_num_fids 

## plot distribution of the variable NUM_FIDs
plot_df1_dist_numfids <- (
  ggplot(data=df1_dist_num_fids
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_numfids

## compute distribution of the variable FIRST_DT_ACTIVE (first date of activation)
df1_dist_first_date <- df_1_cli_fid_clean %>%
  group_by(FIRST_DT_ACTIVE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(FIRST_DT_ACTIVE)

df1_dist_first_date

## plot distribution of the variable FIRST_DT_ACTIVE
plot_df1_dist_first_date <- (
  ggplot(data=df1_dist_first_date
         , aes(x=FIRST_DT_ACTIVE, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_first_date

# search the date in which there's a peak in the number of clients that subscript
df1_dist_first_date %>% 
  arrange(desc(TOT_CLIs)) %>% 
  head()

## compute distribution of the variable LAST_DT_ACTIVE
df1_dist_last_date <- df_1_cli_fid_clean %>%
  group_by(LAST_DT_ACTIVE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(LAST_DT_ACTIVE)

df1_dist_last_date

## plot distribution of the variable LAST_DT_ACTIVE
plot_df1_dist_last_date <- (
  ggplot(data=df1_dist_last_date
         , aes(x=LAST_DT_ACTIVE, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_last_date

# search the date in which there's a peak in the number of clients that subscript
df1_dist_last_date %>% 
  arrange(desc(TOT_CLIs)) %>% 
  head()

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)
