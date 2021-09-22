#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#### ???? TO DO df_4 ???? ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

## compute the distribution of the variable FLAG_PRIVACY_1 (consent, no_consent)
df4_dist_privacy <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_privacy

## plot distribution of the variable FLAG_PRIVACY_1
plot_df4_dist_privacy <- (
  ggplot(data=df4_dist_privacy
         , aes(x=FLAG_PRIVACY_1, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_privacy

## compute the distribution of the variable FLAG_PRIVACY_2 (profiling, no_profiling)
df4_dist_profiling <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_profiling

## plot distribution of the variable FLAG_PRIVACY_2
plot_df4_dist_profiling <- (
  ggplot(data=df4_dist_profiling
         , aes(x=FLAG_PRIVACY_2, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_profiling

## compute the distribution of the variable FLAG_DIRECT_MKT 
df4_dist_direct_mark <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_direct_mark

## plot distribution of the variable FLAG_DIRECT_MKT 
plot_df4_dist_direct_mark <- (
  ggplot(data=df4_dist_direct_mark
         , aes(x=FLAG_DIRECT_MKT, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_direct_mark


# cross analysis between privacy and profiling
tab1 <- table(df_4_cli_privacy_clean$FLAG_PRIVACY_1, df_4_cli_privacy_clean$FLAG_PRIVACY_2)
rownames(tab1) <- c("no_privacy","privacy")
colnames(tab1) <- c("no_profiling","profiling")
prop.table(tab1)*100

# cross analysis between privacy and direct marketing
tab2 <- table(df_4_cli_privacy_clean$FLAG_PRIVACY_1, df_4_cli_privacy_clean$FLAG_DIRECT_MKT)
rownames(tab2) <- c("no_privacy","privacy")
colnames(tab2) <- c("no_dir_marketing","dir_marketing")
prop.table(tab2)*100

# cross analysis between direct marketing and profiling
tab3 <- table(df_4_cli_privacy_clean$FLAG_DIRECT_MKT, df_4_cli_privacy_clean$FLAG_PRIVACY_2)
rownames(tab3) <- c("no_dir_marketing","dir_marketing")
colnames(tab3) <- c("no_profiling","profiling")
prop.table(tab3)*100


#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

