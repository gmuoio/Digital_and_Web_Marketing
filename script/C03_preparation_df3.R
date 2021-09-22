#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS) # numero totale di indirizzi
            , TOT_ROWs = n()) # numero totale di righe

#!!! NOTE:  there are duplicates, rimuoviamo quindi i duplicati

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarise(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####

#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

# compute the distibution of the number of the addresses associated within a region
tab1 <- table(df_3_cli_address_clean$REGION)
prop.table(tab1)*100

## compute the distribution of the variable REGION
df3_dist_region <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarise(TOT_ADRs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADRs/sum(TOT_ADRs)) %>%
  arrange(desc(PERCENT))

df3_dist_region

## plot distribution of the variable REGION
plot_df3_dist_region <- (
  ggplot(data=df3_dist_region
         , aes(x=REGION, y=TOT_ADRs)) +
    labs(title="Total Address per region",
         x = "Region",
         y= "Total Address",
         cex=10)+
    geom_bar(stat="identity"
             , fill="#1c3d86") +
    theme_minimal() +
    theme(plot.title = element_text(size=15,hjust =0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=10, hjust=1))
)

plot_df3_dist_region

## compute the distribution of the variable CAP
df3_dist_cap <- df_3_cli_address_clean %>%
  group_by(CAP) %>%
  summarise(TOT_ADRs = n()) %>%
  mutate(PERCENT = TOT_ADRs/sum(TOT_ADRs)) %>%
  arrange(desc(PERCENT))

df3_dist_cap

## skip plot because there are too many different values of the variable CAP

## compute the distribution of the variable PRV
df3_dist_prov <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarise(TOT_ADRs = n()) %>%
  mutate(PERCENT = TOT_ADRs/sum(TOT_ADRs)) %>%
  arrange(desc(PERCENT))

df3_dist_prov

## skip plot because there are too many different values of the variable PRV

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)
