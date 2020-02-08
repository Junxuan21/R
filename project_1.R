# -----------------------------------------------------------------
# An analysis of bank's term deposit prediction & marketing
# -----------------------------------------------------------------


# loading packages
library(readxl)
library(tidyverse)
library(recipes)
library(tidyquant)
library(ggplot2)
library(ggrepel)

# loading data from excel sheets
path <- "bank_term_deposit.xlsx"
sheets <- excel_sheets(path)

sheets
#[1] "PROCEDURE"                    "DATA DESCRIPTION"            
#[3] "Step 1 - Collect Information" "CLIENT_INFO"                 
#[5] "LOAN_HISTORY"                 "MARKETING HISTORY"           
#[7] "SUBSCRIPTION HISTORY"         "Step 2 - Merge Information"  
#[9] "CLIENT_MERGE"                 "Step 3 - Marketing Analysis" 
#[11] "DAILY RANGE"                  "JOB ANALYSIS"                
#[13] "Sheet3"   


# pulling data from sheets into R
sheets %>% 
  map(~ read_excel(path = path, sheet = .)) %>%
  set_names(sheets)

# select sheets needed and merge by ID
data_joined_tb <- sheets[4:7] %>%
  map(~ read_excel(path = path, sheet = .)) %>%
  reduce(left_join)

# have a look at the new data as a tibble
data_joined_tb

data_joined_tb %>% glimpse()
# Observations: 45,211
# Variables: 18
# $ ID           <chr> "2836", "2837", "2838", "2839", "2840", "2841",…
# $ AGE          <dbl> 58, 44, 33, 47, 33, 35, 28, 42, 58, 43, 41, 29,…
# $ JOB          <chr> "management", "technician", "entrepreneur", "bl…
# $ MARITAL      <chr> "married", "single", "married", "married", "sin…
# $ EDUCATION    <chr> "tertiary", "secondary", "secondary", "unknown"…
# $ DEFAULT      <chr> "no", "no", "no", "no", "no", "no", "no", "yes"…
# $ BALANCE      <dbl> 2143, 29, 2, 1506, 1, 231, 447, 2, 121, 593, 27…
# $ HOUSING      <chr> "yes", "yes", "yes", "yes", "no", "yes", "yes",…
# $ LOAN         <chr> "no", "no", "yes", "no", "no", "no", "yes", "no…
# $ CONTACT      <chr> "unknown", "unknown", "unknown", "unknown", "un…
# $ DAY          <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
# $ MONTH        <chr> "may", "may", "may", "may", "may", "may", "may"…
# $ DURATION     <dbl> 261, 151, 76, 92, 198, 139, 217, 380, 50, 55, 2…
# $ CAMPAIGN     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
# $ PDAYS        <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,…
# $ PREVIOUS     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
# $ POUTCOME     <chr> "unknown", "unknown", "unknown", "unknown", "un…
# $ TERM_DEPOSIT <chr> "no", "no", "no", "no", "no", "no", "no", "no",…



# transfrom data to perform correlation analysis

?recipe

recipe_obj <- recipe(~. , data = data_joined_tb) %>%
  # first remove the ID variable
  step_rm(ID) %>%
  
  # convert numeric variables into factors and bin them into quantile 
  # to assess correlation within groups
  step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  
  # apply one-hot encoding on all categorical variables
  step_dummy(all_nominal(), one_hot = TRUE,
             naming = partial(dummy_names, sep = "__")) %>%
  prep()

data_transformed_tb <- data_joined_tb %>%
  bake (recipe_obj, new_data =. )

data_transformed_tb %>% glimpse()


# now the new binary data format is good for correlation analysis
# TERM_DEPOSIT variable is the target y that we want to predict
# so we perform correlations using all variables and TERM_DEPOSIT 

corr_tb <- data_transformed_tb %>%
  cor(y = data_transformed_tb$TERM_DEPOSIT__yes) %>%
  
  # convert the matrix to a tibble (nice-looking dataframe)
  as_tibble(rownames = "feature") %>%
  rename(TERM_DEPOSIT__yes = V1) %>%
  
  # separate feature column into two different columns called "feature" and "bin"
  separate(feature, into = c("feature", "bin"), sep = "__") %>%
  
  # remove NA values
  filter(!is.na(TERM_DEPOSIT__yes)) %>%
  
  # remove TERM_DEPOSIT from feature column as this is our target
  filter(!str_detect(feature, "TERM_DEP")) %>%
  
  # arrange according to the absolute value of correlations
  arrange(abs(TERM_DEPOSIT__yes) %>% desc()) %>%
  
  # convert feature column into factors for visualization
  mutate(feature = as_factor(feature) %>% fct_rev())

corr_tb
# A tibble: 63 x 3
#   feature   bin     TERM_DEPOSIT__yes
#   <fct>    <chr>                <dbl>
# 1 DURATION bin4                 0.318
# 2 POUTCOME success              0.307
# 3 DURATION bin1                -0.191
# 4 POUTCOME unknown             -0.167
# 5 CONTACT  unknown             -0.151
# 6 HOUSING  no                   0.139
# 7 HOUSING  yes                 -0.139
# 8 CONTACT  cellular             0.136
# 9 MONTH    mar                  0.129
# 10 MONTH    oct                  0.129
# … with 53 more rows


# visualize correlations
corr_tb %>%
  ggplot(aes(TERM_DEPOSIT__yes, y = feature, text = bin)) +
  
  # geometrics
  geom_vline(xintercept = 0, linetype = 2, color = "red") +
  geom_point(color = "#2c3e50") +
  geom_text_repel(aes(label = bin), size = 3, color = "#2c3e50") +
  
  # formatiing
  expand_limits(x = c(-0.4, 0.4)) +
  theme_tq() +
  labs(title = "Bank Marketing Analysis",
       subtitle = "Correlation to Enrollment in Term Deposit",
       y = "", x = "Correlation to Term Deposit")
  

  
# interpret correlation

# DURATION means the duration since last contact and POUTCOME means prior enrollment outcome.

# from the plot, we see that bin4 of DURATION and the success of POUTCOME have the highest correlation.

# POUTCOME already has labels on the chart, yet we don't know what bin4 means;
# so we need to know the distribution of all the bins within DURATION variable

# pipe recipe object that created into tidy() function shows all the steps
recipe_obj %>% tidy()

# have a look at the second step discretize 
# filter DURATION to see bins within the variable 
bins_tbl <- recipe_obj %>% tidy(2) %>% 
  filter(terms == "DURATION")

bins_tbl
# A tibble: 5 x 3
#  terms    value id              
#  <chr>    <dbl> <chr>           
# 1 DURATION  -Inf discretize_eIM0W
# 2 DURATION   103 discretize_eIM0W
# 3 DURATION   180 discretize_eIM0W
# 4 DURATION   319 discretize_eIM0W
# 5 DURATION   Inf discretize_eIM0W

# now from bin4, we can see that the person that has not been contacted within 319 days
# has a high positive correlation to term deposit, in other words,
# they are very likely to enroll in banks's term deposit.


# implement marketing strategy

strategy_tb <- data_joined_tb %>%
  select(DURATION, POUTCOME, TERM_DEPOSIT) %>%
  
  # add a new column to evaluate the likelihood of term deposit
  mutate(LIKELIHOOD = case_when(
    DURATION > 319 ~ "High",
    POUTCOME == "success" ~ "High",
    TRUE ~ "Normal")) %>%
  
  group_by(LIKELIHOOD) %>%
  count(TERM_DEPOSIT) %>%
  
  # add another column to show the proportion of people getting term deposit
  mutate( Proportion = n/sum(n)) %>%
  
# strategy_tb
  
# A tibble: 4 x 4
# Groups:   LIKELIHOOD [2]
# LIKELIHOOD TERM_DEPOSIT     n Proportion
#  <chr>      <chr>        <int>      <dbl>
# 1 High       no            8364     0.683 
# 2 High       yes           3887     0.317 
# 3 Normal     no           31558     0.957 
# 4 Normal     yes           1402     0.0425
    
# by applying our prediction in the whole dataset, we can see that
# there is 31.7% people in the High likelihood group actually took the term deposit
# while compared to the Normal group, only 4.25% people did that

  ungroup() %>%
  mutate(label_text = str_glue(
    "n: {n}
    Proportion: {scales::percent(Proportion)}" ))
  

# now let's visualize the results we found

strategy_tb %>%
  ggplot(aes(LIKELIHOOD, Proportion, fill = TERM_DEPOSIT)) +
  geom_col() +
  geom_label(aes(label = label_text), fill = "white", color = "#2c3e50") +
  scale_fill_tq() +
  scale_y_continuous(labels = scales:: percent_format()) +
  theme_tq() +
  labs(title =  "Bank Marketing Strategy",
       subtitle = str_glue("Targeting customers that haven't been contacted in 319 days 
                             or those with prior enrollments yields 32% vs 4.25%"))
  


