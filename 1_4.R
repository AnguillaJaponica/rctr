install.packages("tidyverse")
library("tidyverse")

email_data <- read.csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>%
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

summary_by_segment <- male_df %>% 
  group_by(treatment) %>% 
  summarise(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n())

mens_mail <- male_df %>% 
  filter(treatment == 1) %>% 
  pull(spend)

no_mail <- male_df %>% 
  filter(treatment == 0) %>% 
  pull(spend)

rct_test <- t.test(mens_mail, no_mail, var.equal = T)

set.seed(1)

obs_rate_c <- 0.5
obs_rate_t <- 0.5

biased_data <- male_df %>%
  mutate(obs_rate_c =
           ifelse( (history > 300) | (recency < 6) |
                     (channel == "Multichannel"), obs_rate_c, 1),
         obs_rate_t =
           ifelse( (history > 300) | (recency < 6) |
                     (channel == "Multichannel"), 1, obs_rate_t),
         random_number = runif(n = NROW(male_df))) %>%
  filter( (treatment == 0 & random_number < obs_rate_c ) |
            (treatment == 1 & random_number < obs_rate_t) )

