-----------------------------------------#Task B------------------------------------------------------------
####President Election
#Import data
library(readr)
library(tidyverse)

county_facts <- read_csv("county_facts.csv")

general_result <- read_csv("general_result.csv")

crime_data <- read_tsv("crime_data.tsv")

#FIPS
crime_data <- crime_data %>%
  mutate(
    FIPS = str_pad(FIPS_ST, width = 2, side = "left", pad = "0") %>%
      paste0(str_pad(FIPS_CTY, width = 3, side = "left", pad = "0"))
  )

county_facts <- county_facts %>%
  mutate(
    FIPS = str_pad(fips, width = 5, side = "left", pad = "0")
  )

#Merge
df_full <- county_facts %>%
  full_join(crime_data, by = "FIPS") %>%
  full_join(general_result, by = "FIPS")


##Variabler
##Data cleaning

#Remove Alaska
df_full <- df_full %>%
  filter(state_abbr != "AK")

#Correlations
var_num <- df_full %>%
  select(where(is.numeric)) %>%
  names()

for (i in var_num) {
  cor <- cor(df_full[[i]], df_full$vote_rep_per)
  if(abs(cor) > 0.4){
    print(i)
    print(cor)
  }
}

#Names
names <- c(viol_crim_tot = "VIOL",
           prop_crim_tot = "PROPERTY",
           pop = "PST045214",
           female_per = "SEX255214",
           age_65 = "AGE775214",
           white_per = "RHI825214",
           black_per = "RHI225214",
           foreign_born = "POP645213",
           bach_deg = "EDU685213",
           veterans = "VET605213",
           income_median = "INC110213",
           apartment_per = "HSG096213",
           vote_rep_per = "per_gop_2016",
           fips_code = "FIPS",
           state = "state_abbr",
           county = "county_name")

df_full <- rename(df_full, any_of(names))

#Selected variables
df_sub <- df_full %>%
  select(viol_crim_tot,
         prop_crim_tot,
         pop,
         female_per,
         age_65,
         white_per,
         black_per,
         foreign_born,
         bach_deg,
         veterans,
         income_median,
         apartment_per,
         vote_rep_per,
         fips_code, 
         state,
         county) %>%
  mutate(viol_crim_per = viol_crim_tot / pop * 1000,
         prop_crim_per = prop_crim_tot / pop * 1000,
         apartment_per_cut = cut(apartment_per, breaks = c(-Inf, 10, Inf)))

summary(df_sub$apartment_per_cut)

#Missing values
summary(is.na((df_sub))) #no NA


#Plot
#Scetterplot
ggplot(df_sub, aes(black_per, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

#Boxplot
ggplot(df_sub, aes(apartment_per_cut, vote_rep_per)) +
  geom_boxplot()

#



##Test
ggplot(df_sub, aes(viol_crim_per, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(prop_crim_per, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,50))

ggplot(df_sub, aes(pop, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(female_per, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(age_65, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(white_per, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(foreign_born, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(bach_deg, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(veterans, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(income_median, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(df_sub, aes(apartment_per_cut, vote_rep_per)) +
  geom_point() +
  geom_smooth(method = lm)


#Vaiables
county_facts$PST045214 #Population
county_facts$SEX255214 #Female
county_facts$AGE775214 #Over 65
county_facts$RHI825214 #White
county_facts$RHI225214 #Black
county_facts$POP645213 #Foreign born person
county_facts$EDU685213 #Bachelors degree
county_facts$VET605213 #Veterans
county_facts$INC110213 #Median household income
general_result$per_gop_2016
















  




