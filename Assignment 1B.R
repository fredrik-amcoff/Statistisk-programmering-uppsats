-----------------------------------------#Task B------------------------------------------------------------
####President Election
#Import data
library(readr)

county_facts <- read_csv("county_facts.csv")

general_result <- read_csv("general_result.csv")

crime_data <- read_tsv("crime_data.tsv")

summary(county_facts)
summary(general_result)
summary(crime_data)


#Göra fips
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
#Viol
df_full$violcrime_cap <- (df_full$VIOL / df_full$PST045214) * 1000
df_full$violcrime_cap

#Property
df_full$propcrime_cap <- (df_full$PROPERTY / df_full$PST045214) * 1000
df_full$propcrime_cap

plot(df_full$propcrime_cap, df_full$per_gop_2016)

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
