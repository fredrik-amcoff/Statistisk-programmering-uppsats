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
  filter(state_abbr != "AK") %>%
  select(-c(STUDYNO, EDITION, PART)) #zero variance

#Correlations
var_num <- df_full %>%
  select(where(is.numeric)) %>%
  names()

for (i in var_num) {
  cor <- cor(df_full[[i]], df_full$per_gop_2016)
  if(abs(cor) > 0.4){
    print(i)
    print(cor)
  }
}

#Names
names <- c(viol_crim_tot = "VIOL",
           prop_crim_tot = "PROPERTY",
           pop_tot = "PST045214",
           female_per = "SEX255214",
           age_65_per = "AGE775214",
           white_per = "RHI825214",
           black_per = "RHI225214",
           asian_per = "RHI425214",
           foreign_born_per = "POP645213",
           bach_deg_per = "EDU685213",
           veterans_tot = "VET605213",
           income_median = "INC110213",
           hous_multi_per = "HSG096213",
           vote_rep_prop = "per_gop_2016",
           fips_code = "FIPS",
           state = "state_abbr",
           county = "county_name")

df_full <- rename(df_full, any_of(names))

#Selected variables
df_sub <- df_full %>%
  select(viol_crim_tot,
         prop_crim_tot,
         pop_tot,
         female_per,
         age_65_per,
         white_per,
         black_per,
         asian_per,
         foreign_born_per,
         bach_deg_per,
         veterans_tot,
         income_median,
         hous_multi_per,
         vote_rep_prop,
         fips_code, 
         state,
         county) %>%
  mutate(viol_crim_per = viol_crim_tot / pop_tot * 1000,
         
         prop_crim_per = prop_crim_tot / pop_tot * 1000,
         
         vote_rep_per = vote_rep_prop * 100,
         
         hous_multi_per_cut = cut(hous_multi_per, 
                                 breaks = c(-Inf, 10, Inf),
                                 labels = c("≤10",
                                            ">10")),
         bach_deg_per_cut = cut(bach_deg_per, 
                            breaks = c(-Inf, 20, Inf),
                            labels = c("≤20",
                                       ">20")),
         bach_deg_per_cut_4 = cut(bach_deg_per, 
                                breaks = c(-Inf, 10, 20, 30, Inf),
                                labels = c("≤10",
                                           "11-20",
                                           "21-30",
                                           ">30")),
         asian_per_cut = cut(asian_per, 
                            breaks = c(-Inf, 1, Inf),
                            labels = c("≤1",
                                       ">1")),
         black_per_cut = cut(black_per, 
                             breaks = c(-Inf, 2, Inf),
                             labels = c("≤2",
                                        ">2")))


#Missing values
summary(is.na((df_sub))) #no NA


###Plot 3 first
#Scatterplot
scatter_simple <- 
  ggplot(df_sub, aes(x = black_per, y = vote_rep_per)) +
  geom_point(colour = "grey55", alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(
    x = "Black people in a county (%)",
    y = "Votes for Trump (%)"
  ) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0", "20", "40", "60", "80", "100")) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray93"),
        axis.ticks.y = element_line(colour = "gray93"),
        axis.ticks.x = element_line(colour = "gray93"))

scatter_simple

ggsave("Scatter_simple.png", plot = scatter_simple) #save


#Boxplot
boxplot <- 
  ggplot(df_sub, aes(hous_multi_per_cut, vote_rep_per)) +
  geom_boxplot() +
  labs(
    x = "Housing units in multi-unit structures (%)",
    y = "Votes for Trump (%)"
  ) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray93"),
        axis.ticks.y = element_line(colour = "gray93"),
        axis.ticks.x = element_line(colour = "gray93"))

boxplot

ggsave("Boxplot.png", plot = boxplot) #save

#Barplot
barplot <- 
  ggplot(df_sub, aes(x = bach_deg_per_cut_4, y = vote_rep_per)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    x = "Bachelor degree or higher (%)",
    y = "Votes for Trump (%)"
  ) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray93"),
        axis.ticks.y = element_line(colour = "gray93"),
        axis.ticks.x = element_line(colour = "gray93"))

barplot

ggsave("Barplot.png", plot = barplot) #save

#Plot extra
#Scatterplot mapped to a variable
scatterplot_mapped <- 
  ggplot(df_sub, aes(black_per, vote_rep_per, colour = bach_deg_per_cut)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Black people in a county (%)",
    y = "Votes for Trump (%)",
    colour = "Bachelor degree or higher (%)") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0", "20", "40", "60", "80", "100")) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray93"),
        axis.ticks.y = element_line(colour = "gray93"),
        axis.ticks.x = element_line(colour = "gray93"),
        legend.position = c(0.7, 0.85))

scatterplot_mapped

ggsave("Scatterplot_mapped.png", plot = scatterplot_mapped) #save

#Facetted plot
facetted_plot <-
  ggplot(df_sub, aes(bach_deg_per, vote_rep_per)) +
  geom_point(colour = "grey55", alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(
    x = "Bachelor degree or higher (%)",
    y = "Votes for Trump (%)") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0", "20", "40", "60", "80", "100")) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray93"),
        axis.ticks.y = element_line(colour = "gray93"),
        axis.ticks.x = element_line(colour = "gray93")) +
  facet_grid(black_per_cut ~ asian_per_cut,
             labeller = labeller(
               asian_per_cut = c("≤1" = "Asian ≤1%", ">1" = "Asian >1%"),
               black_per_cut = c("≤2" = "Black ≤2%", ">2" = "Black >2%")))

facetted_plot

ggsave("Facetted_plot.png", plot = facetted_plot) #save


#Unconditional summaries
#vote_rep_per
sum_vote_rep_per <- df_sub %>%
    summarise(
    mean = mean(vote_rep_per),
    median = median(vote_rep_per),
    sd = sd(vote_rep_per)
  )

#bach_deg_per
sum_bach_deg_per <- 
  df_sub %>%
  summarise(
    mean = mean(bach_deg_per),
    median = median(bach_deg_per),
    sd = sd(bach_deg_per)
  )

#black_per
sum_black_per <- 
  df_sub %>%
  summarise(
    mean = mean(black_per),
    median = median(black_per),
    sd = sd(black_per)
  )

#asian_per
sum_asian_per <- 
  df_sub %>%
  summarise(
    mean = mean(asian_per),
    median = median(asian_per),
    sd = sd(asian_per)
  )

#hous_multi_per
sum_hous_multi_per <- 
  df_sub %>%
  summarise(
    mean = mean(hous_multi_per),
    median = median(hous_multi_per),
    sd = sd(hous_multi_per)
  )

sum_tot <- bind_rows(sum_vote_rep_per, 
                     sum_bach_deg_per, 
                     sum_black_per, 
                     sum_asian_per, 
                     sum_hous_multi_per)

sum_tot_df <- as.data.frame(sum_tot)
  
rownames(sum_tot_df) <- c("Vote (%)", 
                          "Bachelor degree (%)", 
                          "Black (%)", 
                          "Asian (%)", 
                          "Hous (%)")
sum_tot_df

library(knitr)
kable(sum_tot_df, format = "latex", booktabs = TRUE) #latex code


#Grouped summaries
group_sum_bach <- 
  df_sub %>%
  group_by(bach_deg_per_cut) %>%
  summarise(
    mean = mean(vote_rep_per),
    median = median(vote_rep_per),
    sd = sd(vote_rep_per)
  )

kable(group_sum_bach, format = "latex", booktabs = TRUE) #latex code

group_sum_hous <- 
  df_sub %>%
  group_by(hous_multi_per_cut) %>%
  summarise(
    mean = mean(vote_rep_per),
    median = median(vote_rep_per),
    sd = sd(vote_rep_per)
  )
kable(group_sum_hous, format = "latex", booktabs = TRUE) #latex code


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


