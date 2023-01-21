setwd("~/Desktop/POLI 281/ps2")
install.packages("lubridate")
library(dplyr)
library(ggplot2)

# Problem 1
jobs <- read.csv("jobs.csv")
jobs <- jobs %>%
  mutate(f_pct_m = total_earnings_female/total_earnings_male)

weighted <- jobs %>%
  group_by(major_category) %>%
  summarize(average = weighted.mean(f_pct_m, total_workers, na.rm = TRUE))

weighted$major_category <- factor(weighted$major_category, levels = weighted$major_category)

f_pct_m_plot <- ggplot(weighted, aes(x = reorder(major_category, -average), y = average)) +
  geom_col()

f_pct_m_plot <- f_pct_m_plot +
  expand_limits(y = 0) +
  expand_limits(x = 0) +
  coord_flip() +
  theme_bw() +
  xlab("Sector") +
  ylab("Female Earnings as a Proportion of Male") +
  geom_hline(yintercept = 1)

f_pct_m_plot

# Problem 2

computer <- jobs %>%
  filter(major_category == "Computer, Engineering, and Science")

computer <- computer %>%
  mutate(prop_male = (workers_male)/total_workers)

computer2 <- computer %>%
  filter(prop_male > 0) %>%
  filter(f_pct_m < 2)

computer_prop_plot <- ggplot(computer2, aes(x=prop_male, y=f_pct_m)) +
  geom_point()
computer_prop_plot

computer_prop_plot <- computer_prop_plot + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_hline(yintercept = 1) +
  xlab("Occupation Proportion Male") +
  ylab("Female Earnings as a Proportion of Male Earnings")

computer_prop_plot

# Problem 3
covid_cases <- read.csv("df_Counties2020.csv")
vaccinations <- read.csv("vaccines.csv")

#c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

to_exclude <- c("American Samoa", "Diamond Princess", "Grand Princess", "Northern Mariana Islands", "Puerto Rico", "Guam", "Virgin Islands")
`%!in%` = Negate(`%in%`)

aggregate_covid <- covid_cases %>%
  rename(State = ST_Name, Date = dt) %>%
  filter(State%!in%to_exclude) %>%
  mutate(Date=lubridate::floor_date(as.Date(Date), 'month')) %>%
  group_by(State, Date) %>%
  summarize(confirmed = sum(Confirmed), deaths = sum(Deaths), population = sum(Population))

aggregate_vaccine <- vaccinations %>%
  rename(State = Province_State) %>%
  filter(Vaccine_Type == "All") %>%
  filter(State%!in%to_exclude) %>%
  mutate(Date=lubridate::floor_date(as.Date(Date), 'month')) %>%
  group_by(State, Date) %>%
  summarize(twice_shot = sum(Stage_Two_Doses, na.rm = TRUE))

combined <- left_join(aggregate_covid, aggregate_vaccine, by = c("State", "Date"))
combined <- combined %>%
  mutate(twice_rate = twice_shot/population)

twice_rate_after_2021 <- combined %>%
  filter(Date >= as.Date("2021-01-01")) #%>%
# ifelse(twice_rate == 0, NA, twice_rate)
twice_rate_after_2021$twice_rate <- ifelse(twice_rate_after_2021$twice_rate == 0 & twice_rate_after_2021$State == "Hawaii", NA, twice_rate_after_2021$twice_rate)
#^ I tried using this line of code to remove the 0 data points from
# Hawaii's data but when you run it, it returns the error: 
# Error in ifelse(., twice_rate == 0, NA, twice_rate) : unused argument (twice_rate)

# As you can see, I also tried many other methods to remove Hawaii's 0 data, all oh which did not work
# replace(twice_rate == 0, NA)
#twice_rate_after_2021 <- replace(twice_rate_after_2021, twice_rate$twice_rate == 0, NA)
# replace_with_na_if(.predicate = is.character, condition = ~.x %in% ("N/A"))


twice_rate_after_aug_2022 <- twice_rate_after_2021 %>%
  filter(Date >= as.Date("2022-08-01"))

max_vax <- which(twice_rate_after_aug_2022$twice_rate == max(twice_rate_after_aug_2022$twice_rate, na.rm = TRUE))
max_vax

min_vax <- which(twice_rate_after_aug_2022$twice_rate == min(twice_rate_after_aug_2022$twice_rate, na.rm = TRUE))
min_vax

twice_rate_after_2021

twice_rate_graph <- ggplot(twice_rate_after_2021, aes(x = Date, y = twice_rate, color = State)) +
  geom_line() +
  xlab("Month") +
  ylab("Twice Vaccinated Rate (%)") +
  geom_hline(color = "Light Blue", lty = 2, yintercept = as.numeric(twice_rate_after_aug_2022[max_vax, "twice_rate"])) +
  annotate(colour = "Light Blue", geom = "text", label= paste(as.character(twice_rate_after_aug_2022[max_vax, "State"]), round(as.numeric(twice_rate_after_aug_2022[max_vax, "twice_rate"]), digits = 2), sep = ": "), x = as.Date("2021-07-01"), y =.85) +
  geom_hline(color = "Pink", lty = 2, yintercept = as.numeric(twice_rate_after_aug_2022[min_vax, "twice_rate"])) +
  annotate(colour = "Pink", geom = "text", label= paste(as.character(twice_rate_after_aug_2022[min_vax, "State"]), round(as.numeric(twice_rate_after_aug_2022[min_vax, "twice_rate"]), digits = 2), sep = ": "), x = as.Date("2022-05-01"), y =.45)

twice_rate_graph


# d
twice_vax_v_deaths <- combined %>%
  filter(Date >= as.Date("2021-01-01")) %>%
  filter(Date <= as.Date("2022-07-01")) %>%
  group_by(Date) %>%
  summarize(national_twice_rate = mean(twice_rate *100, na.rm = TRUE), national_deaths = mean(deaths), national_population = sum(population))


twice_vax_v_deaths$deaths_per_1000 <- (twice_vax_v_deaths$national_deaths/(twice_vax_v_deaths$national_population)*100)

twice_vax_v_deaths_graph <- ggplot(twice_vax_v_deaths, aes(sample = national_twice_rate, theoretical = deaths_per_1000)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Deaths (per thousand)") +
  ylab("Twice-vaccinated rate (%)")

twice_vax_v_deaths_graph


#e

state_rate_plot <- ggplot(combined, aes(x = deaths/(population/1000), y = twice_rate*100)) +
  geom_point() +
  coord_flip() +
  xlab("Deaths per thousand") +
  ylab("Twice-vaccinated Rate (%)") +
  facet_wrap(vars(State)) +
  theme_bw() +
  geom_point(data = transform(combined, State = NULL), colour = "grey85") +
  geom_point() +
  facet_wrap(vars(State)) +
  geom_smooth(formula=y~x, method="loess")

state_rate_plot

#EXTRA CREDIT#


female_dominated_occupations <- jobs %>%
  group_by(major_category) %>%
  summarise(women = sum(workers_female), men = sum(workers_male), total_workers)

female_dominated_occupations_chart <- ggplot(female_dominated_occupations, aes(x = reorder(major_category, -men), y = women, fill = men)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("Sector") +
  ylab("Population of women")

female_dominated_occupations_chart

