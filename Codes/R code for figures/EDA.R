library(ggplot2)

# With negatives --------------------------------------------------------

# cases by state

oxcgrt_with_neg<- read.csv("oxcgrt_daily_cases_deaths_wNegatives.csv")

str(oxcgrt_with_neg)

summary(oxcgrt_with_neg$Date)

oxcgrt_with_neg$Date <- as.Date(oxcgrt_with_neg$Date)
#oxcgrt_with_neg$Date <- as.Date(oxcgrt_with_neg$Date, format ="%m/%d/%Y")

summary(oxcgrt_with_neg$Date)

oxcgrt_with_neg <- oxcgrt_with_neg[!is.na(oxcgrt_with_neg$Date), ]
oxcgrt_with_neg <- oxcgrt_with_neg[!is.na(oxcgrt_with_neg$cases_daily), ]

ggplot(oxcgrt_with_neg,aes(x = as.Date(Date), y = cases_daily, group=RegionName)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ RegionName, scales = "free_y") +
  labs(title = "Daily COVID-19 Cases by State",
       x = "Date",
       y = "Daily Cases") +
  theme_minimal()





# Australian Capital Territory
act <- subset(oxcgrt_with_neg,RegionName == "Australian Capital Territory")

ggplot(act,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7FB8") +
  geom_point(color = "#2C7FB8") +
  labs(
    title = "COVID-19 Daily Cases in Australian Capital Territory",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After June 2022
act_new <- subset(oxcgrt_with_neg,RegionName == "Australian Capital Territory" &
                            Date >= as.Date("2021-11-01")&
                            Date <= as.Date("2022-12-31"))

ggplot(act_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7FB8") +
  geom_point(color = "#2C7FB8") +
  labs(
    title = "COVID-19 Daily Cases in Australian Capital Territory - After November 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()







# New South Wales
nsw <- subset(oxcgrt_with_neg,RegionName == "New South Wales")

ggplot(nsw,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in New South Wales",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After November 2021
nsw_new <- subset(oxcgrt_with_neg,RegionName == "New South Wales" &
                    Date >= as.Date("2021-11-01")&
                    Date <= as.Date("2022-12-31"))

ggplot(nsw_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in New South Wales - After November 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()







# Northern Territory
nt <- subset(oxcgrt_with_neg,RegionName == "Northern Territory")

ggplot(nt,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in Northern Territory",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After December 2021
nt_new <- subset(oxcgrt_with_neg,RegionName == "Northern Territory" &
                    Date >= as.Date("2021-12-01")&
                    Date <= as.Date("2022-12-31"))

ggplot(nt_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in Northern Territory - After December 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()








# Queensland
qns <- subset(oxcgrt_with_neg,RegionName == "Queensland")

ggplot(qns,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Queensland",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After December 2021
qns_new <- subset(oxcgrt_with_neg,RegionName == "Queensland" &
                   Date >= as.Date("2021-12-01")&
                   Date <= as.Date("2022-12-31"))

ggplot(qns_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Queensland - After December 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()









# South Australia
sa <- subset(oxcgrt_with_neg,RegionName == "South Australia")

ggplot(sa,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in South Australia",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()


# After December 2021
sa_new <- subset(oxcgrt_with_neg,RegionName == "South Australia" &
                    Date >= as.Date("2021-12-01")&
                    Date <= as.Date("2022-12-31"))

ggplot(sa_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in South Australia - After December 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()








# Tasmania
tsmn <- subset(oxcgrt_with_neg,RegionName == "Tasmania")

ggplot(tsmn,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Tasmania",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After December 2021
tsmn_new <- subset(oxcgrt_with_neg,RegionName == "Tasmania" &
                    Date >= as.Date("2021-12-01")&
                    Date <= as.Date("2022-12-31"))

ggplot(tsmn_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Tasmania - After December 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()









# Victoria
vc <- subset(oxcgrt_with_neg,RegionName == "Victoria")

ggplot(vc,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in Victoria",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After December 2021
vc_new <- subset(oxcgrt_with_neg,RegionName == "Victoria" &
                   Date >= as.Date("2021-11-01")&
                   Date <= as.Date("2022-12-31"))

ggplot(vc_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7FCD") +
  geom_point(color = "#2C7FCD") +
  labs(
    title = "COVID-19 Daily Cases in Victoria - After December 2021",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()









# Western Australia
wa <- subset(oxcgrt_with_neg,RegionName == "Western Australia")

ggplot(wa,aes(x = as.Date(Date), y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Western Australia",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# After December 2021
wa_new <- subset(oxcgrt_with_neg,RegionName == "Western Australia" &
                     Date >= as.Date("2022-01-01")&
                     Date <= as.Date("2022-12-31"))

ggplot(wa_new,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7") +
  geom_point(color = "#2C7") +
  labs(
    title = "COVID-19 Daily Cases in Western Australia - After January 2022",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()









# deaths by state

ggplot(oxcgrt_with_neg,aes(x = as.Date(Date), y = deaths_daily, group=RegionName)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ RegionName, scales = "free_y") +
  labs(title = "Daily COVID-19 Deaths by State",
       x = "Date",
       y = "Daily deaths") +
  theme_minimal()




# Without negatives ----------------------------------------------------------

# cases

oxcgrt_without_neg<- read.csv("oxcgrt_new_daily_cases_deaths.csv")

str(oxcgrt_without_neg)

summary(oxcgrt_without_neg$Date)

oxcgrt_without_neg$Date <- as.Date(oxcgrt_without_neg$Date)
#oxcgrt_with_neg$Date <- as.Date(oxcgrt_with_neg$Date, format ="%m/%d/%Y")

summary(oxcgrt_without_neg$Date)

oxcgrt_without_neg <- oxcgrt_without_neg[!is.na(oxcgrt_without_neg$Date), ]
oxcgrt_without_neg <- oxcgrt_without_neg[!is.na(oxcgrt_without_neg$cases_daily), ]




# Australian Capital Territory


# act_nonegatives <- subset(oxcgrt_without_neg,RegionName == "Australian Capital Territory")

# year <- subset(oxcgrt_without_neg, Date >= "2021-06-01" & date <= "2022-12-31")
# year <- oxcgrt_without_neg[oxcgrt_without_neg$Date >= "2021-06-01" & oxcgrt_without_neg$Date <= "2022-12-31", ]


act_nonegatives <- subset(oxcgrt_without_neg, RegionName == "Australian Capital Territory" &
    Date >= as.Date("2021-06-01") &
    Date <= as.Date("2022-12-31"))



ggplot(act_nonegatives,aes(x = Date, y = cases_daily)) +
  geom_line(color = "#2C7FB8") +
  geom_point(color = "#2C7FB8") +
  labs(
    title = "COVID-19 Daily Cases without negatives in Australian Capital Territory",
    x = "Date",
    y = "Daily Cases"
  ) 


# Min-Max verification ---------------------------------------------

oxcgrt_min_max<-read.csv("oxcgrt__9day_centered_cases_deaths.csv")

summary(oxcgrt_min_max)

oxcgrt_min_max$Date <- as.Date(oxcgrt_min_max$Date)

# Full cases_daily
ggplot(oxcgrt_min_max, aes(x = cases_daily)) +
  geom_histogram(bins = 200, fill = "skyblue", color = "black", linewidth = 0.3) +
  labs(title = "Full distribution of daily COVID-19 cases",x = "Daily cases",y = "Frequency") +
  theme_minimal()



# Full 7day rolling average
ggplot(oxcgrt_min_max, aes(x = X7days_rolling_cases)) +
  geom_histogram(bins = 200, fill = "green", color = "black", linewidth = 0.3) +
  labs(title = "Full distribution of 7 day rolling COVID-19 cases",x = "7 day rolling cases",y = "Frequency") +
  theme_minimal()



# Full 7day centered average
ggplot(oxcgrt_min_max, aes(x = X7day_centered_cases)) +
  geom_histogram(bins = 200, fill = "yellow", color = "black", linewidth = 0.3) +
  labs(title = "Full distribution of 7 day centered COVID-19 cases",x = "7 day centered cases",y = "Frequency") +
  theme_minimal()

# Results are identical in both rolling and centered average data.
