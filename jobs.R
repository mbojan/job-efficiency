library(tidyverse)

readr::read_csv("data/aircrafts_-_cessna_172_skyhawk.csv")

c172 <- readr::read_csv(
  "data/flight_logs_-_cessna_172_skyhawk.csv.gz",
  col_types = cols(
    .default = col_double(),
    Aircraft = col_character(),
    FlightTime = col_character(),
    From = col_character(),
    GroupName = col_character(),
    MakeModel = col_character(),
    Pilot = col_character(),
    RentalType = col_character(),
    RentalUnits = col_character(),
    Time = col_datetime(format = ""),
    To = col_character(),
    TotalEngineTime = col_character(),
    Type = col_character()
  )
) %>%
  separate(FlightTime, into=c("ft_days", "ft_time"), sep=" days ") %>%
  separate(TotalEngineTime, into=c("et_days", "et_time"), sep=" days ") %>%
  mutate(
    Time = lubridate::ymd_hms(Time),
    ft_days = as.integer(ft_days),
    ft_time = lubridate::hms(ft_time),
    ft = lubridate::days(ft_days) + ft_time,
    et_days = as.integer(et_days),
    et_time = lubridate::hms(et_time),
    et = lubridate::days(et_days) + et_time,
    ft_nm = as.numeric(ft) / Distance
  ) %>%
  filter(
    Distance > 0, 
    ft_time > lubridate::hms("00:1:0.0"), 
    ft_days == 0, 
    Income > 0,
    Income < 10^5
    )


# Pay per nm --------------------------------------------------------------

ggplot(c172, aes(y=Income)) + geom_histogram()

c172 %>%
  ggplot(aes(x=Distance, y=Income)) +
  geom_point() +
  scale_y_log10()

model <- lm( I(log(Income)) ~ Distance, data=c172)
summary(model)


# Flight time per distance ------------------------------------------------


c172 %>%
  ggplot(aes(x=ft_nm)) +
  geom_histogram() +
  scale_x_log10()

c172 %>%
  ggplot(aes(y = ft_nm, x = Distance)) +
  geom_point() +
  scale_x_log10()
  