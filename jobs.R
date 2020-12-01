library(tidyverse)

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

pay <- function(dis, ppl) {
  (1/ppl)^0.3 * 500 * (atan(dis/25) / atan(1)) * ppl * dis
}

curve(pay(x, ppl=1)/x, from=1, to=400, main="Pay per 1nm per pax",
      xlab = "Distance [nm]", ylab = "$")
curve(pay(x, ppl=1)/x/x, from=1, to=400, main="Pay per pax",
      xlab = "Distance [nm]", ylab="$")


# Flight time per distance ------------------------------------------------

fit1 <- gam(as.numeric(ft_time) ~ s(Distance), data=c172)
tibble(
  Distance = with(c172, seq(min(Distance), max(Distance), length=100))
) %>%
  mutate(
    p = predict(fit1, .)
  ) %>%
  ggplot(aes(x=Distance, y=p)) +
  geom_line() +
  geom_rug(aes(x=Distance, y=NULL), data=c172, sides="b")

fit2 <- gam(ft_nm ~ s(Distance), data=c172)
tibble(
  Distance = with(c172, seq(min(Distance), max(Distance), length=100))
) %>%
  mutate(
    p = predict(fit2, .)
  ) %>%
  ggplot(aes(x=Distance, y=p)) +
  geom_line() +
  geom_rug(aes(x=Distance, y=NULL), data=c172, sides="b")


# Pay per hour of flight per distance-----------------------------------------

tibble(
  Distance = with(c172, seq(min(Distance), 300, length=100))
) %>%
  mutate(
    ft = predict(fit1, .),
    p = pay(Distance, ppl=1)/Distance / (ft + 15)
  ) %>%
  ggplot(aes(x=Distance, y=p)) +
  geom_line()
  




