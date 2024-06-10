
#Importing dataset, turning into a tsibble object and preprocessing.---------
library(fpp3)
library(dplyr)
dm0 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
df <- dm0 |>
  mutate(Date = yearmonth(Date)) |>
  as_tsibble(index = Date)
dm <- df %>%
  mutate(Dam.Occupancy = Dam.Occupancy*100)
dm_train <- dm |>
  filter(year(Date) <= 2022)

#Time Series Graphics--------------------------------------------------------------
autoplot(dm, Dam.Occupancy) +
  labs(y = "% (percent)",
       title = "2011-2023 Istanbul's Dams Occupancy")
dm |>
  gg_season(Dam.Occupancy, labels = "both") +
  labs(y = "% (percent)",
       title = "Seasonal plot: Istanbul's Dams Occupancy")
dm |>
  gg_subseries(Dam.Occupancy, labels = "both") +
  labs(y = "% (percent)",
       title = "Istanbul's Dams Occupancy")
dm |>
  gg_lag(Dam.Occupancy, geom = "point") +
  labs(x = "lag(Dam.Occupancy, k)")
dm |>
  ACF(Dam.Occupancy) |>          #Autocorrelation function
  autoplot() + labs(title="Istanbul's Dams Occupancy")
dm |>
  PACF(Dam.Occupancy) |>        #Partial autocorrelation function
  autoplot() + labs(title="Istanbul's Dams Occupancy")

#Exponential Smoothing-----------------------
fit <- dm_train |> 
  model(ETS(Dam.Occupancy))
report(fit1)
fm <- fit |> forecast(h = 12)
fm |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  guides(colour = "none")
#ARIMA--------------------
fit2 <- dm_train |> model(ARIMA(Dam.Occupancy))
report(fit2)
ff <- fit2 |> forecast(h = 12)
ff |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit2)) +
  guides(colour = "none")
#Neural Network Model------------
fit3 <- dm_train |>
  model(NNETAR(Dam.Occupancy))
report(fit3)
fn <- fit3 |>
  forecast(h = 12)
fn |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit3)) +
  guides(colour = "none")
#Accuracy -----
accuracy(ff,dm) #ARIMA
accuracy(fm,dm) #ETS 
accuracy(fn,dm) #NNA

