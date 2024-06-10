#Importing dataset, turning into a tsibble object and preprocessing.---------
library(fpp3)
library(dplyr)
dm0 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
df <- dm0 |>
  mutate(Date = yearmonth(Date)) |>
  as_tsibble(index = Date)
dm_train <- df |>
  filter(year(Date) <= 2022)

#Time Series Graphics--------------------------------------------------------------
autoplot(df, Demand) +
  labs(y = "m3",
       title = "2011-2023 Istanbul's Water Demand")
df |>
  gg_season(Demand, labels = "both") +
  labs(y = "m3",
       title = "Seasonal plot: Istanbul's Water Demand")
df |>
  gg_subseries(Demand, labels = "both") +
  labs(y = "% (percent)",
       title = "Istanbul's Dams Occupancy")
df |>
  gg_lag(Demand, geom = "point") +
  labs(x = "lag(Dam.Occupancy, k)")
df |>
  ACF(Demand) |>          #Autocorrelation function
  autoplot() + labs(title="Istanbul's Dams Occupancy")
df |>
  PACF(Demand) |>        #Partial autocorrelation function
  autoplot() + labs(title="Istanbul's Dams Occupancy")

#Exponential Smoothing-----------------------
fit <- dm_train |> 
  model(ETS(Demand))
report(fit)
fm <- fit |> forecast(h = 12)
fm |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  guides(colour = "none")
#ARIMA--------------------
fit2 <- dm_train |> model(ARIMA(Demand))
report(fit2)
ff <- fit2 |> forecast(h = 12)
ff |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit2)) +
  guides(colour = "none")
#Neural Network Model------------
fit3 <- dm_train |>
  model(NNETAR(Demand))
report(fit3)
fn <- fit3 |>
  forecast(h = 12)
fn |>
  autoplot(dm_train) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit3)) +
  guides(colour = "none")
#Accuracy ------------
aa_fit <- dm_train |> 
  model(ETS=ETS(Demand),
        ARIMA=ARIMA(Demand),
        ANN=NNETAR(Demand)
  )
aa_fc <- aa_fit |>
  forecast (h=12)
aa_fc |>
  autoplot(
    df |> filter(year(Date) >= 2020) ,
    level = NULL
  ) +
  labs(
    y = "m3",
    title = "Water Demand"
  ) +
  guides(colour = guide_legend(title = "Forecasts"))

accuracy(aa_fc, df)
