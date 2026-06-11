
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(psych)
library(zoo)
library(markovchain)
library(reshape2)



install.packages("depmixS4")
library(depmixS4)

Stocks <- read.csv("C:/Users/HP/Downloads/archive (58)/stocks.csv")
Stocks$Date <- as.Date(stocks$Date)
Stocks <- stocks[order(stocks$Date), ]

str(Stocks)
summary(Stocks)

apple <- Stocks %>% filter(Ticker == "AAPL")
microsoft <- Stocks %>% filter(Ticker == "MSFT")
netflix <- Stocks %>% filter(Ticker == "NFLX")
google <- Stocks %>% filter(Ticker == "GOOGL")

stocks_norm <- Stocks %>%
  group_by(Ticker) %>%
  mutate(Normalized = Close / first(Close))

ggplot(stocks_norm, aes(x = Date, y = Normalized, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "Normalized Stock Price Trends",
       y = "Normalized Price",
       x = "Date") +
  theme_minimal()

stocks <- Stocks %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(Normalized_Close = Close / first(Close))

#closing price trend
ggplot(subset(stocks_norm, Ticker != "GOOG"),
       aes(x = Date, y = Normalized, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "Normalized Closing Price Trends",
       x = "Date",
       y = "Normalized Value") +
  theme_minimal()



#volatility
stocks_norm %>%
  filter(Ticker != "GOOG") %>%
  ggplot(aes(x = Date, y = Volatility, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "7-Day Rolling Volatility",
       x = "Date",
       y = "Volatility (SD of Returns)") +
  theme_minimal()

#daily returns
Stocks <- Stocks %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(Return = (Close / lag(Close)) - 1) %>%
  ungroup()


ggplot(Stocks, aes(x = Date, y = Return, color = Ticker)) +
  geom_line(alpha = 0.6) +
  labs(
    title = "Daily Returns of Stocks",
    x = "Date",
    y = "Daily Return"
  ) +
  theme_minimal()

#volatility
stocks <- Stocks %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(Volatility = rollapply(Return, width = 7, FUN = sd, fill = NA, align = "right"))

ggplot(stocks, aes(x = Date, y = Volatility, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "7-Day Rolling Volatility",
       x = "Date",
       y = "Volatility (SD of Returns)") +
  theme_minimal()

#moving average
Stocks <- Stocks %>%
  filter(Ticker != "GOOGL")

Stocks <- Stocks %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(
    MA7 = rollmean(Close, 7, fill = NA, align = "right"),
    MA14 = rollmean(Close, 14, fill = NA, align = "right")
  ) %>%
  ungroup()

ggplot(Stocks, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), alpha = 0.5) +
  geom_line(aes(y = MA7, color = "MA7"), linewidth = 1) +
  geom_line(aes(y = MA14, color = "MA14"), linewidth = 1) +
  facet_wrap(~Ticker, scales = "free_y") +
  labs(
    title = "Moving Averages of Stock Prices",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal()



#markov chain preparations(creating our states)
Stocks <- Stocks %>%
  mutate(State = case_when(
    Return > 0 ~ "Up",
    Return < 0 ~ "Down",
    TRUE ~ "Stable"
  ))

#i. define customer states within the market.


# Calculate returns
Stocks <- Stocks %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    Return = (Close / lag(Close)) - 1
  ) %>%
  ungroup()

# Define market states
Stocks <- Stocks %>%
  mutate(
    State = case_when(
      Return > 0.01  ~ "Bullish",
      Return < -0.01 ~ "Bearish",
      TRUE           ~ "Stable"
    )
  )

Stocks <- Stocks %>%
  filter(!is.na(State)) %>%
  filter(Ticker != "GOOGL")

Stocks <- Stocks %>%
  filter(
    !is.na(State),
    Ticker %in% c("AAPL", "MSFT", "NFLX")
  )

state_dist <- Stocks %>%
  group_by(Ticker, State) %>%
  summarise(
    Frequency = n(),
    .groups = "drop"
  ) %>%
  group_by(Ticker) %>%
  mutate(
    Percentage = Frequency / sum(Frequency) * 100
  )

state_dist


ggplot(
  state_dist,
  aes(
    x = Ticker,
    y = Percentage,
    fill = State
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Market States Across Stocks",
    x = "Stock",
    y = "Percentage (%)",
    fill = "Market State"
  )

Stocks %>%
  group_by(Ticker) %>%
  summarise(
    Bullish = sum(State == "Bullish"),
    Stable  = sum(State == "Stable"),
    Bearish = sum(State == "Bearish")
  )

Stocks %>%
  group_by(Ticker, State) %>%
  summarise(Freq = n())

Stocks %>%
  group_by(Ticker, State) %>%
  summarise(
    Freq = n(),
    .groups = "drop"
  ) %>%
  group_by(Ticker) %>%
  slice_max(Freq, n = 1)


state_matrix <- table(Stocks$Ticker, Stocks$State)

state_matrix

prop.table(state_matrix, 1)


#To analyze the transition dynamics of competing stocks 
#across Bullish, Stable, and Bearish market states using Markov Chain Analysis.

Stocks <- Stocks %>%
  filter(
    Ticker %in% c("AAPL", "MSFT", "NFLX"),
    !is.na(State)
  ) %>%
  arrange(Ticker, Date)
# create transition matrix
tickers <- unique(Stocks$Ticker)

transition_matrices <- list()

for(t in tickers){
  
  states <- Stocks %>%
    filter(Ticker == t) %>%
    arrange(Date) %>%
    pull(State)
  
  mc_fit <- markovchainFit(data = states)
  
  transition_matrices[[t]] <-
    mc_fit$estimate@transitionMatrix
}

names(transition_matrices)

# ii.Estimate the transition probabilities of moving from one state to another 
#(e.g., gain to loss, loss to stable) based on observed occurrences in the data.

# Ensure ordering within each stock



Stocks_clean <- Stocks %>%
  filter(Ticker %in% c("AAPL", "MSFT", "NFLX"),
         !is.na(State)) %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(next_state = lead(State)) %>%
  ungroup() %>%
  filter(!is.na(next_state))

transitions <- Stocks_clean %>%
  dplyr::select(State, next_state)

# Fit ONE Markov chain for all stocks combined
mc_overall <- markovchainFit(data = transitions)

# Overall transition matrix
overall_matrix <- mc_overall$estimate@transitionMatrix

# Print nicely
cat("====================================\n")
cat("OVERALL TRANSITION PROBABILITY MATRIX\n")
cat("====================================\n")

round(overall_matrix, 4)



mc_df <- as.data.frame(as.table(overall_matrix))

colnames(mc_df) <- c("From", "To", "Probability")

ggplot(mc_df, aes(x = To, y = From, fill = Probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Probability, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Overall Stock Market State Transition Probabilities",
    x = "Next State",
    y = "Current State"
  ) +
  theme_minimal()

mc <- new("markovchain",
          states = rownames(overall_matrix),
          transitionMatrix = overall_matrix)


steady_state <- steadyStates(mc)

steady_state

steady_state_df <- data.frame(
  State = colnames(overall_matrix),
  LongRunProbability = as.numeric(steady_state)
)

steady_state_df

ggplot(steady_state_df, aes(x = State, y = LongRunProbability)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LongRunProbability, 3)), vjust = -0.5) +
  labs(
    title = "Long-Run Probability of Market States",
    x = "State",
    y = "Steady-State Probability"
  ) +
  theme_minimal()

