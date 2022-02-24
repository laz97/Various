#Libraries
library(tidyverse)
library(tidyquant)
library(geckor)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggdark)


#Get LUNA prices
LUNA <- coin_history(
  "terra-luna",
  vs_currency = "usd",
  days = 365,
  interval = NULL,
  max_attempts = 3
)


#Strike multiplier
strike_multiplier <- 1.15
average_premium <- 0.00

#Price vs strike plot function
plot_price_vs_strike <- function(DF, plot_title = "PLOT") {
  p1 <-  ggplot(DF, aes(x=timestamp, y = value, color = type, linetype = type)) +
    geom_line(data = subset(DF, type == "price")) +
    geom_segment(data = subset(DF, type == "simulated_strike"), aes(x = lag(timestamp), y = value,
                                                                    xend = timestamp, yend = value)) +
    scale_color_brewer(palette = "Set3") +
    dark_theme_gray() +
    ggtitle(plot_title) +
    xlab("Date") + 
    ylab("Price")
  
  ggplotly(p1)
}

#Luna price and simulated strikes YTD
LUNA %>%
  mutate(week = floor_date(timestamp, "week", getOption("lubridate.week.start", 5))) %>%
  mutate(prev = if_else(is.na(lag(price, default = NA)), price, lag(price, default = NA))) %>%
  group_by(week) %>%
  mutate(simulated_strike = round(first(prev)*strike_multiplier*2)/2,
         breakeven = simulated_strike / price - 1) %>%
  ungroup() %>%
  pivot_longer(., cols = c("price", "simulated_strike"), names_to = "type", values_to = "value") %>%
  filter(timestamp >= as.Date("2022-01-01")) %>%
  plot_price_vs_strike(., plot_title = "YTD price vs simulated strikes")
  
#Luna price and simulated strikes last year
LUNA %>%
  mutate(week = floor_date(timestamp, "week", getOption("lubridate.week.start", 5))) %>%
  mutate(prev = if_else(is.na(lag(price, default = NA)), price, lag(price, default = NA))) %>%
  group_by(week) %>%
  mutate(simulated_strike = round(first(prev)*strike_multiplier*2)/2,
         breakeven = simulated_strike / price - 1) %>%
  ungroup() %>%
  pivot_longer(., cols = c("price", "simulated_strike"), names_to = "type", values_to = "value") %>%
  filter(timestamp >= Sys.Date() - 364) %>%
  plot_price_vs_strike(., plot_title = "Last year's price vs simulated strikes")

#Luna price and simulated strikes last 365 summary
LUNA %>%
  mutate(week = floor_date(timestamp, "week", getOption("lubridate.week.start", 5))) %>%
  mutate(prev = if_else(is.na(lag(price, default = NA)), price, lag(price, default = NA))) %>%
  group_by(week) %>%
  mutate(simulated_strike = round(first(prev)*strike_multiplier*2)/2,
         breakeven = simulated_strike / price - 1) %>%
  summarise(price = last(price), simulated_strike = last(simulated_strike), weekly_breakeven = last(breakeven)) %>%
  mutate(weekly_underperformance = if_else(weekly_breakeven < 0 , 1,0)) %>%
  summarise(weeks = n(), underperformace = sum(weekly_underperformance)) %>%
  knitr::kable(.,col.names = c("# Weeks",
               "#Covered Calls Underperformance"))


#Simulated portfolio function
simulate_portfolios <- function(DF, output = "plot") {
LUNA_RETURNS <- DF %>%
  mutate(prev = if_else(is.na(lag(price, default = NA)), price, lag(price, default = NA))) %>%
  mutate(week = floor_date(timestamp, "week", getOption("lubridate.week.start", 5))) %>%
  group_by(week) %>%
  mutate(first = first(prev)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(covered_call_returns = min(c(price/first-1,strike_multiplier-1)) + average_premium) %>%
  ungroup()

#Simulated portfolios returns
portfolios_returns <- left_join(LUNA_RETURNS, LUNA_RETURNS %>%
              group_by(week) %>%
              summarise(weekly_returns = last(covered_call_returns)) %>%
              mutate(weekly_multiplier = weekly_returns + 1,
                     lag_cum_multiplier = replace_na(lag(cumprod(weekly_multiplier),n=1), 1)) %>%
              select(week, lag_cum_multiplier)) %>%
  mutate(covered_call_portfolio = 100 * lag_cum_multiplier * (1+covered_call_returns),
         hold_luna_portfolio = price/first(price) * 100) 

#Plot
portfolios_plot <- portfolios_returns %>%  pivot_longer(., cols = c("hold_luna_portfolio", "covered_call_portfolio"), names_to = "portfolio", values_to = "value") %>%
  ggplot(., aes(x=timestamp, y = value, color = portfolio, linetype = portfolio)) +
  geom_line() +
  scale_color_brewer(palette = "Set3") +
  dark_theme_gray() 

plot <- ggplotly(portfolios_plot)

#Summary table
table <- portfolios_returns %>%
  select(timestamp, week, hold_luna_portfolio,covered_call_portfolio) %>%
  group_by(week) %>%
  summarise(hold_luna_portfolio =last(hold_luna_portfolio), covered_call_portfolio = last(covered_call_portfolio)) %>%
  mutate(hold_luna_returns = hold_luna_portfolio/lag(hold_luna_portfolio, n = 1, default = 100),
         covered_call_returns = covered_call_portfolio/lag(covered_call_portfolio, n = 1, default = 100),
         covered_call_underperformance = if_else(covered_call_returns < hold_luna_returns,1,0),
         covered_call_average_underperformance = covered_call_underperformance *  (covered_call_returns-hold_luna_returns)) %>%
  summarise(weeks = n(),
            hold_luna_portfolio_returns = last(hold_luna_portfolio)/100-1,
            covered_call_portfolio_returns = last(covered_call_portfolio)/100-1,
            covered_call_underperformance = sum(covered_call_underperformance),
            covered_call_average_underperformance = sum(covered_call_average_underperformance)/covered_call_underperformance,
            breakeven_average_yield = ((1+hold_luna_portfolio_returns)/(1+covered_call_portfolio_returns))^(1/weeks)-1)  %>%
  mutate(hold_luna_portfolio_returns = scales::percent(hold_luna_portfolio_returns), 
         covered_call_portfolio_returns = scales::percent(covered_call_portfolio_returns), 
         covered_call_average_underperformance = scales::percent( covered_call_average_underperformance, accuracy = 0.01), 
         breakeven_average_yield = scales::percent(breakeven_average_yield, , accuracy = 0.01)) %>%
  knitr::kable(., col.names = c("# Weeks",
                                "Hold Luna Ret%",
                                "Covered Calls Ret%",
                                "#Covered Calls Underperformance",
                                "Covered Calls Average Underperformance",
                                "Average Premium for Breakeven"))
  
  
#Output
if(output == "plot") {plot} else if (output == "table") {table} else {warning("output misspecified")}

}

#YTD simulated portfolio plot
LUNA %>%
  filter(timestamp >= as.Date("2022-01-01")) %>%
  simulate_portfolios()

#YTD simulated portfolio table
LUNA %>%
  filter(timestamp >= as.Date("2022-01-01")) %>%
  simulate_portfolios(., "table")
  

#Yearly simulated portfolio plot
LUNA %>%
  simulate_portfolios()

#Yearly simulated portfolio table
LUNA %>%
  simulate_portfolios(., "table")

  




