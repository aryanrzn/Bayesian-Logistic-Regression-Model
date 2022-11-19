# R Packages -------------------------------------------

library(readxl)
library(ggfortify)
library(arm)
library(dplyr)

# Excel File --------------------------------------------

data0 <- read_excel("NBA 2022 Advanced Stats.xlsx",
  col_types = c(
    "numeric", "text", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
    "numeric", "text", "numeric", "numeric"
  )
)

data1 <- data0 %>%
  select(
    # Rk, # Rank
    # Team,
    Age, # Player's age on Febury 1 of the season
    W, # Wins
    L, # Losses
    PW, # Pythagorean wins, i.e., expected wins based on points scored and allowed
    PL, # Pythagorean losses, i.e., expected losses based on points scored and allowed
    MOV, # Margin of Victory
    SOS, # Strength of Schedule; a rating of strength. The rating is denominated in points above/below average, where zero in average.
    SRS, # Simple Rating System; a team rating that takes into account average point differential and strength of schedule. The rating is denominated in points above/below average, where zero is average.
    ORtg, # Offensive Rating; An estimate of points produced (players) or scored (teams) per 100 possessions
    DRtg, # Defensive Rating; An estimate of points allowed per 100 possessions
    NRtg, # Net Rating; an estimate of point differential per 100 possessions
    Pace, # Pace Factor; An estimate of possessions per 48 minutes
    FTr, # Free Throw Attempt Rate; Number of FT Attempts Per FG Attempt
    R3PA, # 3-Point Attempt Rate; Percentage of FG Attempts from 3-Point Range
    TS, # True Shooting Percentage; A measure of shooting efficiency that takes into account 2-point
    Offense_eFG, # Effective Field Goal Percentage; This statistic adjusts for the fact that a 3-point field goal is worth one more point than a 2-point field goal.
    Offense_TOV, # Turnover Percentage; An estimate of turnovers committed per 100 plays.
    Offense_ORB, # Offensive Rebound Percentage; An estimate of the percentage of available offensive rebounds a player grabbed while they were on the floor.
    Offense_FT_FGA, # Free Throws Per Field Goal Attempt
    Defense_eFG, # Opponent Effective Field Goal Percentage
    Defense_TOV, # Opponent Turnover Percentage
    Defense_ORB, # Defensive Rebound Percentage; An estimate of the percentage of available offensive rebounds a player grabbed while they were on the floor.
    Defense_FT_FGA, # Opponent Free Throws Per Field Goal Attempt
    # Arena,
    Attend,
    Attend_G
  )
print(data1, n = 5, width = Inf)

# Base Model ------------------------------------------------------

model_base <- glm(cbind(W, L) ~ ., family = binomial(), data = data1)
summary(model_base)
par(mfrow = c(2, 2))
plot(model_base)
par(mfrow = c(1, 1))
data_model_base <- fortify(model_base, ) %>%
  as_tibble() %>%
  mutate(
    real = with(data0, W / (W + L)),
    pred = invlogit(.fitted)
  )
print(data_model_base, n = 5, width = Inf)
with(data_model_base, plot(real, pred))
abline(0, 1)

# Reduced Model -----------------------------------------------------

model_stepwise <- step(model_base)
anova(model_stepwise, model_base)
p_value <- 1 - pchisq(q = 3.6008, df = 11)
p_value
summary(model_stepwise)
par(mfrow = c(2, 2))
plot(model_stepwise)
par(mfrow = c(1, 1))
data_model_stepwise <- fortify(model_stepwise, ) %>%
  as_tibble() %>%
  mutate(
    real = with(data0, W / (W + L)),
    pred = invlogit(.fitted)
  )
with(data_model_stepwise, plot(real, pred))
abline(0, 1)

# Bayesian Model ----------------------------------------------

model_bayes <- bayesglm(
  formula = model_stepwise$formula,
  family = binomial(),
  data = data1,
  prior.df = Inf,
  prior.df.for.intercept = Inf
)
model_bayes$prior.mean
model_bayes$prior.scale
model_bayes$prior.df
summary(model_bayes)
data_model_bayes <- data1 %>%
  mutate(
    .fitted = predict(model_bayes, type = "response"),
    real = with(data0, W / (W + L))
  )
with(data_model_bayes, plot(real, .fitted))
abline(0, 1)
