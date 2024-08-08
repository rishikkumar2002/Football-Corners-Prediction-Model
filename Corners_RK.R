library(readr)
test <- read_csv("Documents/Programming for quant finance/test.csv")
View(test)



# Convert columns to factors
train <- train %>%
  mutate(
    HomeTeamId = as.factor(HomeTeamId),
    AwayTeamId = as.factor(AwayTeamId),
    Home_Goals = as.factor(Home_Goals),
    Away_Goals = as.factor(Away_Goals),
    LeagueId = as.factor(LeagueId)
  )

# Fit Poisson regression model for Home Corners
poisson_model_home <- glm(Home_Corners ~ HomeTeamId + AwayTeamId, 
                          data = train, family = poisson)

# Fit Poisson regression model for Away Corners
poisson_model_away <- glm(Away_Corners ~ HomeTeamId + AwayTeamId,
                          data = train, family = poisson)


# Fit Negative Binomial regression model for Home Corners
negbin_model_home <- glm.nb(Home_Corners ~ HomeTeamId + AwayTeamId, data = train)

# Fit Negative Binomial regression model for Away Corners
negbin_model_away <- glm.nb(Away_Corners ~ HomeTeamId + AwayTeamId, data = train)

# Predict function 
predict_corners <- function(model, newdata) {
  suppressWarnings(predict(model, newdata = newdata, type = "response"))
}



calculate_probabilities <- function(line, total_corners, size = NULL) {
  if (line %% 1 == 0) {
    # Line is an integer
    if (!is.null(size)) {
      # For Negative Binomial distribution
      P_under <- pnbinom(line - 1, size = size, mu = total_corners)
      P_at <- dnbinom(line, size = size, mu = total_corners)
      P_over <- 1 - pnbinom(line, size = size, mu = total_corners)
    } else {
      # For Poisson distribution
      P_under <- ppois(line - 1, lambda = total_corners)
      P_at <- dpois(line, lambda = total_corners)
      P_over <- 1 - ppois(line, lambda = total_corners)
    }
  } else {
    # Line is a decimal
    if (!is.null(size)) {
      # For Negative Binomial distribution
      P_under <- pnbinom(floor(line), size = size, mu = total_corners) 
      P_over <- 1 - pnbinom(floor(line), size = size, mu = total_corners) 
    } else {
      # For Poisson distribution
      P_under <- ppois(floor(line), lambda = total_corners)
      P_over <- 1 - ppois(floor(line), lambda = total_corners)
    }
    P_at <- 0
  }
  
  return(list(P_under = P_under, P_at = P_at, P_over = P_over))
}


test <- test %>%
  mutate(
    HomeTeamId = factor(HomeTeamId, levels = levels(train$HomeTeamId)),
    AwayTeamId = factor(AwayTeamId, levels = levels(train$AwayTeamId))
  )

# Predict expected corners
test <- test %>%
  rowwise() %>%
  mutate(
    Poisson_Home = predict_corners(poisson_model_home, cur_data()),
    NegBin_Home = predict_corners(negbin_model_home, cur_data()),
    Poisson_Away = predict_corners(poisson_model_away, cur_data()),
    NegBin_Away = predict_corners(negbin_model_away, cur_data()),
    Total_Poisson = Poisson_Home + Poisson_Away,
    Total_NegBin = NegBin_Home + NegBin_Away
  ) %>%
  ungroup()

# Calculate probabilities for Poisson
test <- test %>%
  rowwise() %>%
  mutate(
    Poisson_Probs = list(calculate_probabilities(Line, Total_Poisson))
  ) %>%
  mutate(
    `P(Under)_Poisson` = Poisson_Probs$P_under,
    `P(At)_Poisson` = Poisson_Probs$P_at,
    `P(Over)_Poisson` = Poisson_Probs$P_over
  ) %>%
  ungroup()

# Calculate probabilities for Negative Binomial
test <- test %>%
  rowwise() %>%
  mutate(
    NegBin_Probs = list(calculate_probabilities(Line, Total_NegBin, size = negbin_model_home$theta))
  ) %>%
  mutate(
    `P(Under)_NegBin` = NegBin_Probs$P_under,
    `P(At)_NegBin` = NegBin_Probs$P_at,
    `P(Over)_NegBin` = NegBin_Probs$P_over
  ) %>%
  ungroup()

View(test)

library(dplyr)

kelly_criterion <- function(prob, odds) {
  b <- odds - 1
  q <- 1 - prob
  return((b * prob - q) / b)
}

# Expected Value function
expected_value <- function(prob1,prob2,prob3, odds) {
  return(prob1 * (odds - 1) - (prob2))
}

# Initial bankroll
total_units <- 341

# Apply the betting strategy
test <- test %>%
  rowwise() %>%
  mutate(
    EV_under = expected_value(`P(Under)_NegBin`,`P(Over)_NegBin` ,`P(At)_NegBin`,Under),
    EV_over = expected_value(`P(Over)_NegBin`,`P(Under)_NegBin` ,`P(At)_NegBin`,Over),
    
    Bet_U_O = ifelse(EV_under > EV_over, "U", "O"),
    
    kelly = ifelse(Bet_U_O == "U", kelly_criterion(`P(Under)_NegBin`, Under), kelly_criterion(`P(Over)_NegBin`, Over)),
    
    Stake = total_units * kelly
  ) %>%
  ungroup()

test <- test %>%
  mutate(Stake = ifelse(Stake < 0, 0, Stake))

test$Stake <- test$Stake * total_units / sum(test$Stake)

View(test)

test$`P(Under)` = test$`P(Under)_NegBin`
test$`P(Over)` = test$`P(Over)_NegBin`
test$`P(At)` = test$`P(At)_NegBin`
test$`Bet (U/O)` = test$Bet_U_O

