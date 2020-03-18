library(dplyr)

college_data <- read.csv("college_data.csv")
covers_data <- read.csv("covers_data.csv")

# Получаю прогноз Propensity score
model <-lm(win ~ line, covers_data)
summary(model)
prop_score <- predict(model, covers_data)
covers_data['prop_score'] <- prop_score

# Соединяю таблички
final_data <- inner_join(college_data, covers_data, by=c('teamname', 'year'))

# Matching
library(MatchIt)

#remove missing
no_missing_data <- final_data[complete.cases(final_data[c('win', 'year', 'athletics_donors', 'line', 'prop_score')]),]
no_missing_data <- no_missing_data[c('win', 'year', 'athletics_donors', 'line', 'prop_score')]

matching <- matchit(win ~ line, no_missing_data)
summary(matching)
weights <- matching$weights

# final effect
model <- lm(athletics_donors ~ win, no_missing_data, weights=weights)
summary(model)


# Blocking

final_data 

# Weighting

weights <- (no_missing_data$win / no_missing_data$prop_score) + (1 - no_missing_data$win) / (1 - no_missing_data$prop_score)

#dropping 0.1 - 0.9
weights <- weights[no_missing_data$prop_score > 0.1 & no_missing_data$prop_score < 0.9]
data_for_weighting <- no_missing_data[no_missing_data$prop_score > 0.1 & no_missing_data$prop_score < 0.9,]

# estimating
model <- lm(athletics_donors ~ win, data_for_weighting, weights=weights)
summary(model)


