# LIBRARIES --------------------------------------------------------------------
library(tidyverse)
library(caret)
library(stringr)

# EDA AND DATA CLEANING ========================================================
# Read data
raw.data <- read.csv(paste0(getwd(),"/data/shot_logs.csv"))
sort(names(raw.data))

# Get an overview of the file
summary(raw.data)
str(raw.data)
head(raw.data)

# Catalogo de jugadores y id
closest.cat <- unique(raw.data[c("CLOSEST_DEFENDER_PLAYER_ID","CLOSEST_DEFENDER")])
shooter.cat <- unique(raw.data[c("player_id", "player_name")])

# Mutate by type of shot
nba <- raw.data %>% 
  mutate(range = case_when (
    SHOT_DIST < 6 & PTS_TYPE == 2 ~ "layup",
    SHOT_DIST >= 6 & SHOT_DIST < 18 & PTS_TYPE == 2 ~ "near",
    SHOT_DIST >= 18 & SHOT_DIST < 24 & PTS_TYPE == 2 ~ "midrange",
    SHOT_DIST >= 22 & SHOT_DIST < 28 & PTS_TYPE == 3 ~ "three",
    SHOT_DIST >= 28 & PTS_TYPE == 3 ~ "longrange",
    TRUE ~ "error"
  ))
# checar cada tipo
table(nba$range) 

# filter out bad shots
shots <- nba %>% 
  filter(range != "error")

# time remaining
shots[c('minute', 'seconds')] <- str_split_fixed(shots$GAME_CLOCK, ':', 2)
shots$PERIOD = as.numeric(as.character(shots$PERIOD))
shots$minute = as.numeric(as.character(shots$minute))
shots$seconds = as.numeric(as.character(shots$seconds))
shots <- shots %>%
  mutate(secs_remaining = ((4 - PERIOD) * 12 * 60) + minute * 60 + seconds)



# plot shot density 
shots %>% 
  ggplot(aes(x=SHOT_DIST, color=SHOT_RESULT, group = SHOT_RESULT))+
  geom_density()+
  xlab("Shot Distance") + ylab("Density") +
  theme_light()

# Logistic Regression ==========================================================

index <- createDataPartition(shots$FGM, p = .70, list = FALSE)
train <- shots[index, ]
test <- shots[-index, ]

# Training the model
logistic_model <- glm(FGM ~ 
                        SHOT_NUMBER +
                        secs_remaining +
                        TOUCH_TIME +
                        DRIBBLES+
                        SHOT_DIST +
                        CLOSE_DEF_DIST,
                      family = binomial(),
                      train)
summary(logistic_model)
