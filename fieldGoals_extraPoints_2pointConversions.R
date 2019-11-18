library(XML)
library(xml2)
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)
library(ggimage)
library(binr)
library(Stat2Data)
library(broom)
library(RColorBrewer)

scrapePFRFieldGoals <- function(url1, url2, url3, url4, url5, startyr, endyr, startdist, enddist) {
  fieldGoals <- data.frame()
  for (i in startyr:endyr) {
    for (j in startdist:enddist) {
      cat('Loading Distance', j, '\n')
      URL <- paste0(url1, as.character(i), url2, as.character(i),  url3, as.character(j), url4, as.character(j), url5)
      test <- try(        read_html(URL) %>% 
                           html_node('body') %>% 
                           html_node("#all_attempts") %>% 
                           html_nodes(xpath = 'comment()') %>%
                           html_text() %>%
                           read_html() %>%
                           html_node('table') %>%
                           html_table() %>%
                           data.frame())
      if(inherits(test, "try-error")){
        next
      } else {
      dist_table <- read_html(URL) %>% 
        html_node('body') %>% 
        html_node("#all_attempts") %>% 
        html_nodes(xpath = 'comment()') %>%
        html_text() %>%
        read_html() %>%
        html_node('table') %>%
        html_table() %>%
        data.frame()
      fieldGoals <- rbind(dist_table, fieldGoals)
      }
    }
  }
  return(fieldGoals)
}

fieldGoals <- scrapePFRFieldGoals(
  "http://www.pro-football-reference.com/play-index/fg_finder.cgi?request=1&year_min=",
  "&year_max=",
  "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&min_distance=",
  "&max_distance=",
  "&order_by=game_date&order_by_asc=Y",
  2005,
  2019,
  20,
  76)

scrapePFRTwoPointConversions <- function(url1, url2, url3, startyr, endyr){
  twoPointConversions <- data.frame()
  for (i in startyr:endyr){
    cat('Loading Year', i, '\n')
    URL <- paste0(url1, as.character(i), url2, as.character(i),  url3)
    twoPointTable <- read_html(URL) %>% 
      html_node('body') %>% 
      html_node("#div_all_plays") %>%
      html_node('table') %>%
      html_table() %>%
      data.frame()
    twoPointConversions <- rbind(twoPointTable, twoPointConversions)
  }
  return(twoPointConversions)
}


twoPointConversions <- scrapePFRTwoPointConversions(
  "http://www.pro-football-reference.com/play-index/play_finder.cgi?request=1&match=all&year_min=",
  "&year_max=",
  "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&quarter%5B%5D=1&quarter%5B%5D=2&quarter%5B%5D=3&quarter%5B%5D=4&quarter%5B%5D=5&minutes_max=15&seconds_max=00&minutes_min=00&seconds_min=00&down%5B%5D=0&down%5B%5D=1&down%5B%5D=2&down%5B%5D=3&down%5B%5D=4&field_pos_min_field=team&field_pos_max_field=team&end_field_pos_min_field=team&end_field_pos_max_field=team&type%5B%5D=2PC&no_play=N&turnover_type%5B%5D=interception&turnover_type%5B%5D=fumble&score_type%5B%5D=touchdown&score_type%5B%5D=field_goal&score_type%5B%5D=safety&rush_direction%5B%5D=LE&rush_direction%5B%5D=LT&rush_direction%5B%5D=LG&rush_direction%5B%5D=M&rush_direction%5B%5D=RG&rush_direction%5B%5D=RT&rush_direction%5B%5D=RE&pass_location%5B%5D=SL&pass_location%5B%5D=SM&pass_location%5B%5D=SR&pass_location%5B%5D=DL&pass_location%5B%5D=DM&pass_location%5B%5D=DR&order_by=game_date&order_by_asc=Y",
  2005,
  2019
)

scrapePFRExtraPoints <- function(url1, url2, url3, url4, startyr, endyr) {
  extraPoints <- data.frame()
  for (i in startyr:endyr) {
    for (j in 1:4) {
      cat('Loading Quarter', j, '\n')
      URL <- paste0(url1, as.character(i), url2, as.character(i),  url3, as.character(j), url4)
        xp_table <- read_html(URL) %>% 
          html_node('body') %>% 
          html_node("#div_all_plays") %>%
          html_node('table') %>%
          html_table() %>%
          data.frame()
        extraPoints <- rbind(xp_table, extraPoints)
      }
    }
  return(extraPoints)
  }
  
extraPoints <- scrapePFRExtraPoints(
  "http://www.pro-football-reference.com/play-index/play_finder.cgi?request=1&match=all&year_min=",
  "&year_max=",
  "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&quarter%5B%5D=",
  "&minutes_max=15&seconds_max=00&minutes_min=00&seconds_min=00&down%5B%5D=0&down%5B%5D=1&down%5B%5D=2&down%5B%5D=3&down%5B%5D=4&field_pos_min_field=team&field_pos_max_field=team&end_field_pos_min_field=team&end_field_pos_max_field=team&type%5B%5D=XP&no_play=N&turnover_type%5B%5D=interception&turnover_type%5B%5D=fumble&score_type%5B%5D=touchdown&score_type%5B%5D=field_goal&score_type%5B%5D=safety&rush_direction%5B%5D=LE&rush_direction%5B%5D=LT&rush_direction%5B%5D=LG&rush_direction%5B%5D=M&rush_direction%5B%5D=RG&rush_direction%5B%5D=RT&rush_direction%5B%5D=RE&pass_location%5B%5D=SL&pass_location%5B%5D=SM&pass_location%5B%5D=SR&pass_location%5B%5D=DL&pass_location%5B%5D=DM&pass_location%5B%5D=DR&order_by=game_date&order_by_asc=Y",
  2005,
  2019
)

extraPoints %>% select(Yds) %>% distinct()
extraPoints %>% select(EPB) %>% distinct()
extraPoints %>% select(EPA) %>% distinct()
extraPoints %>% select(Diff) %>% distinct()
str(extraPoints)
extraPoints <- extraPoints %>% select(1:5, 8:10)

write_csv(extraPoints, path = "extraPoints.csv")


str(twoPointConversions)
twoPointConversions %>% select(Yds) %>% distinct()
twoPointConversions %>% select(EPB) %>% distinct()
twoPointConversions %>% select(EPA) %>% distinct()
twoPointConversions %>% select(Diff) %>% distinct()
twoPointConversions <- twoPointConversions %>% select(1:5, 8:10)
write_csv(twoPointConversions, path = "twoPointConversions.csv")

str(fieldGoals)
fieldGoals <- fieldGoals %>% select(-Var.4)
write_csv(fieldGoals, path = "fieldGoals.csv")

twoPointConversions <- read_csv("twoPointConversions.csv")
str(twoPointConversions)
twoPointConversions <- twoPointConversions %>% mutate(Result = ifelse(str_detect(Detail, "succeeds"), 1,
                               ifelse(str_detect(Detail, "fails"), 0, NA)))
twoPointConversions %>% group_by(Result) %>% summarize(number = n())
which(is.na(twoPointConversions$Result))
twoPointNAs <- twoPointConversions[which(is.na(twoPointConversions$Result)), 8]
twoPointConversions[c(45, 128, 136, 162, 170), 9] <- 1
twoPointConversions[c(165, 174, 346, 360), 9] <- 0


twoPointConversions <- twoPointConversions %>% select(-Location)
twoPointConversions <- twoPointConversions %>% drop_na(Result)


twoPointConversions %>% group_by(year(Date)) %>% summarize(number = n()) %>% arrange(`year(Date)`)

twoPointConversionTeamAmount <- twoPointConversions %>% group_by(Tm) %>% summarize(number = n()) %>% arrange(desc(number))

mean(twoPointConversions$Result)

extraPoints <- read_csv("extraPoints.csv")

str(extraPoints)
extraPoints <- extraPoints %>% mutate(Result = ifelse(str_detect(Detail, "no"), 0,
                                                          ifelse(str_detect(Detail, "good"), 1, 
                                                                 ifelse(str_detect(Detail, "blocked"), 0, NA))))

extraPoints %>% group_by(Result) %>% summarize(number = n())
which(is.na(extraPointsTest$Result))

extraPoints <- extraPoints %>% select(-Location) %>% drop_na(Result)

fieldGoals <- read_csv("fieldGoals.csv")
fieldGoals <- fieldGoals %>% select(-Rk)
fieldGoals <- fieldGoals %>% mutate(Good = ifelse(str_detect(`Good.`, "Y"), 1,
                                                          ifelse(str_detect(`Good.`, "N"), 0, NA)), Block = ifelse(str_detect(`Blk.`, "Y"), 1,
                                                                                                                   ifelse(str_detect(`Blk.`, "N"), 0, NA)))
fieldGoals <- fieldGoals %>% select(-Good., -Blk.)

fieldGoals <- fieldGoals %>% mutate(Season = if_else(month(Date) < 3, year(Date) - 1, year(Date)))
extraPoints <- extraPoints %>% mutate(Season = if_else(month(Date) < 3, year(Date) - 1, year(Date)))
twoPointConversions <- twoPointConversions %>% mutate(Season = if_else(month(Date) < 3, year(Date) - 1, year(Date)))
# ----------------------------------------------------------------------
# Graph formatting
nflGraphTheme <- function() {
  theme_classic() + 
    theme(text = element_text(family = "Georgia", color = "gray25"),
          plot.title = element_text(size = 24, face = "bold"), 
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(color = "gray30", size = 12),
          legend.position = "right",
          legend.text = element_text(color = "gray30", size = 12),
          legend.title = element_text(color = "gray30", size = 14),
          axis.text = element_text(color = "gray25", size = 14)
    )
    
}
# ----------------------------------------------------------------------
# Grouped Bar Chart of Extra Point and Two Point conversions over the years
# and Percent Stacked Bar Chart
extraPointsbySeason <- extraPoints %>% 
  group_by(Season) %>% 
  filter(Season != 2019) %>%  
  summarize(Number_ExtraPoints = n())

twoPointsbySeason <- twoPointConversions %>% 
  group_by(Season) %>% 
  filter(Season != 2019) %>% 
  summarize(Number_TwoPoints = n())

extraPointTwoPointbySeason <- merge(extraPointsbySeason, twoPointsbySeason, by = "Season")
extraPointTwoPointbySeason <- extraPointTwoPointbySeason %>% gather(key = "Play", value = "Amount", -Season)
extraPointTwoPointbySeason$Season <- as.character(extraPointTwoPointbySeason$Season)

nflBlue <- "#013369"
nflRed <- "#D50A0A"
# Grouped Bar Chart
ggplot(extraPointTwoPointbySeason, aes(fill = Play, x = Season, y = Amount)) +
  geom_bar(position="dodge", stat="identity") + scale_y_log10(expand = c(0, 0)) +
  scale_fill_manual(values = c(nflRed, nflBlue), name = "Play", labels = c("Extra Points", "Two Point Conversions")) +
  nflGraphTheme() + xlab('') + ylab('Log Base 10') + labs(title = "Number of NFL Extra Point and Two Point Attempts by Season", subtitle = "2005-2018", caption = "Data from https://www.pro-football-reference.com/")
  

# Percent Stacked Bar Chart
ggplot(extraPointTwoPointbySeason, aes(fill = Play, x = Season, y = Amount)) +
  geom_bar(position="fill", stat="identity") + scale_fill_manual(values = c(nflRed, nflBlue), name = "Play", labels = c("Extra Points", "Two Point Conversions")) +
  nflGraphTheme() + xlab('') + ylab('') + labs(title = "Percentage of NFL Extra Point and Two Point Attempts by Season", subtitle = "2005-2018", caption = "Data from https://www.pro-football-reference.com/") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), labels = paste0(seq(from = 0, to = 100, by = 10), "%"), expand = c(0, 0))

# Dual Expected Value Line Graph
extraPointExpectedValue <- extraPoints %>% 
  group_by(Season) %>% 
  filter(Season < 2015) %>% 
  summarize(ExtraPointConversion = mean(Result)) %>% 
  mutate(ExtraPointExpectedValue = (1 * ExtraPointConversion))

twoPointExpectedValue <- twoPointConversions %>% 
  group_by(Season) %>% 
  filter(Season < 2015) %>% 
  summarize(TwoPointConversion = mean(Result)) %>% 
  mutate(TwoPointExpectedValue = (2 * TwoPointConversion))

extraPointTwoPointExpectedValuebySeason <- merge(extraPointExpectedValue, 
                                               twoPointExpectedValue, by = "Season")
extraPointTwoPointExpectedValuebySeason <- extraPointTwoPointExpectedValuebySeason %>% 
  select(1, 3, 5) %>% 
  gather(key = "Play", value = "Expected_Value", -Season)

ggplot() + 
  geom_line(data = extraPointTwoPointExpectedValuebySeason, 
            aes(x = Season, y = Expected_Value, col = Play), size = 3) +
  geom_line(data = extraPointTwoPointExpectedValuebySeason, 
            aes(x = Season, y = Expected_Value, col = Play), size = 3) +
  geom_point(data = extraPointTwoPointExpectedValuebySeason, 
            aes(x = Season, y = Expected_Value, col = Play), size = 5) +
  geom_point(data = extraPointTwoPointExpectedValuebySeason, 
            aes(x = Season, y = Expected_Value, col = Play), size = 5) +
  nflGraphTheme() + scale_color_manual(values = c(nflRed, nflBlue), name = "Play", labels = c("Extra Points", "Two Point Conversions")) +
  scale_x_continuous(breaks = 2005:2014) + scale_y_continuous(breaks = seq(from = 0.85, to = 1.15, by = 0.05)) +
  xlab('') + ylab('') + labs(title = "Expected Value of NFL Extra Point and Two Point Attempts by Season", subtitle = "2005-2014", caption = "Data from https://www.pro-football-reference.com/")
  

# Dual Line and Bar graph of Number of two point conversions increasing post 2014 but
# conversion rate not suffering
twoPointsbySeasonPostChange <- twoPointConversions %>% 
  group_by(Season) %>% 
  filter(Season >= 2011 & Season < 2019) %>% 
  summarize(Number_TwoPoints = n()) %>% 
  arrange(Season)

twoPointsConversionPercentagePostChange <- twoPointConversions %>% 
  group_by(Season) %>% 
  filter(Season >= 2011 & Season < 2019) %>% 
  summarize(Conversion_Percentage = mean(Result)) %>% 
  arrange(Season) %>% 
  mutate(Conversion_Percentage = Conversion_Percentage * 100)



ggplot() + 
  geom_bar(data = twoPointsbySeasonPostChange, aes(x = Season, y = Number_TwoPoints), stat = "identity", fill = nflBlue) +
  geom_line(data = twoPointsConversionPercentagePostChange, aes(x = Season, y = Conversion_Percentage), size = 3, color = nflRed) +
  geom_point(data = twoPointsConversionPercentagePostChange, aes(x = Season, y = Conversion_Percentage), size = 5, color = nflRed) +
  scale_y_continuous(name = "Number of Two Point Attempts", 
                     sec.axis = sec_axis(~., name = "Two Point Conversion Percentage", 
                                         labels = function(b) { paste0(round(b, 0), "%")})) +
  scale_x_continuous(breaks = 2011:2018, expand = c(0, 0)) +
  theme_classic() +
  theme(text = element_text(family = "Georgia", color = "gray25"),
        plot.title = element_text(size = 24, face = "bold"), 
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(color = "gray30", size = 12),
        legend.position = "right",
        legend.text = element_text(color = "gray30", size = 12),
        legend.title = element_text(color = "gray30", size = 14),
        axis.text = element_text(color = "gray25", size = 14),
        axis.title.y = element_text(color = nflBlue, size = 18, face = "bold"),
        axis.title.y.right = element_text(color = nflRed, size = 18, face = "bold")
    ) + xlab('') + labs(title = "Two Point Attempts & the Conversion Percentage by Season", subtitle = "Before and after 2015 Rule Change", caption = "Data from https://www.pro-football-reference.com/")

# This is the image scatter plot of post-Rule Change teams two point amount and conversion Percentage
twoPointConversionsTeamPostChange <- twoPointConversions %>% 
  filter(Season > 2014) %>% 
  group_by(Tm) %>% 
  summarize(Amount = n(), Conversion_Rate = mean(Result))

twoPointConversionsTeamPostChange$image <- c("SF_49ers.png", "CHI_Bears.png", 
                                             "CIN_Bengals.png", "BUF_Bills.png", 
                                             "DEN_Broncos.png", "CLE_Browns.png", 
                                             "TB_Buccaneers.png", "AZ_Cardinals.png", 
                                             "LA_Chargers.png", "KC_Chiefs.png", 
                                             "IND_Colts.png", "DAL_Cowboys.png", 
                                             "MIA_Dolphins.png", "PHI_Eagles.png", 
                                             "ATL_Falcons.png", "NY_Giants.gif", 
                                             "JAX_Jaguars.png", "NY_Jets.png", 
                                             "DET_Lions.png", "GB_Packers.png", 
                                             "CAR_Panthers.png", "NE_Patriots.png", 
                                             "OAK_Raiders.png", "LA_Rams.png", 
                                             "BAL_Ravens.png", "WAS_Redskins.png", 
                                             "NO_Saints.png", "SEA_Seahawks.png", 
                                             "PIT_Steelers.png", "HOU_Texans.png", 
                                             "TEN_Titans.png", "MIN_Vikings.png")
# For reference, average extra point percentage Post rule change is:
extraPoints %>% 
  filter(Season > 2014 & Season < 2019) %>% 
  summarize(Conversion_Percentage = mean(Result))

twoPointConversionsTeamPostChange %>% arrange(Conversion_Rate)

ggplot(twoPointConversionsTeamPostChange, aes(Amount, Conversion_Rate)) + geom_image(aes(image=image), size=.1) + 
  theme_classic() + 
  nflGraphTheme() + xlab('') + ylab('') + geom_hline(yintercept = .446, linetype = "dashed", color = "black", size = 2) +
  geom_label(data = twoPointConversionsTeamPostChange, aes(x = 30, y = 0.41, label = "Converstion Rate equivalent\nto Extra Point\nExpected Value"), color = "black") +
  scale_y_continuous(labels = c("20%", "30%", "40%", "50%", "60%", "70%")) +
  labs(title = "NFL teams Total Two Point Attempts and Converstion Rate", subtitle = "After 2015 Rule Change", caption = "Data from https://www.pro-football-reference.com/")

# Graph of decline of field goal percentage by yard and decline of extra point once moved
# back
# This is to make the side-by-side line graphs of field goal percentage by distance and extra points
fieldGoalDistanceFrequency <- fieldGoals %>% 
  group_by(Dist) %>% 
  summarize(Field_Goal_Conversion_Percentage = mean(Good))


ggplot(data = fieldGoalDistanceFrequency, aes(x = Dist, y = Field_Goal_Conversion_Percentage)) + geom_line()

preRuleExtraPoints <- extraPoints %>% filter(Season < 2015)
postRuleExtraPoints <- extraPoints %>% filter(Season >= 2015 & Season < 2019)

preRuleExtraPointPercentage <- mean(preRuleExtraPoints$Result)
postRuleExtraPointPercentage <- mean(postRuleExtraPoints$Result)

extraPointDistanceFrequency <- data.frame("Distance" = c(20,33), 
                                          "Conversion_Percentage" = c(preRuleExtraPointPercentage, postRuleExtraPointPercentage))
# This needs to be in one dataframe, I am going to insert it manually
names(fieldGoalDistanceFrequency) <- c("Dist", "Conversion_Percentage")
fieldGoalDistanceFrequency$Play <- rep("Field Goal", times = 52)
names(extraPointDistanceFrequency) <- c("Dist", "Conversion_Percentage")
extraPointDistanceFrequency$Play <- c("Extra Point", "Extra Point")
fieldGoalExtraPointDistFreq <- bind_rows(fieldGoalDistanceFrequency, extraPointDistanceFrequency)

ggplot() + geom_line(data = fieldGoalExtraPointDistFreq, aes(x = Dist, y = Conversion_Percentage, col = Play), size = 3) +
  geom_line(data = fieldGoalExtraPointDistFreq, aes(x = Dist, y = Conversion_Percentage, col = Play), size = 3) +
  nflGraphTheme() + scale_color_manual(values = c(nflRed, nflBlue), name = "Play", labels = c("Extra Points", "Field Goals")) +
  xlab('') + ylab('') + labs(title = "Conversion Rate of NFL Field Goals & Extra Points by Distance", subtitle = "2005-2018", caption = "Data from https://www.pro-football-reference.com/") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), labels = paste0(seq(from = 0, to = 100, by = 10), "%")) + scale_x_continuous(breaks = (seq(from = 20, to = 76, by = 4)))



## Logistic Regression Dataframe
fieldGoalsLogReg <- fieldGoals %>% filter(Season != 2019) %>% select(Dist, Good)

ggplot(data = fieldGoalsLogReg, mapping = aes(x = Dist, y = Good)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) + geom_smooth(method = "lm", se = FALSE)

ggplot(data = fieldGoalsLogReg, mapping = aes(x = Dist, y = Good)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) + geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))

fieldGoalMod <- glm(Good ~ Dist, data = fieldGoalsLogReg, family = binomial)

# Creating binned dataframe
fieldGoalsLogRegBin <- fieldGoalsLogReg %>% 
  group_by(bin = cut(Dist, breaks = seq(20, 76, by = 4))) %>% 
  summarize(mean_Dist = mean(Dist), completion_rate = mean(Good)) %>% na.omit()

# Binned Points and line
ggplot(data = fieldGoalsLogRegBin, mapping = aes(x = mean_Dist, y = completion_rate)) + geom_point() + geom_line()

# augmented model
fieldGoalMod_plus <- augment(fieldGoalMod, type.predict = "response")

# logistic model on probability scale
ggplot(data = fieldGoalsLogRegBin, mapping = aes(x = mean_Dist, y = completion_rate)) + geom_point(color = nflBlue, size = 5) + geom_line(color = nflBlue, size = 3) +
  geom_label(data = fieldGoalsLogRegBin, aes(x = 59, y = 0.57, label = "Actual Field Goals"), color = nflBlue) +
  geom_line(data = fieldGoalMod_plus, aes(x = Dist, y = .fitted), color = nflRed, size = 3) +
  geom_label(data = fieldGoalMod_plus, aes(x = 70, y = 0.31, label = "Field Goal\nLogistic Model"), color = nflRed) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "black", size = 2) +
  nflGraphTheme() + xlab('') + ylab('') + labs(title = "NFL Field Goals by Distance with Logistic Probability Model", subtitle = "2005-2018", caption = "Data from https://www.pro-football-reference.com/") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), labels = paste0(seq(from = 0, to = 100, by = 10), "%")) + scale_x_continuous(breaks = (seq(from = 20, to = 76, by = 4)))

# logistic model on odds scale
# If the probability of making a field goal is y, then the odds are y/(1−y).
# Create the odds column in the binned dataframe
fieldGoalsLogRegBin <- fieldGoalsLogRegBin %>% 
  mutate(odds = completion_rate / (1 - completion_rate))

# scatterplot for odds as a function of mean_Dist using 
# the binned data in fieldGoalsLogRegBin. Points connected with geom_line().

ggplot(data = fieldGoalsLogRegBin, aes(x = mean_Dist, y = odds)) + 
  geom_point() + geom_line()

# compute odds for observations
fieldGoalMod_plus <- fieldGoalMod_plus %>% 
  mutate(odds_hat = .fitted / (1 - .fitted))

# logistic model on odds scale
ggplot(data = fieldGoalsLogRegBin, aes(x = mean_Dist, y = odds)) + 
  geom_point() + geom_line() +
  geom_line(data = fieldGoalMod_plus, aes(x = Dist, y = odds_hat), color = "red")

# Most people tend to interpret the fitted values on the probability scale and the 
# function on the log-odds scale. The interpretation of the coefficients is most 
# commonly done on the odds scale.

# create new data frame
new_data <- data.frame(Dist = 50)

# make predictions
augment(fieldGoalMod, type.predict = "response", newdata = new_data)

# data frame with binary predictions
tidy_mod <- augment(fieldGoalMod, type.predict = "response") %>% mutate(Good_hat = round(.fitted))


# confusion matrix to see what percentage accurate the model is with all of our data
tidy_mod %>%
  select(Good, Good_hat) %>% 
  table()

# Who were the best kickers at ~50yard distnace and what were their percentages
# Justin Tucker and Chris Boswell both at 85% accuracy since 2005
fieldGoals2018kickers <- fieldGoals %>%
  filter(between(Dist, 48, 52)) %>% 
  group_by(Player) %>% 
  summarize(Percentage = mean(Good), Amount = n()) %>% 
  filter(Amount >= 16) %>% 
  arrange(desc(Percentage))

fieldGoals %>%
  filter(Dist == 50) %>% 
  summarize(Percentage = mean(Good))

# The extra point should be 50 yards!

# Who are the best kickers and how much better are they than the average kicker
fieldGoals %>%
  filter(Dist > 45) %>% 
  filter(Season > 2014 & Season < 2019) %>%
  group_by(Player) %>% 
  summarize(Percentage = mean(Good), Amount = n()) %>% 
  filter(Amount > 18) %>% 
  arrange(desc(Percentage))

BestDistanceKickers <- c("Robbie Gould", "Matt Bryant", "Adam Vinatieri", "Jake Elliott", "Phil Dawson", "Chris Boswell", "Justin Tucker")
BestDistanceKickers <- append(BestDistanceKickers, "NFL Average") %>% sort()

bestKickers <- fieldGoals %>% 
  filter(Season > 2014 & Season < 2019) %>%
  group_by(Player, bin = cut(Dist, breaks = seq(20, 76, by = 4))) %>%
  filter(Player %in% BestDistanceKickers) %>%
  summarize(mean_Dist = mean(Dist), completion_rate = mean(Good)) %>% na.omit()

# How do the best kickers do from the 48 to 52 range as compared to what the model would say the average kicker would do?
fieldGoals %>% 
  filter(Season > 2014 & Season < 2019) %>%
  filter(between(Dist, 48, 52)) %>% 
  filter(Player %in% BestDistanceKickers) %>% 
  summarize(completion_rate = mean(Good))

# 83%! Almost 20% better

averageKickers <- fieldGoals %>% 
  filter(Season > 2014 & Season < 2019) %>%
  group_by(bin = cut(Dist, breaks = seq(20, 76, by = 4))) %>%
  summarize(mean_Dist = mean(Dist), completion_rate = mean(Good)) %>% na.omit()

averageKickers$Player <- "NFL Average"
allKickers <- bind_rows(bestKickers, averageKickers)
playerColors <- c("#003B78", "#21201C", "#004650", "#2A2C8A", "#CB2340", nflBlue, "#C4021D", "#F26522")
# Ehh that doesn't work, colors too close to tell them apart

ggplot() + geom_line(data = allKickers, aes(x = mean_Dist, y = completion_rate, col = Player), size = 3) + 
  scale_color_brewer(palette = "Set1") +
  nflGraphTheme() + xlab('') + ylab('') + labs(title = "Best NFL Field Goal Kickers vs. League Average by Distance", subtitle = "2015-2018", caption = "Data from https://www.pro-football-reference.com/") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), labels = paste0(seq(from = 0, to = 100, by = 10), "%")) + scale_x_continuous(breaks = (seq(from = 20, to = 76, by = 4))) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "black", size = 2)

# Getting average Extra Point expected value and Two-Point conversion expected value after rule change

extraPoints %>% 
  filter(Season > 2014 & Season < 2019) %>%
  summarize(Expected_Value = mean(Result))

twoPointConversions %>% 
  filter(Season > 2014 & Season < 2019) %>%
  summarize(Expected_Value = 2 *(mean(Result)))
