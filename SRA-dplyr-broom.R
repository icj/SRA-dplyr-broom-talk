################################################################################
# Data manipulation with dplyr and broom
# Talk for SRA/BSR Meeting on 2015-12-18
# Isaac Jenkins
# ijenkins@fredhutch.org
# December 14, 2015
################################################################################

library(Lahman)
library(dplyr)
library(broom)
library(ggplot2)
library(scales)

# dplyr
# browseVignettes("dplyr")

# broom
# browseVignettes("broom")

# Do work

# Can you name the players with 4 letters or fewer in their last names 
# who have hit 40 or more home runs in a season?
# (source: http://www.sporcle.com/games/deej/40hr4letters)

# Know your data: e.g. Mark McGwire

## The problem
Batting %>% 
  filter(playerID == "mcgwima01") %>% 
  select(playerID, teamID, yearID, HR) %>% 
  filter(yearID == 1997)

## The fix
Batting %>% 
  filter(playerID == "mcgwima01") %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>% 
  filter(yearID == 1997)

# The solution (at the end of 2014)

## Home run info
hr <- Batting %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>%  
  ungroup %>% 
  filter(HR >= 40) %>% 
  group_by(playerID) %>% 
  summarise(seasons = n(),
            last_year = max(yearID)) %>% 
  arrange(desc(seasons))

## Player name info
players <- Master %>% 
  tbl_df %>% 
  select(playerID, nameFirst, nameLast) %>% 
  mutate(letters_last = nchar(nameLast))

## Answer
hr %>% 
  left_join(players, by = "playerID")  %>% 
  filter(letters_last <= 4)

# Team Salaries vs. Win Percentage (1985-2014)

## Team salaries (in millions)
money <- Salaries %>% 
  group_by(yearID, teamID) %>% 
  summarise(total_salary = sum(salary) / 1000000)

## Team wins and champs status
wins <- Teams %>% 
  tbl_df %>% 
  filter(yearID >= 1985) %>% 
  mutate(win_pct = W / (W + L) * 100) %>% 
  select(yearID, teamID, win_pct, LgWin, WSWin) %>% 
  left_join(money, by = c("yearID", "teamID"))

## Yearly plots
wins %>% 
  mutate(LgWin = ifelse(is.na(LgWin), "N", LgWin),
         WSWin = ifelse(is.na(WSWin), "N", WSWin)) %>% 
  ggplot(aes(total_salary, win_pct)) +
  facet_wrap(~yearID, scales = "free_x") +
  geom_point(aes(color = LgWin, shape = WSWin)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = dollar) +
  ylab("Win Percentage") +
  xlab("Total Team Salary (in millons)") +
  scale_color_manual(name = "League Champ", values = c("black", "red")) +
  scale_shape_discrete(name = "World Series Champ") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

## Test the slopes (without broom)
m1 <- lm(win_pct ~ total_salary, data = filter(wins, yearID == 2014))
summary(m1)$coefficients["total_salary", "Pr(>|t|)"]

sapply(split(wins, wins$yearID), function(x) {
  mm <- lm(win_pct ~ total_salary, data = x)
  summary(mm)$coefficients["total_salary", "Pr(>|t|)"]
})

## Test the slopes (with broom)
tidy(m1)
wins %>% 
  group_by(yearID) %>% 
  do(tidy(lm(win_pct ~ total_salary, data = .))) %>% 
  filter(term == "total_salary")
