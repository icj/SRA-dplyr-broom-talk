## ---- echo = FALSE, results = 'hide', message = FALSE--------------------
library(Lahman)
library(dplyr)
library(broom)
library(ggplot2)

## ---- echo = FALSE, results = 'markup'-----------------------------------
knitr::kable(data_frame(Function = c("`select()`", "`filter()`", "`mutate()`", "`summarise()`", "`group_by()`", "`left_join()`"),
                        Action = c("Select colums", "Filter rows", "Add columns", 
                                   "Summarize columns", "Perform grouped operations", "SQL-like join operations")))

## ---- echo = FALSE, results = 'markup'-----------------------------------
knitr::kable(data_frame(Function = c("`tidy()`", "`glance()`", "`augment()`"),
                        Action = c("Summarize model findings (like `summary()`)", 
                                   "Concise *one-row* model summary", 
                                   "Add columns to modeled data (like `predict()`)")))

## ------------------------------------------------------------------------
Batting %>% 
  filter(playerID == "mcgwima01") %>% 
  select(playerID, teamID, yearID, HR) %>% 
  filter(yearID == 1997)

## ------------------------------------------------------------------------
Batting %>% 
  filter(playerID == "mcgwima01") %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>% 
  filter(yearID == 1997)

## ------------------------------------------------------------------------
hr <- Batting %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>%  
  ungroup %>% 
  filter(HR >= 40) %>% 
  group_by(playerID) %>% 
  summarise(seasons = n(),
            last_year = max(yearID)) %>% 
  arrange(desc(seasons))

## ---- echo = FALSE-------------------------------------------------------
hr

## ------------------------------------------------------------------------
players <- Master %>% 
  tbl_df %>% 
  select(playerID, nameFirst, nameLast) %>% 
  mutate(letters_last = nchar(nameLast))

## ---- echo = FALSE-------------------------------------------------------
players

## ------------------------------------------------------------------------
hr %>% 
  left_join(players, by = "playerID")  %>% 
  filter(letters_last <= 4)

## ------------------------------------------------------------------------
money <- Salaries %>% 
  group_by(yearID, teamID) %>% 
  summarise(total_salary = sum(salary) / 1000000)

## ---- echo = FALSE-------------------------------------------------------
money

## ---- warning = FALSE----------------------------------------------------
wins <- Teams %>% 
  tbl_df %>% 
  filter(yearID >= 1985) %>% 
  mutate(win_pct = W / (W + L) * 100) %>% 
  select(yearID, teamID, win_pct, LgWin, WSWin) %>% 
  left_join(money, by = c("yearID", "teamID"))

## ---- echo = FALSE-------------------------------------------------------
wins

## ---- echo = FALSE, fig.width = 8, fig.height = 5, fig.align = "center", dev = "svg"----
## Yearly plots
wins %>% 
  filter(yearID <= 1999) %>% 
  mutate(LgWin = ifelse(is.na(LgWin), "N", LgWin),
         WSWin = ifelse(is.na(WSWin), "N", WSWin)) %>% 
  ggplot(aes(total_salary, win_pct)) +
  facet_wrap(~yearID, scales = "free_x", nr = 3) +
  geom_point(aes(color = LgWin, shape = WSWin), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Win Percentage") +
  xlab("Total Team Salary (in millons)") +
  scale_color_manual(name = "League Champ", values = c("black", "red")) +
  scale_shape_discrete(name = "World Series Champ") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 8))

## ---- echo = FALSE, fig.width = 8, fig.height = 5, fig.align = "center", dev = "svg"----
## Yearly plots
wins %>% 
  filter( yearID >= 2000) %>% 
  mutate(LgWin = ifelse(is.na(LgWin), "N", LgWin),
         WSWin = ifelse(is.na(WSWin), "N", WSWin)) %>% 
  ggplot(aes(total_salary, win_pct)) +
  facet_wrap(~yearID, scales = "free_x", nr = 3) +
  geom_point(aes(color = LgWin, shape = WSWin), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Win Percentage") +
  xlab("Total Team Salary (in millons)") +
  scale_color_manual(name = "League Champ", values = c("black", "red")) +
  scale_shape_discrete(name = "World Series Champ") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(size = 8))

## ------------------------------------------------------------------------
m1 <- lm(win_pct ~ total_salary, data = filter(wins, yearID == 2014))
m1

## ------------------------------------------------------------------------
summary(m1)

## ------------------------------------------------------------------------
summary(m1)$coefficients["total_salary", "Pr(>|t|)"]

## ------------------------------------------------------------------------
tidy(m1)

## ------------------------------------------------------------------------
sapply(split(wins, wins$yearID), function(x) {
  mm <- lm(win_pct ~ total_salary, data = x)
  summary(mm)$coefficients["total_salary", "Pr(>|t|)"]
})

## ------------------------------------------------------------------------
wins %>% 
  group_by(yearID) %>% 
  do(tidy(lm(win_pct ~ total_salary, data = .))) %>% 
  filter(term == "total_salary")

