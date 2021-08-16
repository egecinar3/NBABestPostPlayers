library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)
library(ggthemes)
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

url <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Postup&PlayerOrTeam=P&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"

res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, "text"))

df <- data.frame(json_resp$resultSets$rowSet[1])

colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]

df$PPP <- as.numeric(as.character(df$PPP))
df$POSS <- as.numeric(as.character(df$POSS))
df$PLAYER_NAME <- as.character(df$PLAYER_NAME)
df %>%
  filter(POSS>=2.5, PPP>=0.6) %>%
  ggplot(aes(x = POSS, y = PPP, color = POSS*PPP, size = POSS*PPP)) +
  xlim(c(2,10))+
  xlab("# of Post Up Possessions") +
  ylab("Points per Possession") +
  labs(caption = "Chart: @egecinar3") +
  geom_point(  
    shape = 16,
  ) +
  scale_size(range = c(2, 7)) +
  geom_text(
    aes(label = PLAYER_NAME),
    check_overlap = TRUE,
    nudge_y = -.01
  ) +
  scale_color_gradient(low = "#1565C0", high = "#f12711")+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 16, face = "bold"))

