library(rvest)
library(dplyr)
library(nnet)

# Pull all team pages links

teams.url <- "http://stats.ncaa.org/team/inst_team_list?academic_year=2017&conf_id=-1&division=1&sport_code=MBB" 

team.list <- as.character(teams.url %>% read_html() %>% 
                            html_nodes(xpath = '//*[@id="contentarea"]/div[4]/div/table'))
team.list <- strsplit(team.list,'a href',fixed = TRUE)[[1]][-1]
team.url.list <- lapply(team.list, function(x) {
  end.url <- gregexpr(12480,x,fixed=TRUE)[[1]][1] + 4
  substr(x,3,end.url)
})
team.url.list <- unlist(team.url.list)
team.url.list <- paste0("http://stats.ncaa.org",team.url.list)

# Pull all game links from team pages

full.game.list <- lapply(team.url.list, function(team.url) {
  game.table.team <- team.url %>% read_html() %>%
    html_nodes('td')
  game.list.team <- grep("/game",game.table.team,fixed=TRUE,value=TRUE)
  game.list.team <- lapply(game.list.team, function(x) {
    start.url <- gregexpr("game",x)[[1]][1]-1
    end.url <- gregexpr("?",x,fixed=TRUE)[[1]][1]
    substr(x,start.url,end.url)
  })
  unlist(game.list.team)
})



test.url <- "http://stats.ncaa.org/game/play_by_play/4320869"

time.frac <- function(time) {
  mins <- as.integer(substr(time,1,2))
  secs <- as.integer(substr(time,4,5))
  decimal <- secs/60
  result <- mins + decimal
  result
}

## produce all times in game
min.vector <- formatC(seq(0,39),width = 2,flag = "0")
sec.vector <- formatC(seq(0,59),width = 2,flag = "0")
all.times <- expand.grid(min.vector,sec.vector) %>% mutate(Time = paste0(Var1,":",Var2)) %>%
  mutate(Time.frac = time.frac(Time)) %>% select(-1,-2) %>% arrange(desc(Time.frac))

get.score.1 <- function(score) {
  as.integer(substr(score,1,regexpr("-",score,fixed=TRUE)[1]-1))
}

get.score.2 <- function(score) {
  as.integer(substr(score,regexpr("-",score,fixed=TRUE)[1]+1,nchar(score)))
}

table.1H <- test.url %>% read_html() %>% html_nodes(xpath = '//*[@id="contentarea"]/table[6]') %>% 
  html_table(header = TRUE)
table.1H <- table.1H[[1]] %>% filter(Time != "End of 1st Half") %>% mutate(Half = 1)
table.2H <- test.url %>% read_html() %>% html_nodes(xpath = '//*[@id="contentarea"]/table[8]') %>% 
  html_table(header = TRUE)
table.2H <- table.2H[[1]] %>% filter(Time != "End of 2nd Half") %>% mutate(Half = 2)
table <- rbind(table.1H,table.2H)
table <- table %>% mutate(Team.1 = colnames(table)[2],Team.2 = colnames(table)[4]) %>%
  rowwise() %>%
  mutate(Score.1 = get.score.1(Score)) %>% 
  mutate(Score.2 = get.score.2(Score)) %>% select(-2,-3,-4) %>% mutate(Time = time.frac(Time)) %>%
  mutate(Time = ifelse(Half == 1,Time + 20, Time)) %>% select(-Half) %>% mutate(Tot.Score = Score.1 + Score.2)
table <- table[!duplicated(table$Time, fromLast=T),]

Team.1.input = table$Team.1[1]
Team.2.input = table$Team.2[1]
Score.1.input = 0
Score.2.input = 0
Tot.Score.input = 0

for (Time.input in all.times$Time.frac) {
  if(Time.input %in% table$Time == TRUE) {
    Score.1.input <- table$Score.1[table$Time == Time.input]
    Score.2.input <- table$Score.2[table$Time == Time.input]
    Tot.Score.input <- table$Tot.Score[table$Time == Time.input]
  } else {
    table <- rbind(table,c(Time.input,Team.1.input,Team.2.input,Score.1.input,Score.2.input,Tot.Score.input))
  }
}
table <- table %>% mutate(Time = as.numeric(Time),Score.1 = as.numeric(Score.1),
                          Score.2 = as.numeric(Score.2),Tot.Score=as.numeric(Tot.Score)) %>% arrange(desc(Time))

game.winner <- ifelse(table$Score.2[table$Time == 0] > table$Score.1[table$Time == 0],2,
                      ifelse(table$Score.2[table$Time == 0] == table$Score.1[table$Time == 0],"Draw",1))
table <- table %>% mutate(Game.Winner = game.winner)
table <- table %>% mutate(Gap12 = Score.1 - Score.2)

model <- multinom(game.winner ~ Gap12 + Time,data=table)