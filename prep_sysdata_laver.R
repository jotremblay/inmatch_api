# Prepare serve return for atp players
library(devtools)
library(oncourt)
library(elodb)

### Update serve data for singles and doubles in oncourt
#source(file = "~/Software/oncourt/prep_sysdata.R")

### Reinstall oncourt
#devtools::install("~/Software/oncourt/")

rm(list = ls())

load(file = "~/Software/inmatch_api_10/R/sysdata.rda")

### List of laver cup players
laver_players <- data.frame(
	name = c(
	"Roger Federer",
	"Rafael Nadal",
	"Dominic Thiem",
	"Alexander Zverev",
	"Stefanos Tsitsipas",
	"Fabio Fognini",
	"Roberto Bautista Agut",
	"John Isner",
	"Jack Sock",
	"Nick Kyrgios",
	"Denis Shapovalov",
	"Milos Raonic",
	"Taylor Fritz",
	"Jordan Thompson"	
),
	World = rep(c(0, 1), each = 7),
	stringsAsFactors = F
)

### Retrieve names as in player database
con <- make_connection(server = 'prd-db-ta', database = 'TennisMatchStats')

player_names <- tbl(con, "Player") %>% collect() 

player_names <- player_names %>%	
	dplyr::mutate(
		name = paste(FirstName, LastName),
		tour = substr(PlayerCode, 1, 3)
	) %>%
	rename(playerid = PlayerCode) 
	
player_names <- player_names[grepl("^ATP", player_names$playerid),]

player_names <- player_names %>%
	inner_join(laver_players, by = "name")
	
nrow(laver_players)
nrow(player_names)	

### Merge with oncourt player info for oncourt ID 
### Fix names for hawkeye
data(atp_players, package = "oncourt")
data(atp_doubles, package = "oncourt")

oncourt_players <- atp_players %>%
	select(name = NAME_P, ID_P)

oncourt_doubles_players <- atp_doubles %>%
	select(NAME1, NAME2, Player1, Player2)
	
oncourt_doubles_players <- rbind(
	oncourt_doubles_players %>% select(name = NAME1, ID_P = Player1),
	oncourt_doubles_players %>% select(name = NAME2, ID_P = Player2)
)	%>% unique()


### Fix names; Merge with player table
oncourt_players$name[oncourt_players$name == "Diego Sebastian Schwartzman"] <- "Diego Schwartzman"

oncourt_doubles_players$name[oncourt_doubles_players$name == "Diego Sebastian Schwartzman"] <- "Diego Schwartzman"

oncourt_players$name[oncourt_players$name == "Taylor Harry Fritz"] <- "Taylor Fritz"

oncourt_doubles_players$name[oncourt_doubles_players$name == "Taylor Harry Fritz"] <- "Taylor Fritz"


player_names$name <- inmatch::clean_names(player_names$name)
oncourt_players$name <- inmatch::clean_names(oncourt_players$name)
oncourt_doubles_players$name <- inmatch::clean_names(oncourt_doubles_players$name)

players <- player_names %>%
	inner_join(oncourt_players, by = "name") %>%
	dplyr::mutate(
		ID_P = as.character(ID_P)
	)

doubles_players <- player_names %>%
	inner_join(oncourt_doubles_players, by = "name") %>%
	dplyr::mutate(
		ID_P = as.character(ID_P)
	)


### Get h2h and current singles and doubles elo ratings
con <- elodb::make_connection()

atp_elo <- get_current(con, basetable = "atp_ratings", surface = "Hard")

atp_elo_doubles <- get_current(con, basetable = "atp_doubles_ratings", surface = "Hard")

atp_elo <- atp_elo %>% rename(ID_P = playerid) %>%
	inner_join(players, by = "ID_P")
	
atp_elo_doubles <- atp_elo_doubles %>% rename(ID_P = playerid) %>%
	inner_join(doubles_players, by = "ID_P")
	
### head to head effects
h2h <- elodb::create_h2h(con, "atp_ratings_historical")

h2h <- h2h %>%
	rename(ID_P = playerid, ID_O = opponentid) %>%
	dplyr::mutate(
		ID_P = as.character(ID_P)
	) %>%
	inner_join(players %>% select(playerid, ID_P), by = "ID_P") %>%
	inner_join(players %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")
	
### Create all possible world and europe matchups
matchup_grid <- rbind(
	expand.grid(ID_P = players$ID_P[players$World == 1], 
	ID_O =  players$ID_P[players$World == 0]),
	expand.grid(ID_P = players$ID_P[players$World == 0], 
	ID_O =  players$ID_P[players$World == 1])	
	)

### Create serve expectations for each matchup
### Event determines the tournament average
### Get most recent Masters indoor hardcourt event
event <- oncourt::get_eventid(atp = T, surface = 3)

serve_priors <- efron_morris(event = event, atp = T, surface = "Hard", doubles = F)

serve_priors$ID_P <- as.character(serve_priors$ID_P)

atp_serve_priors <- matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))
	

atp_serve_priors <- atp_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)

doubles_matchup_grid <- rbind(
	expand.grid(
	ID_P = doubles_players$ID_P[doubles_players$World == 1], 
	ID_O =  doubles_players$ID_P[doubles_players$World == 0]
		),
	expand.grid(
	ID_P = doubles_players$ID_P[doubles_players$World == 0], 
	ID_O =  doubles_players$ID_P[doubles_players$World == 1]
		)			
	)

### Use same tournament priors for doubles
atp_doubles_serve_priors <- doubles_matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))
	

atp_doubles_serve_priors <- atp_doubles_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)
	
# Merge with playerid
atp_serve_priors <- atp_serve_priors %>%
	left_join(players %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(players %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")
	
	
atp_doubles_serve_priors <- atp_doubles_serve_priors %>%
	left_join(doubles_players %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(doubles_players %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")
	
	
setwd("~/Software/inmatch_api_10/")

usethis::use_data(
	h2h,
	player_names, 
	atp_elo, 
	atp_elo_doubles,
	atp_serve_priors,
	atp_doubles_serve_priors,
	advantage_matches,
	regular_game_matrices,
	set_win_advantage,
	set_win_tiebreak,
	tiebreak_game_matrices,
	tiebreak_matches,
	tiebreak10,
	tiebreak10_matches,
	internal = TRUE, 
	overwrite = TRUE
	)
