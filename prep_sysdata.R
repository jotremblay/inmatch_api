# Prepare serve return for atp players
library(devtools)
library(oncourt)

rm(list = ls())

load(file = "~/Software/inmatch_api/R/sysdata.rda")

data(atp_players_links)

player_names <- data.frame(
	Name = c(
	"Roger Federer",
	"Grigor Dimitrov",
	"Alexander Zverev",
	"Novak Djokovic",
	"David Goffin",
	"Kyle Edmund",
	"Jack Sock",
	"Juan Martin Del Potro",
	"John Isner",
	"Nick Kyrgios",
	"Kevin Anderson",
	"Diego Sebastian Schwartzman"
),
	World = rep(c(0, 1), each = 6),
	stringsAsFactors = F
)


players <- atp_players_links %>%
	filter(NAME_P %in% player_names$Name)

players$World <- players$NAME_P %in% player_names$Name[player_names$World == 1]

matchup_grid <- expand.grid(ID_P = players$ID_P[players$World], 
	ID_O =  players$ID_P[!players$World])

# Paris as indoor hard
event <- 13868 

matchup_grid$Serve <- mapply(
	efron_morris,
	player = matchup_grid$ID_P,
	opponent = matchup_grid$ID_O,
	MoreArgs = list(event = event)
)

matchup_grid2 <- expand.grid(ID_P = players$ID_P[!players$World], 
	ID_O =  players$ID_P[players$World])


matchup_grid2$Serve <- mapply(
	efron_morris,
	player = matchup_grid2$ID_P,
	opponent = matchup_grid2$ID_O,
	MoreArgs = list(event = event)
)


matchup_grid <- rbind(matchup_grid, matchup_grid2)

serve_priors <- matchup_grid %>%
	inner_join(players %>% select(player = NAME_P, playerid = atpid, ID_P)) %>%
	inner_join(players %>% select(opponent = NAME_P, opponentid = atpid, ID_O = ID_P)) %>%
	select(-ID_P, -ID_O)
	
	
library(sofascoreR)

atp_elo <- current_elo(mens = T, surface = "Hard")	

atp_elo <- atp_elo %>% filter(Player %in% player_names$Name) %>%
	select(player = Player, elo = Hard, matches = player_match_num)

atp_elo <- atp_elo %>% 
	inner_join(players %>% select(player = NAME_P, playerid = atpid))
	

devtools::use_data(
	atp_elo,
	serve_priors,
	advantage_matches,
	atp_importance,
	atp_importance_laver,
	atp_importance_single,
	regular_game_matrices,
	set_win_advantage,
	set_win_tiebreak,
	tiebreak_game_matrices,
	tiebreak_matches,
	tiebreak10,
	tiebreak10_matches,
	wta_importance, 
	pkg = "~/Software/inmatch_api", 
	internal = TRUE, 
	overwrite = TRUE
	)

