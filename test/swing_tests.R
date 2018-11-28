library(ggplot2)
library(inmatch)
library(GIGEngineDB)

get_win_prob_df <- Vectorize(get_win_prob_df)

con <- make_connection()

score <- tbl(con, "tbl_GIG_Stats_Final_WinPrediction") %>%
	collect()

score <- score %>%
	filter(vchTournamentYear == 2018, vchMatchCode %in% "WS701")
	
params <- do.call("rbind", lapply(score$vchParameterUsed, function(x){
	x <- strsplit(x, ";")[[1]]
	variable <- sapply(strsplit(x, "="), function(z) z[1])
	value <- sapply(strsplit(x, "="), function(z) z[2])
	names(value) <- variable
value
}))

params <- as.data.frame(params, stringsAsFactors = F)

params <- params %>%
	dplyr::mutate(
		format = "bestof3",
		player1.serving = player1.serving == "TRUE",
		player1.won = player1.won == "TRUE"
	) %>%
	dplyr::mutate_at(
		vars(player1.serve.prob:player2.score),
		funs(as.numeric)
	)

score <- cbind(score, params)

score <- score %>% arrange(vchMatchCode, intCurrentSet, intCurrentGame, intCurrentPoint)


# Scored with evenly matched players
score <- score %>%
	dplyr::mutate(
		prediction = get_win_prob_df(
			format = format,
			player1.serving = player1.serving,
			player1.won = player1.won,			
			player1.serve.prob= player1.serve.prob, 
			player2.serve.prob= player2.serve.prob,
			player1.serve.points= player1.serve.points,
			player2.serve.points= player2.serve.points,
			player1.serve.won= player1.serve.won,
			player2.serve.won=  player2.serve.won,
			player1.sets= player1.sets,
			player2.sets= player2.sets,
			player1.games= player1.games,
			player2.games= player2.games,
			player1.score= player1.score,
			player2.score= player2.score
		)
	)
	
score$prediction[score$vchPlayerSecondName == "Wozniacki"] <- 1 - score$prediction[score$vchPlayerSecondName == "Wozniacki"]

score <- score %>%
	ungroup() %>%
	group_by(vchPlayerSecondName) %>%
	dplyr::mutate(
		swing = c(NA, diff(prediction))
	)
	
set1 <- score %>%
	filter(intCurrentSet == 1) %>% 
	select(intCurrentSet, intCurrentGame, intCurrentPoint, contains("dec"), vchPlayerSecondName, prediction, swing)
	
as.data.frame(set1 %>% filter(vchPlayerSecondName == "Halep") %>% arrange(-swing))
as.data.frame(set1 %>% filter(vchPlayerSecondName == "Wozniacki") %>% arrange(-swing))
		
		
get_win_prob <- Vectorize(get_win_prob)
			
	
x <- score %>%
	filter(intCurrentSet == 1, intCurrentGame == 13, vchPlayerSecondName == "Halep") %>%
	dplyr::mutate(
		prediction = get_win_prob(
			format = format,
			player1.serving = player1.serving,
			player1.won = player1.won,			
			player1.serve.prob= player1.serve.prob, 
			player2.serve.prob= player2.serve.prob,
			player1.serve.points= player1.serve.points,
			player2.serve.points= player2.serve.points,
			player1.serve.won= player1.serve.won,
			player2.serve.won=  player2.serve.won,			
			player1.sets= player1.sets,
			player2.sets= player2.sets,
			player1.games= player1.games,
			player2.games= player2.games,
			player1.score= player1.score,
			player2.score= player2.score
		)
	)	
	
# Player serving at 0-1 in tiebreak, 0.57 vs 0.59
# Chance of winning set 
0.33867823

# Chance of winning match
0.6740313 # Set up
0.1840913 # Set down

# Player receiving at 2-1 in tiebreak, 0.57 vs 0.59
# Chance of winning set 
0.32897872

# Chance of winning match (same)
0.6740313 # Set up
0.1840913 # Set down


	