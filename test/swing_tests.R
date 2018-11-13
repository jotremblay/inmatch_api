library(ggplot2)
library(inmatch)
library(GIGEngineDB)

get_win_prob_df <- Vectorize(get_win_prob_df)

con <- make_connection()

score <- tbl(con, "tbl_GIG_SMTGameScore" ) %>%
	collect()

score <- score %>%
	filter(vchTournamentYear == 2018, vchMatchCode %in% c("MS701", "WS701", "MS601", "MS602", "WS601", "WS602"))
	
baseline <- score %>%
	select(vchMatchCode, vchPlayer1, vchPlayer2, vchPlayer1Code, vchPlayer2Code)	%>%
	unique()
	
	
baseline$format <- ifelse(grepl("WS", baseline$vchMatchCode), "bestof3", "bestof5")
	
baseline_stats <- do.call("rbind", mapply(
	get_baseline_df,
	player1id = baseline$vchPlayer1Code,
	opponent1id = baseline$vchPlayer2Code,
	mens = baseline$format == "bestof5",
	format  = baseline$format,
	SIMPLIFY = F
))

baseline <- cbind(baseline, baseline_stats)

score <- score %>%
	inner_join(baseline %>% select(-(vchPlayer1:vchPlayer2Code)), by = "vchMatchCode")

score <- score %>% arrange(vchMatchCode, intCurrentSet, intCurrentGame, intCurrentPoint)


score <- score %>%
	group_by(vchMatchCode, intCurrentSet) %>%
	dplyr::mutate(
		intPlayer2Sets = intPlayer2Sets[1],
		intPlayer1Sets = intPlayer1Sets[1]
		) %>%
	group_by(vchMatchCode, intCurrentSet, intCurrentGame) %>%
	dplyr::mutate(
		intPlayer2Games = intPlayer2Games[1],
		intPlayer1Games = intPlayer1Games[1]
		) %>%
	dplyr::mutate(
		player1.serving = vchServer == vchPlayer1,
		player1.won = (player1.serving & intServerPoint == 1) | (!player1.serving & intServerPoint == -1)
	)

# Scored with evenly matched players
score <- score %>%
	filter(!is.na(intServerPoint)) %>%
	dplyr::mutate(
		prediction = get_win_prob_df(
			format = format,
			player1.serving = player1.serving,
			player1.won = player1.won,			
			player1.serve.prob= 0.6, 
			player2.serve.prob= 0.6,
			player1.serve.points= 0,
			player2.serve.points= 0,
			player1.serve.won= 0,
			player2.serve.won= 0,
			player1.sets= intPlayer1Sets,
			player2.sets= intPlayer2Sets,
			player1.games= intPlayer1Games,
			player2.games= intPlayer2Games,
			player1.score= intPlayer1Score,
			player2.score= intPlayer2Score
		)
	)
	

score <- score %>%
	group_by(vchMatchCode) %>%
	dplyr::mutate(
		swing = c(NA, diff(prediction)),
		point = 1:n()
	)
	
score %>%
	ggplot(aes(y = prediction, x = point)) +
	facet_wrap(~vchMatchCode, scale = "free_x") +
	geom_point() + 
	geom_line() +
	scale_y_continuous(lim = c(0, 1))
	
	
# Check consistency in swings
any(score$swing > 0 & !score$player1.won, na.rm = T)
any(score$swing < 0 & score$player1.won, na.rm = T)

# Biggest swings
biggest_swings <- score %>%
	arrange(vchMatchCode, -abs(swing)) %>%
	dplyr::mutate(
		biggest = 1:n() %in% 1:5
	) %>%
	filter(biggest)