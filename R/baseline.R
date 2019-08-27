#' Get Baseline Serve and Win Probabilities
#'
#' Gets players current chances of winning point based on score at end
#'
#' @param player1id. ATP/WTA player id
#' @param player2id. ATP/WTA player id (only used for doubles)
#' @param opponent1id. ATP/WTA opponent id
#' @param opponent2id. ATP/WTA opponent id (only used for doubles)
#' @param mens. Logical TRUE if mens match, FALSE if womens
#' @param format. Character ('bestof3', 'doubles', 'laver' or 'bestof5')
#'
#' @return data frame of player 1 and player 2 win probs
#'
#' @export
get_baseline <- function(player1id, player2id = NULL, opponent1id, opponent2id = NULL, mens = TRUE, format = 'laver'){
	
	player1.elo <- elo_lookup(player1id, player2id, mens)
	
	player2.elo <- elo_lookup(opponent1id, opponent2id, mens)		
	
	if(is.null(player2id)){
		adjust <- get_h2h(player1id, opponent1id)
	}	
	else{
		adjust <- 0
	}
	
	player1.win.prediction <- elo_prediction(player1.elo, player2.elo, adjust)
	
	player2.win.prediction <- elo_prediction(player2.elo, player1.elo, -1 * adjust)
	
	player_serve <- serve_prior_lookup(player1id, player2id, opponent1id, opponent2id, mens = mens)
	
	if(length(player_serve$player) == 0)
		player_serve$player <- ifelse(mens, 0.65, 0.57)
	
	if(length(player_serve$opponent) == 0)
		player_serve$opponent <- ifelse(mens, 0.65, 0.57)

	player1.serve.prob <- calibrate_serve(player1.win.prediction, player_serve$player, player_serve$opponent, bestof3 = format %in% c("laver", "bestof3", "doubles"))
	
	player2.serve.prob <- calibrate_serve(player2.win.prediction, player_serve$opponent, player_serve$player, bestof3 = format %in% c("laver", "bestof3", "doubles"))
	
	
	if(is.null(player2id)){
		player2id <- ""
		opponent2id <- ""
	}
	
	
		labels <- c(
			"player1id",
			"player2id",
			"opponent1id",
			"opponent2id",
			"playerelo",
			"opponentelo",
			"playerwin",
			"opponentwin",
			"playerserve",
			"opponentserve"
		)
		
		
		values <- c(
			player1id,
			player2id,
			opponent1id,
			opponent2id,
			player1.elo,
			player2.elo,
			player1.win.prediction,
			player2.win.prediction,
			player1.serve.prob,
			player2.serve.prob
		)
	
paste(paste(labels, values, sep = ':'), collapse = ",", sep = "")
}