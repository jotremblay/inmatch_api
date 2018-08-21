#' Get Baseline Serve and Win Probabilities
#'
#' Gets players current chances of winning point based on score at end
#'
#' @param playerid. 4-character player id
#' @param opponentid. 4-character opponent id
#' @param mens. Logical TRUE if mens match, FALSE if womens
#' @param format. Character ('bestof3', 'laver' or 'bestof5')
#'
#' @return data frame of player 1 and player 2 win probs
#'
#' @export
get_baseline <- function(playerid, opponentid, mens = TRUE, format = 'laver'){
	
	player1.elo <- elo_lookup(playerid, mens)
	
	player2.elo <- elo_lookup(opponentid, mens)			
	
	player1.win.prediction <- elo_prediction(player1.elo, player2.elo)
	player2.win.prediction <- elo_prediction(player2.elo, player1.elo)
	
	player1.serve.prob <- calibrate_serve(player1.win.prediction, playerid, opponentid, bestof3 = format %in% c("laver", "bestof3"))
	
	player2.serve.prob <- calibrate_serve(player2.win.prediction, opponentid, playerid, bestof3 = format %in% c("laver", "bestof3"))

	labels <- c(
		"playerid",
		"opponentid",
		"playerelo",
		"opponentelo",
		"playerwin",
		"opponentwin",
		"playerserve",
		"opponentserve"
	)
	
	values <- c(
		playerid,
		opponentid,
		player1.elo,
		player2.elo,
		player1.win.prediction,
		player2.win.prediction,
		player1.serve.prob,
		player2.serve.prob
	)
	
paste(paste(labels, values, sep = ':'), collapse = ",", sep = "")
}