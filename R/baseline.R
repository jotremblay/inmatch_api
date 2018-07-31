#' Get Baseline Serve and Win Probabilities
#'
#' Gets players current chances of winning point based on score at end
#'
#' @param playerid. 4-character player id
#' @param opponentid. 4-character opponent id
#' @param mens. Logical TRUE if mens match, FALSE if womens
#' @param bestof3. Logical TRUE if bestof3 match, FALSE if bestof5
#'
#' @return data frame of player 1 and player 2 win probs
#'
#' @export
get_baseline <- function(playerid, opponentid, mens = TRUE, bestof3 = T){
	
	player1.elo <- elo_lookup(playerid, mens)
	
	player2.elo <- elo_lookup(opponentid, mens)			
	
	player1.win.prediction <- elo_prediction(player1.elo, player2.elo)
	player2.win.prediction <- elo_prediction(player2.elo, player1.elo)
	
	player1.serve.prob <- calibrate_serve(player1.win.prediction, playerid, opponentid, bestof3 = bestof3)
	
	player2.serve.prob <- calibrate_serve(player2.win.prediction, opponentid, playerid, bestof3 = bestof3)

	data.frame(
		playerid = playerid, 
		opponentid = opponentid,
		playerelo = player1.elo,
		opponentelo = player2.elo,
		playerwin = player1.win.prediction,
		opponentwin = player2.win.prediction,
		playerserve = player1.serve.prob,
		opponentserve = player2.serve.prob
	)
}