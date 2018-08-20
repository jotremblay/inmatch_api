# plumber_win.R

#' @param player1.score Player points won at the start of current point
#' @param player2.score Player points won at the start of current point
#' @param player1.games Player number of games won in the current set
#' @param player2.games Player number of games won in the current set
#' @param player1.sets Player sets won in the current match
#' @param player2.sets Player sets won in the current match
#' @param player1.serve.won Player service points won in the current match
#' @param player1.serve.points Player service points played in the current match
#' @param player2.serve.won Player service points won in the current match
#' @param player2.serve.points Player service points played in the current match
#' @param player1.serve.prob Player expected probability of winning on serve 
#' @param player2.serve.prob Player expected probability of winning on serve 
#' @param player1.won Logical (TRUE or FALSE) whether player 1 won the current point
#' @param player1.serving Logical (TRUE or FALSE) whether player 1 served the current point
#' @param format Character description of match format ('bestof3', 'laver', 'bestof5')
#'
#' @get /win
function(player1.score,
		player2.score,
		player1.games,
		player2.games,
		player1.sets,
		player2.sets, 
		player1.serve.won, 
		player1.serve.points,
		player2.serve.won, 
		player2.serve.points,		
		player1.serve.prob, 
		player2.serve.prob, 
		player1.won,
		player1.serving = T,
		format){
			
		get_win_prob(
			as.integer(player1.score),
			as.integer(player2.score),
			as.integer(player1.games),
			as.integer(player2.games),
			as.integer(player1.sets),
			as.integer(player2.sets), 
			as.integer(player1.serve.won), 
			as.integer(player1.serve.points),
			as.integer(player2.serve.won), 
			as.integer(player2.serve.points),		
			as.numeric(player1.serve.prob), 
			as.numeric(player2.serve.prob), 
			as.logical(player1.won),
			as.logical(player1.serving),
			format)		
}

