#' Dynamic Match Win Prediction
#'
#' This function calculates the match win prediction for matches in progress based on the current score and the in-match server performance
#'
#' @param point_a Numeric game points won by current server at start of point
#' @param point_b Numeric game points won by current returner at start of point
#' @param game_a Numeric games won by current server in the current set
#' @param game_b Numeric games won by current returner in the current set
#' @param set_a Numeric sets won by current server
#' @param set_b Numeric sets won by current returner
#' @param server.prob Numeric serve win prob of current server
#' @param return.prob Numeric serve win prob of current returner
#' @param server.serve.points.won Numeric total service points won in match up to current point
#' @param server.serve.points Numeric total service points played in match up to current point
#' @param returner.serve.points.won Numeric total service points won in match up to current point
#' @param returner.serve.points  Numeric total service points played in match up to current point
#' @param format Character and one of ('bestof3', 'laver', 'bestof5', 'doubles')
#'
#' @export
dynamic_in_match_win <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points, format) {
	
	 server.prob <- serve_update(server.serve.points.won, server.serve.points, server.prob)
	 return.prob <- serve_update(returner.serve.points.won, returner.serve.points, returner.prob)
	
		# Assume points checked and switched for serving player

	 match_win(
			point_a,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			server.prob,
			returner.prob,
			bestof3 = format == "bestof3"
			)
	
}