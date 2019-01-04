#' Win Prob at End of Point
#'
#' Gets players current chances of winning point based on score at end
#'
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
#' @param player1.serve.prob Player expected probability of winning on serve from \code{get_baseline}
#' @param player2.serve.prob Player expected probability of winning on serve from \code{get_baseline}
#' @param player1.won Logical (TRUE or FALSE) whether player 1 won the current point
#' @param player1.serving Logical (TRUE or FALSE) whether player 1 served the current point
#' @param format. Character description of match format ('bestof3', 'laver', 'bestof5', 'doubles')
#'
#' @return data frame of player 1 and player 2 win probs
#'
#' @export
#'
get_win_prob <- function(
		player1.score,
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
			
			
			if(format == "doubles") format <- "bestof3"
			
			if(player1.serving)
				score <- update_score_tiebreak10(
					pointa = player1.score + as.numeric(player1.won), 
					pointb = player2.score + as.numeric((1 - player1.won)), 
					gamea = player1.games, 
					gameb = player2.games, 
					seta = player1.sets, 
					setb = player2.sets, 
					bestof3 = format == "bestof3"
				)
			else
				score <- update_score_tiebreak10(
					pointa = player2.score + as.numeric((1 - player1.won)), 
					pointb = player1.score + as.numeric(player1.won), 
					gamea = player2.games, 
					gameb = player1.games, 
					seta = player2.sets, 
					setb = player1.sets, 
					bestof3 = format == "bestof3"
				)
				
			
			if((player1.serving & !score$serve.changed) | (!player1.serving & score$serve.changed)){
				
				values <- dynamic_in_match_win(
						point_a = score$pointa,
						point_b = score$pointb,
						game_a = score$gamea,
						game_b = score$gameb,
						set_a = score$seta,
						set_b = score$setb,
						server.prob = player1.serve.prob,
						returner.prob =  player2.serve.prob,
						server.serve.points.won = player1.serve.won,
						server.serve.points = player1.serve.points,
						returner.serve.points.won = player2.serve.won,
						returner.serve.points = player2.serve.points,
						format = format
					)
				}							
			else{
				
				values <- 1 - dynamic_in_match_win(
						point_a = score$pointa,
						point_b = score$pointb,
						game_a = score$gamea,
						game_b = score$gameb,
						set_a = score$seta,
						set_b = score$setb,
						server.prob = player2.serve.prob,
						returner.prob =  player1.serve.prob,
						server.serve.points.won = player2.serve.won,
						server.serve.points = player2.serve.points,
						returner.serve.points.won = player1.serve.won,
						returner.serve.points = player1.serve.points,
						format = format
					)						
			}
			
				values <- formatC(c(values, 1 - values) * 100, dig = 1, format = "f")
				names(values) <- c("player1", "player2")
						
paste(paste(names(values), values, sep = ':'), collapse = ",", sep = "")
}
