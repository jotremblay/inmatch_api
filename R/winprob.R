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
#' @param format. Character description of match format ('bestof3', 'laver', 'bestof5')
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
	
	
			if(player1.serving){
				if(format == "laver")
				score <- update_score_laver(player1.score + as.numeric(player1.won), player2.score + as.numeric(!player1.won), player1.games, player2.games, player1.sets, player2.sets)
				score <- update_score(player1.score + as.numeric(player1.won), player2.score + as.numeric(!player1.won), player1.games, player2.games, player1.sets, player2.sets, bestof = format == "bestof3")	
					
			}
			else{
				if(format == "laver")
				score <- update_score_laver(player2.score + as.numeric(player2.won), player1.score + as.numeric(!player2.won), player2.games, player1.games, player2.sets, player1.sets)
				else
				score <- update_score(player2.score + as.numeric(player2.won), player1.score + as.numeric(!player2.won), player2.games, player1.games, player2.sets, player1.sets,  bestof = format == "bestof3")
				
			}
		
			# Adjust erroneous set scores
			if(format != "bestof5" & (score$seta + score$setb) > 2){
					score$seta <- 1
					score$setb <- 1
				}
				
			if(format == "bestof5" & (score$seta + score$setb) > 4){
					score$seta <- 2
					score$setb <- 2
				}			
				
			
			if(score$serve.changed)
				player1.serving <- !player1.serving
			
			if(player1.serving){
					player1.win <- dynamic_in_match_win(
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
					
				player1.win$server_win[player1.win$server_win < 0.01] <- 0.01
				player1.win$server_win[player1.win$server_win > 0.99] <- 0.99
											
				winprob <- data.frame(
					win.prob = c(player1.win$server_win, 1 - player1.win$server_win),
					win.prob.point.won = c(player1.win$server_win_if_point_won, 1 - player1.win$server_win_if_point_lost),
					win.prob.point.lost = c(player1.win$server_win_if_point_lost, 1 - player1.win$server_win_if_point_won)
				)
				
				winprob$importance <- winprob$win.prob.point.won - winprob$win.prob.point.lost	
				
			}
			else{
					player2.win <- dynamic_in_match_win(
						point_a = score$pointa,
						point_b = score$pointb,
						game_a = score$gamea,
						game_b = score$gameb,
						set_a = score$seta,
						set_b = score$setb,
						server.prob = player2.serve.prob,
						returner.prob =  player1.serve.prob,
						server.serve.points.won = player2.serve.stats$serve,
						server.serve.points = player2.serve.stats$n,
						returner.serve.points.won = player1.serve.stats$serve,
						returner.serve.points = player1.serve.stats$n,
						format = format
					)				

					
				player2.win$server_win[player2.win$server_win < 0.01] <- 0.01
				player2.win$server_win[player2.win$server_win > 0.99] <- 0.99
				
				winprob <- data.frame(
					win.prob = c(1 - player2.win$server_win, player2.win$server_win),
					win.prob.point.won = c(1 - player2.win$server_win_if_point_lost,  player2.win$server_win_if_point_won),
					win.prob.point.lost = c(1 - player2.win$server_win_if_point_won,  player2.win$server_win_if_point_lost)
				)		
			
				winprob$importance <- winprob$win.prob.point.won - winprob$win.prob.point.lost
			}
			
				
				if(winprob[1,"win.prob.point.lost"] > winprob[1,"win.prob"] | winprob[1,"win.prob.point.won"] < winprob[1,"win.prob"]){
					winprob[1,"win.prob.point.lost"] <- winprob[1,"win.prob"] - winprob$importance[1]/2
					
					winprob[1,"win.prob.point.won"] <- winprob[1,"win.prob"] + winprob$importance[1]/2
				}
				
				
				if(winprob[2,"win.prob.point.lost"] > winprob[2,"win.prob"] | winprob[2,"win.prob.point.won"] < winprob[2,"win.prob"]){
					winprob[2,"win.prob.point.lost"] <- winprob[2,"win.prob"] - winprob$importance[2]/2
					
					winprob[2,"win.prob.point.won"] <- winprob[2,"win.prob"] + winprob$importance[2]/2
				}
						
				formatting <- function(x) round(x * 100, 1)
				
				winprob$win.prob <- formatting(winprob$win.prob)
				winprob$win.prob.point.won <- formatting(winprob$win.prob.point.won)
				winprob$win.prob.point.lost <- formatting(winprob$win.prob.point.lost)
				winprob$importance <- formatting(winprob$importance)
				
				player1 <- winprob[1,]
				player2 <- winprob[2,]
				
				values <- c(as.numeric(winprob[1,]), as.numeric(winprob[2,]))
				
				names(values) <- paste(rep(c("player1","player2"), each = 4), names(winprob), sep = ".")
				
					
paste(paste(names(values), values, sep = ':'), collapse = ",", sep = "")
}
