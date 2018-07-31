#' Win Prob at End of Point
#'
#' Gets players current chances of winning point based on score at end
#'
#' @param data Data frame from \code{point_data}
#' @param mens Logical if men's match
#'
#' @return data frame of player 1 and player 2 win probs
#'
#' @export
#'
get_win_prob <- function(
	playerid, 
	opponentid,
	mens = TRUE){
	
	player1.elo <- elo_lookup(playerid, mens)
	player2.elo <- elo_lookup(opponentid, mens)			

	
	player1.win.prediction <- elo_prediction(player1.elo, player2.elo)
	player2.win.prediction <- elo_prediction(player2.elo, player1.elo)
	
	player1.serve.prob <- calibrate_serve(player1.win.prediction, playerid, opponentid, atp = mens)
	player2.serve.prob <- calibrate_serve(player2.win.prediction, opponentid, playerid, atp = mens)

	winprob <- function(data, player1.serve.prob, player2.serve.prob, mens){
		
		player1.serve.stats <- data %>%
			filter(server == player1) %>%
			dplyr::summarise(
				player = player1[1],
				serve = sum(player1.won & player1 == server),
				n = n()
			)
	
		player2.serve.stats <- data %>%
			filter(server == player2) %>%
			dplyr::summarise(
				player = player2[1],
				serve = sum(player2.won & player2 == server),
				n = n()
			)
			
		if(player1.serve.stats$n == 0){
			player1.serve.stats$player <- data$player1[1]
		}
		
		if(player2.serve.stats$n == 0){
			player2.serve.stats$player <- data$player2[1]
		}	
			
			current <- data[nrow(data),]
			
			player1.serving <- current$player1 == current$server
			
			if(player1.serving){
				score <- with(current, 
					update_score(player1.score + as.numeric(player1.won), player2.score + as.numeric(!player1.won), player1.games, player2.games, player1.sets, player2.sets, bestof = !mens)
					)
			}
			else{
				score <- with(current, 
					update_score(player2.score + as.numeric(player2.won), player1.score + as.numeric(!player2.won), player2.games, player1.games, player2.sets, player1.sets,  bestof = !mens)
					)
			}
		
			# Fix for 3-set matches
			if(!mens){
				if((score$seta + score$setb) > 2 & current$set > 3){
					score$seta <- 0
					score$setb <- 0
				}
				
				if((score$seta + score$setb) > 2 & current$set <= 3){
					score$seta <- 1
					score$setb <- 1
				}
			}
			else{
				if((score$seta + score$setb) > 4 & current$set > 4){
					score$seta <- 0
					score$setb <- 0
				}
				
				if((score$seta + score$setb) > 4 & current$set <= 4){
					score$seta <- 2
					score$setb <- 2
				}				
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
						server.serve.points.won = player1.serve.stats$serve,
						server.serve.points = player1.serve.stats$n,
						returner.serve.points.won = player2.serve.stats$serve,
						returner.serve.points = player2.serve.stats$n,
						best = !mens
					)
					
				player1.win$server_win[player1.win$server_win < 0.01] <- 0.01
				player1.win$server_win[player1.win$server_win > 0.99] <- 0.99
											
				winprob <- data.frame(
					player = c(player1.serve.stats$player, player2.serve.stats$player),
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
						best = !mens
					)				

					
				player2.win$server_win[player2.win$server_win < 0.01] <- 0.01
				player2.win$server_win[player2.win$server_win > 0.99] <- 0.99
				
				winprob <- data.frame(
					player = c(player1.serve.stats$player, player2.serve.stats$player),
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
				
# # 				winprob <- winprob %>%
					# dplyr::mutate_at(
						# dplyr::vars(win.prob:importance), formatting
					# )
				
	winprob		
	}
	
	current <- winprob(data, player1.serve.prob, player2.serve.prob, mens)
	
	if(nrow(data) > 1){
		last <- winprob(data[1:(nrow(data) - 1),], player1.serve.prob, player2.serve.prob, mens)[,c("player", "win.prob")]
		names(last)[2] <- "last.win.prob"
	}
	else{
		last <- current[,"player", drop = FALSE]
		last$last.win.prob <- round(c(player1.win.prediction, player2.win.prediction) * 100, 1)
	}
	
	rnames <- current$player
	
	rownames(current) <- rnames
	rownames(last) <- rnames
	
	current <- merge(current, last, by = "player")	
	current <- current[rnames,]
	
	current$baseline.win.prob <- round(c(player1.win.prediction, player2.win.prediction) * 100, 1)

current
}
