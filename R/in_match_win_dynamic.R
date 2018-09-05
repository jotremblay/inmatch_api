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
#' @param format Character and one of ('bestof3', 'laver', or 'bestof5')
#'
#' @export
dynamic_in_match_win <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points, format) {
	
		assign.weight <- function(points){
			 n0 <- 900
			 n0 / (n0 + points^2)
			}
	
	
		  assign.weight <- function(points){
			 n0 <- 2700
			 n0 / (n0 + points^2)
			}	
	

	dynamic_in_match_win3 <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points) 
	{

	
		iid_player_probs_lookup <- function(pa, pb, advantage){
			
				pa <- max(c(pa, 0.5))
				pb <- max(c(pb, 0.5))
			
				pa <- min(c(pa, 0.99))
				pb <- min(c(pb, 0.99))
					
				p1 <- as.character(round(pa, 2))
				p2 <- as.character(round(pb, 2))
				
				id1 <- paste(p1, p2, sep = ":")
				id2 <- paste(p2, p1, sep = ":")
					
				gameA_mat <- regular_game_matrices[[p1]]
				gameB_mat <- regular_game_matrices[[p2]]
				
				tbgameA_mat <- tiebreak_game_matrices[[id1]]
				tbgameB_mat <- tiebreak_game_matrices[[id2]]
				
				settbgameA_mat <- set_win_tiebreak[[id1]]
				settbgameB_mat <- set_win_tiebreak[[id2]]
				
				setadvgameA_mat <- set_win_advantage[[id1]]
				setadvgameB_mat <- set_win_advantage[[id2]]
				
				MA <- advantage_matches[[id1]]
				MB <- advantage_matches[[id2]]
				
			
			list(A = list(game = gameA_mat, tiebreak = tbgameA_mat, set_tiebreak = settbgameA_mat, set_advantage = setadvgameA_mat, match = MA), 
				B = list(game = gameB_mat, tiebreak = tbgameB_mat, set_tiebreak = settbgameB_mat, set_advantage = setadvgameB_mat, match = MB)
				)
			}
			
			
			match_win <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob) 
		{
			
			
		    matrices <- iid_player_probs_lookup(server.prob, returner.prob, adv = TRUE)
		    
		    serving_player <- 1
		    returning_player <- 2
	
			is.regular.tiebreak <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 2	 
		    invalid.score <- FALSE
		    playera.won <- FALSE
		    playerb.won <- FALSE
		    
		    # Boundary cases for points
		    if(!(is.regular.tiebreak)){
		    	if(point_a >= 3 & point_b >= 3){
		    		if(point_a == point_b){
		    			point_a <- 3
		    			point_b <- 3
		    		}	
			    		else if(point_a > point_b){
		    			point_a <- 3
		    			point_b <- 2
		    		}
		    		else{
		    			point_a <- 2
		    			point_b <- 3
		    		}
		    	}    	
		    if((point_a >= 4 & point_b < 3) | (point_b >= 4 & point_a < 3))
		    		invalid.score <- TRUE
		    } 
		    else{
		     	if(point_a >= 6 & point_b >= 6){
		    		if(point_a == point_b){
		    			point_a <- 6
		    			point_b <- 6
		    		}	
			    	else if(point_a > point_b){
		    			point_a <- 6
		    			point_b <- 5
		    		}
		    		else{
		    			point_a <- 5
		    			point_b <- 6
		    		}
		    	}    	
		    	if((point_a >= 7 & point_b < 6) | (point_b >= 7 & point_a < 6))
		    		invalid.score <- TRUE   	
		    }
		    	
		    
		    # Boundary cases for game
		    if(game_a >= 6 & game_b >= 6){
		    		if(game_a == game_b){
		    			game_a <- 6
		    			game_b <- 6
		    		}
		    		else if(game_a > game_b){
		    			game_a <- 6
		    			game_b <- 5
		    		}
		    		else{
		     			game_a <- 5
		    			game_b <- 6   			
		    		}
		    	}	
		    
		    if((game_a >= 6 & game_b <= 4) | (game_b >= 6 & game_a <= 4))
		    		invalid.score <- TRUE
	
		    
		    # Boundary for sets
		    if(set_a >= 2 & set_b >= 2)
		    	 invalid.score <- TRUE
		    	
		    if(set_a == 2 & set_b <= 1)
		    		playera.won <- TRUE
			    	
			  if(set_b == 2 & set_a <= 1)
			    	playerb.won <- TRUE	    		
		    		
		    
		    if(playera.won)
		    		return(1)
		    
		    if(playerb.won)
		    		return(0)
		    
		    if(invalid.score)
		   	 	return(NA)
			
		
		win_loss <- function(win_game = TRUE, win_set = TRUE, point_a, 
		        point_b, game_a, game_b, set_a, set_b, is.regular.tiebreak, serving_player, returning_player, matrices) {
		        	
		        	# Winning the game 
	 			winning_game <- function(point_a, point_b, win_game, is.regular.tiebreak, matrices) {
		            	
		           if ((is.regular.tiebreak) & win_game) {
		            	# Determine who is serving
		           if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
		                  part1 <- matrices[[serving_player]]$tiebreak[(point_a + 
		                    1), (point_b + 1)]
		                else 
		                	part1 <- 1 - matrices[[returning_player]]$tiebreak[(point_b + 
		                  1), (point_a + 1)]
		              }
		            else if (!(is.regular.tiebreak) & win_game) 
		                part1 <- matrices[[serving_player]]$game[(point_a + 
		                  1), (point_b + 1)]
		            else if ((is.regular.tiebreak) & !win_game) {
		            	# Determine who is serving
		                if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
		                  part1 <- 1 - matrices[[serving_player]]$tiebreak[(point_a + 
		                    1), (point_b + 1)]
		              	else   	
		              	part1 <- matrices[[returning_player]]$tiebreak[(point_b + 
		                  1), (point_a + 1)]
		            }
			        else part1 <- 1 - matrices[[serving_player]]$game[(point_a + 
		                1), (point_b + 1)]
		            part1
		        }
			     
			winning_set <- function(game_a, game_b, win_set, is.regular.tiebreak, matrices, returning_player) {
		            if ((is.regular.tiebreak)) {
		                part2 <- as.numeric(win_set)
		            }
		            else if (win_set) {
		               part2 <- 1 - matrices[[returning_player]]$set_tiebreak[(game_b + 
		                    1), (game_a + 1)]
		            }
		            else {
		               part2 <- matrices[[returning_player]]$set_tiebreak[(game_b + 
		                    1), (game_a + 1)]
		            }
		            part2
		        }
	
		 		winning_match <- function(set_a, set_b, serving_player) {
		            if (set_a >= 2 & set_b <= 1) 
		                part3 <- 1
		            else if(set_b >= 2 & set_a <= 1)
		            	part3 <- 0
		            else
		            	part3 <- matrices[[serving_player]]$match[(set_a + 
		                1), (set_b + 1)]
		        part3
		        }
		        
		        part1 <- winning_game(point_a = point_a, point_b = point_b, 
		            win_game = win_game, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices)
		        
		        # Check is won game implies won set
		        must.win.set <- win_game & ((game_a == 6 & game_b <= 5) | (game_a == 6 & game_b == 6))
	
		        must.lose.set <- !win_game & ((game_b == 6 & game_a <= 5) | (game_a == 6 & game_b == 6))
		        
		        must.win.match <- win_set & (set_a >= 1 & set_b <= 1) 
		        must.lose.match <- !win_set & (set_b >= 1 & set_a <= 1) 
		        
		        # Handle inconsistencies            
		        if (must.win.set & win_set) {
		            part2 <- 1
		            if(must.win.match)
		            	part3 <- 1
		            else
		            	part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		        }
		        else if (must.win.set & !win_set) {
		            part2 <- 0
		            part3 <- 0
		        }
		        else if (must.lose.set & win_set) {
		            part2 <- 0
		            part3 <- 0
		        }
		        else if(must.lose.set & !win_set){
		        		part2 <- 1
		            if(must.lose.match)
		            	part3 <- 0
		            else
		            	part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        	}
		        else if (win_game) {
		        		# But doesn't change set
		            part2 <- winning_set(game_a = game_a + 1, game_b = game_b, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices, returning_player = returning_player)
		            
		        		if (win_set) 
		                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		                
		            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        }
		        else {
		            part2 <- winning_set(game_a = game_a, game_b = game_b + 
		                1, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices, returning_player = returning_player)
	
		            if (win_set) 
		                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		                
		            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        }
		        part1 * part2 * part3
		    }
	    
	   	 if (is.regular.tiebreak) {
		        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type3 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type1 + type3
		    }
		    else {
		        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type3 <- win_loss(TRUE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type2 <- win_loss(FALSE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type4 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		            
		        type1 + type2 + type3 + type4
		   }
		}
	
	
	
			W <- assign.weight(server.serve.points)
			
			if(server.serve.points != 0){
				W1 <- assign.weight(server.serve.points + 1)
				server.prob1 <- W1 * server.prob + (1 - W1) * (server.serve.points.won + 1) /(server.serve.points + 1)
	
				W0 <- assign.weight(server.serve.points + 1)
				server.prob0 <- W0 * server.prob + (1 - W0) * (server.serve.points.won + 0) /(server.serve.points + 1)
	
				server.prob <- W * server.prob + (1 - W) * server.serve.points.won/server.serve.points
			}
			else{
				server.prob1 <- server.prob0 <- server.prob	
			}
		
			W <- assign.weight(returner.serve.points)
				
			if(returner.serve.points != 0)
				returner.prob <- W * returner.prob + (1 - W) * returner.serve.points.won/returner.serve.points
	
			#print(c(server.prob, returner.prob))
			
		win <- match_win(
			point_a,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			server.prob,
			returner.prob
			)
	
		score_when_point_lost <- update_score(
			point_a,
			point_b + 1,
			game_a,
			game_b,
			set_a,
			set_b,
			bestof = T
			)
	
		score_when_point_won <- update_score(
			point_a + 1,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			bestof = T
			)
		
		if(score_when_point_lost$serve.changed)
			loss_score <- 1 - match_win(
				score_when_point_lost$pointb,
				score_when_point_lost$pointa,
				score_when_point_lost$gameb,
				score_when_point_lost$gamea,
				score_when_point_lost$setb,
				score_when_point_lost$seta,
				returner.prob,
				server.prob0
				)
	
		else		
			loss_score <- match_win(
				score_when_point_lost$pointa,
				score_when_point_lost$pointb,
				score_when_point_lost$gamea,
				score_when_point_lost$gameb,
				score_when_point_lost$seta,
				score_when_point_lost$setb,
				server.prob0,
				returner.prob
				)
	
		if(score_when_point_won$serve.changed)
			win_score <- 1 - match_win(
				score_when_point_won$pointb,
				score_when_point_won$pointa,
				score_when_point_won$gameb,
				score_when_point_won$gamea,
				score_when_point_won$setb,
				score_when_point_won$seta,
				returner.prob,
				server.prob1
				)
	
		else		
			win_score <- match_win(
				score_when_point_won$pointa,
				score_when_point_won$pointb,
				score_when_point_won$gamea,
				score_when_point_won$gameb,
				score_when_point_won$seta,
				score_when_point_won$setb,
				server.prob1,
				returner.prob
				)
	
		if(is.na(win) | is.na(win_score) | is.na(loss_score))
			data.frame(
				server_win = 0.5, 
				server_win_if_point_won = 0.5, 
				server_win_if_point_lost = 0.5,
				serve_updating = server.prob
				)	
		else
			data.frame(
				server_win = win, 
				server_win_if_point_won = win_score, 
				server_win_if_point_lost = loss_score,
				serve_updating = server.prob
				)
	}
	
	
	dynamic_in_match_win5 <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points) 
	{
		
			iid_player_probs_lookup <- function(pa, pb, advantage){
			
				pa <- max(c(pa, 0.5))
				pb <- max(c(pb, 0.5))
			
				pa <- min(c(pa, 0.99))
				pb <- min(c(pb, 0.99))
					
				p1 <- as.character(round(pa, 2))
				p2 <- as.character(round(pb, 2))
				
				id1 <- paste(p1, p2, sep = ":")
				id2 <- paste(p2, p1, sep = ":")
					
				gameA_mat <- regular_game_matrices[[p1]]
				gameB_mat <- regular_game_matrices[[p2]]
				
				tbgameA_mat <- tiebreak_game_matrices[[id1]]
				tbgameB_mat <- tiebreak_game_matrices[[id2]]
				
				settbgameA_mat <- set_win_tiebreak[[id1]]
				settbgameB_mat <- set_win_tiebreak[[id2]]
				
				setadvgameA_mat <- set_win_advantage[[id1]]
				setadvgameB_mat <- set_win_advantage[[id2]]
				
				MA <- advantage_matches[[id1]]
				MB <- advantage_matches[[id2]]
				
			
			list(A = list(game = gameA_mat, tiebreak = tbgameA_mat, set_tiebreak = settbgameA_mat, set_advantage = setadvgameA_mat, match = MA), 
				B = list(game = gameB_mat, tiebreak = tbgameB_mat, set_tiebreak = settbgameB_mat, set_advantage = setadvgameB_mat, match = MB)
				)
			}
			
			
			match_win <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob) 
		{
			
			
		    matrices <- iid_player_probs_lookup(server.prob, returner.prob, adv = TRUE)
		    
		    serving_player <- 1
		    returning_player <- 2
	
			is.regular.tiebreak <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 4	 
		    invalid.score <- FALSE
		    playera.won <- FALSE
		    playerb.won <- FALSE
		    
		    # Boundary cases for points
		    if(!(is.regular.tiebreak)){
		    	if(point_a >= 3 & point_b >= 3){
		    		if(point_a == point_b){
		    			point_a <- 3
		    			point_b <- 3
		    		}	
			    		else if(point_a > point_b){
		    			point_a <- 3
		    			point_b <- 2
		    		}
		    		else{
		    			point_a <- 2
		    			point_b <- 3
		    		}
		    	}    	
		    if((point_a >= 4 & point_b < 3) | (point_b >= 4 & point_a < 3))
		    		invalid.score <- TRUE
		    } 
		    else{
		     	if(point_a >= 6 & point_b >= 6){
		    		if(point_a == point_b){
		    			point_a <- 6
		    			point_b <- 6
		    		}	
			    	else if(point_a > point_b){
		    			point_a <- 6
		    			point_b <- 5
		    		}
		    		else{
		    			point_a <- 5
		    			point_b <- 6
		    		}
		    	}    	
		    	if((point_a >= 7 & point_b < 6) | (point_b >= 7 & point_a < 6))
		    		invalid.score <- TRUE   	
		    }
		    	
		    
		    # Boundary cases for game
		    if(game_a >= 6 & game_b >= 6){
		    		if(game_a == game_b){
		    			game_a <- 6
		    			game_b <- 6
		    		}
		    		else if(game_a > game_b){
		    			game_a <- 6
		    			game_b <- 5
		    		}
		    		else{
		     			game_a <- 5
		    			game_b <- 6   			
		    		}
		    	}	
		    
		    if((game_a >= 6 & game_b <= 4) | (game_b >= 6 & game_a <= 4))
		    		invalid.score <- TRUE
	
		    
		    # Boundary for sets
		    if(set_a >= 3 & set_b >= 3)
		    	 invalid.score <- TRUE
		    	
		    if(set_a == 3 & set_b <= 2)
		    		playera.won <- TRUE
			    	
			  if(set_b == 3 & set_a <= 2)
			    	playerb.won <- TRUE	    		
		    		
		    
		    if(playera.won)
		    		return(1)
		    
		    if(playerb.won)
		    		return(0)
		    
		    if(invalid.score)
		   	 	return(NA)
			
		
		win_loss <- function(win_game = TRUE, win_set = TRUE, point_a, 
		        point_b, game_a, game_b, set_a, set_b, is.regular.tiebreak, serving_player, returning_player, matrices) {
		        	
		        	# Winning the game 
	 			winning_game <- function(point_a, point_b, win_game, is.regular.tiebreak, matrices) {
		            	
		           if ((is.regular.tiebreak) & win_game) {
		            	# Determine who is serving
		           if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
		                  part1 <- matrices[[serving_player]]$tiebreak[(point_a + 
		                    1), (point_b + 1)]
		                else 
		                	part1 <- 1 - matrices[[returning_player]]$tiebreak[(point_b + 
		                  1), (point_a + 1)]
		              }
		            else if (!(is.regular.tiebreak) & win_game) 
		                part1 <- matrices[[serving_player]]$game[(point_a + 
		                  1), (point_b + 1)]
		            else if ((is.regular.tiebreak) & !win_game) {
		            	# Determine who is serving
		                if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
		                  part1 <- 1 - matrices[[serving_player]]$tiebreak[(point_a + 
		                    1), (point_b + 1)]
		              	else   	
		              	part1 <- matrices[[returning_player]]$tiebreak[(point_b + 
		                  1), (point_a + 1)]
		            }
			        else part1 <- 1 - matrices[[serving_player]]$game[(point_a + 
		                1), (point_b + 1)]
		            part1
		        }
			     
			winning_set <- function(game_a, game_b, win_set, is.regular.tiebreak, matrices, returning_player) {
		            if ((is.regular.tiebreak)) {
		                part2 <- as.numeric(win_set)
		            }
		            else if (win_set) {
		               part2 <- 1 - matrices[[returning_player]]$set_tiebreak[(game_b + 
		                    1), (game_a + 1)]
		            }
		            else {
		               part2 <- matrices[[returning_player]]$set_tiebreak[(game_b + 
		                    1), (game_a + 1)]
		            }
		            part2
		        }
	
		 		winning_match <- function(set_a, set_b, serving_player) {
		            if (set_a >= 3 & set_b <= 2) 
		                part3 <- 1
		            else if(set_b >= 3 & set_a <= 2)
		            	part3 <- 0
		            else
		            	part3 <- matrices[[serving_player]]$match[(set_a + 
		                1), (set_b + 1)]
		        part3
		        }
		        
		        part1 <- winning_game(point_a = point_a, point_b = point_b, 
		            win_game = win_game, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices)
		        
		        # Check is won game implies won set
		        must.win.set <- win_game & ((game_a == 6 & game_b <= 5) | (game_a == 6 & game_b == 6))
	
		        must.lose.set <- !win_game & ((game_b == 6 & game_a <= 5) | (game_a == 6 & game_b == 6))
		        
		        must.win.match <- win_set & (set_a >= 2 & set_b <= 2) 
		        must.lose.match <- !win_set & (set_b >= 2 & set_a <= 2) 
		        
		        # Handle inconsistencies            
		        if (must.win.set & win_set) {
		            part2 <- 1
		            if(must.win.match)
		            	part3 <- 1
		            else
		            	part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		        }
		        else if (must.win.set & !win_set) {
		            part2 <- 0
		            part3 <- 0
		        }
		        else if (must.lose.set & win_set) {
		            part2 <- 0
		            part3 <- 0
		        }
		        else if(must.lose.set & !win_set){
		        		part2 <- 1
		            if(must.lose.match)
		            	part3 <- 0
		            else
		            	part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        	}
		        else if (win_game) {
		        		# But doesn't change set
		            part2 <- winning_set(game_a = game_a + 1, game_b = game_b, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices, returning_player = returning_player)
		            
		        		if (win_set) 
		                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		                
		            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        }
		        else {
		            part2 <- winning_set(game_a = game_a, game_b = game_b + 
		                1, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, matrices = matrices, returning_player = returning_player)
	
		            if (win_set) 
		                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
		                
		            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
		        }
		        part1 * part2 * part3
		    }
	    
	   	 if (is.regular.tiebreak) {
		        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type3 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type1 + type3
		    }
		    else {
		        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type3 <- win_loss(TRUE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type2 <- win_loss(FALSE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		        type4 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak)
		            
		        type1 + type2 + type3 + type4
		   }
		}
		
			W <- assign.weight(server.serve.points)
			
			if(server.serve.points != 0){
				W1 <- assign.weight(server.serve.points + 1)
				server.prob1 <- W1 * server.prob + (1 - W1) * (server.serve.points.won + 1) /(server.serve.points + 1)
	
				W0 <- assign.weight(server.serve.points + 1)
				server.prob0 <- W0 * server.prob + (1 - W0) * (server.serve.points.won + 0) /(server.serve.points + 1)
	
				server.prob <- W * server.prob + (1 - W) * server.serve.points.won/server.serve.points
			}
			else{
				server.prob1 <- server.prob0 <- server.prob	
			}
		
			W <- assign.weight(returner.serve.points)
				
			if(returner.serve.points != 0)
				returner.prob <- W * returner.prob + (1 - W) * returner.serve.points.won/returner.serve.points
	
		
			
			#print(c(server.prob, returner.prob))
			
		win <- match_win(
			point_a,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			server.prob,
			returner.prob
			)
	
		score_when_point_lost <- update_score(
			point_a,
			point_b + 1,
			game_a,
			game_b,
			set_a,
			set_b,
			bestof = F
			)
	
		score_when_point_won <- update_score(
			point_a + 1,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			bestof = F
			)
		
		if(score_when_point_lost$serve.changed)
			loss_score <- 1 - match_win(
				score_when_point_lost$pointb,
				score_when_point_lost$pointa,
				score_when_point_lost$gameb,
				score_when_point_lost$gamea,
				score_when_point_lost$setb,
				score_when_point_lost$seta,
				returner.prob,
				server.prob0
				)
	
		else		
			loss_score <- match_win(
				score_when_point_lost$pointa,
				score_when_point_lost$pointb,
				score_when_point_lost$gamea,
				score_when_point_lost$gameb,
				score_when_point_lost$seta,
				score_when_point_lost$setb,
				server.prob0,
				returner.prob
				)
	
		if(score_when_point_won$serve.changed)
			win_score <- 1 - match_win(
				score_when_point_won$pointb,
				score_when_point_won$pointa,
				score_when_point_won$gameb,
				score_when_point_won$gamea,
				score_when_point_won$setb,
				score_when_point_won$seta,
				returner.prob,
				server.prob1
				)
	
		else		
			win_score <- match_win(
				score_when_point_won$pointa,
				score_when_point_won$pointb,
				score_when_point_won$gamea,
				score_when_point_won$gameb,
				score_when_point_won$seta,
				score_when_point_won$setb,
				server.prob1,
				returner.prob
				)
	
		if(is.na(win) | is.na(win_score) | is.na(loss_score))
			data.frame(
				server_win = 0.5, 
				server_win_if_point_won = 0.5, 
				server_win_if_point_lost = 0.5,
				serve_updating = server.prob
				)	
		else
			data.frame(
				server_win = win, 
				server_win_if_point_won = win_score, 
				server_win_if_point_lost = loss_score,
				serve_updating = server.prob
				)
	}

	dynamic_in_match_laver <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points) 
	{

	
		iid_player_probs_lookup <- function(pa, pb){
	
		pa <- max(c(pa, 0.5))
		pb <- max(c(pb, 0.5))
	
		pa <- min(c(pa, 0.99))
		pb <- min(c(pb, 0.99))
			
		p1 <- as.character(round(pa, 2))
		p2 <- as.character(round(pb, 2))
		
		id1 <- paste(p1, p2, sep = ":")
		id2 <- paste(p2, p1, sep = ":")
			
		gameA_mat <- regular_game_matrices[[p1]]
		gameB_mat <- regular_game_matrices[[p2]]
		
		tbgameA_mat <- tiebreak_game_matrices[[id1]]
		tbgameB_mat <- tiebreak_game_matrices[[id2]]

		tb10gameA_mat <- tiebreak10[[id1]]
		tb10gameB_mat <- tiebreak10[[id2]]
		
		settbgameA_mat <- set_win_tiebreak[[id1]]
		settbgameB_mat <- set_win_tiebreak[[id2]]
		
		MA <- tiebreak10_matches[[id1]]
		MB <- tiebreak10_matches[[id2]]
		
	
	list(A = list(game = gameA_mat, tiebreak = tbgameA_mat, set_tiebreak = settbgameA_mat, tiebreak10 = tb10gameA_mat, match = MA), 
		B = list(game = gameB_mat, tiebreak = tbgameB_mat, set_tiebreak = settbgameB_mat, tiebreak10 = tb10gameB_mat, match = MB)
		)
	}
	
	
		match_win <- function (point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob) 
	{
		
		
	    matrices <- iid_player_probs_lookup(server.prob, returner.prob)
	    
	    serving_player <- 1
	    returning_player <- 2

		is.regular.tiebreak <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 2	 
		
		is.tiebreak10 <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 2   

	    invalid.score <- FALSE
	    playera.won <- FALSE
	    playerb.won <- FALSE
	    
	    # Boundary cases for points
	    if(!(is.regular.tiebreak | is.tiebreak10)){
	    	if(point_a >= 3 & point_b >= 3){
	    		if(point_a == point_b){
	    			point_a <- 3
	    			point_b <- 3
	    		}	
		    		else if(point_a > point_b){
	    			point_a <- 3
	    			point_b <- 2
	    		}
	    		else{
	    			point_a <- 2
	    			point_b <- 3
	    		}
	    	}    	
	    if((point_a >= 4 & point_b < 3) | (point_b >= 4 & point_a < 3))
	    		invalid.score <- TRUE
	    } 
	    else if(is.regular.tiebreak){
	     	if(point_a >= 6 & point_b >= 6){
	    		if(point_a == point_b){
	    			point_a <- 6
	    			point_b <- 6
	    		}	
		    	else if(point_a > point_b){
	    			point_a <- 6
	    			point_b <- 5
	    		}
	    		else{
	    			point_a <- 5
	    			point_b <- 6
	    		}
	    	}    	
	    	if((point_a >= 7 & point_b < 6) | (point_b >= 7 & point_a < 6))
	    		invalid.score <- TRUE   	
	    }
	    else{
	     	if(point_a >= 9 & point_b >= 9){
	    		if(point_a == point_b){
	    			point_a <- 9
	    			point_b <- 9
	    		}	
		    	else if(point_a > point_b){
	    			point_a <- 9
	    			point_b <- 8
	    		}
	    		else{
	    			point_a <- 8
	    			point_b <- 9
	    		}
	    	}    	
	    	if((point_a >= 10 & point_b < 9) | (point_b >= 10 & point_a < 9))
	    		invalid.score <- TRUE   	
	    	}	    	

	    
	    # Boundary cases for game
	    if(game_a >= 6 & game_b >= 6){
	    		if(game_a == game_b){
	    			game_a <- 6
	    			game_b <- 6
	    		}
	    		else if(game_a > game_b){
	    			game_a <- 6
	    			game_b <- 5
	    		}
	    		else{
	     			game_a <- 5
	    			game_b <- 6   			
	    		}
	    	}	
	    
	    if((game_a >= 6 & game_b <= 4) | (game_b >= 6 & game_a <= 4))
	    		invalid.score <- TRUE

	    
	    # Boundary for sets
	    if(set_a >= 2 & set_b >= 2)
	    	 invalid.score <- TRUE
	    	
	    if(set_a == 2 & set_b <= 1)
	    		playera.won <- TRUE
		    	
		  if(set_b == 2 & set_a <= 1)
		    		playerb.won <- TRUE	    		
	    		
	    
	    if(playera.won)
	    		return(1)
	    
	    if(playerb.won)
	    		return(0)
	    
	    if(invalid.score)
	   	 	return(NA)
	
	win_loss <- function(win_game = TRUE, win_set = TRUE, point_a, 
	        point_b, game_a, game_b, set_a, set_b, is.regular.tiebreak, is.tiebreak10, serving_player, returning_player, matrices) {
	        	
	        	# Winning the game 
 			winning_game <- function(point_a, point_b, win_game, is.regular.tiebreak, is.tiebreak10, matrices) {
	            	
	           if ((is.regular.tiebreak | is.tiebreak10) & win_game) {
	            	# Determine who is serving
	           if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
	                  part1 <- matrices[[serving_player]]$tiebreak[(point_a + 
	                    1), (point_b + 1)]
	                else if (is.tiebreak10 & point_a + point_b %% 4 %in% c(0, 3)) 
	                  part1 <- matrices[[serving_player]]$tiebreak10[(point_a + 
	                    1), (point_b + 1)]
	                else if (is.regular.tiebreak) 
	                	part1 <- 1 - matrices[[returning_player]]$tiebreak[(point_b + 
	                  1), (point_a + 1)]
	                else
	                	part1 <- 1 - matrices[[returning_player]]$tiebreak10[(point_b + 
	                  1), (point_a + 1)]
	            }
	            else if (!(is.regular.tiebreak | is.tiebreak10) & win_game) 
	                part1 <- matrices[[serving_player]]$game[(point_a + 
	                  1), (point_b + 1)]
	            else if ((is.regular.tiebreak | is.tiebreak10) & !win_game) {
	            	# Determine who is serving
	                if (is.regular.tiebreak & point_a + point_b %% 4 %in% c(0, 3)) 
	                  part1 <- 1 - matrices[[serving_player]]$tiebreak[(point_a + 
	                    1), (point_b + 1)]
	                else if (is.tiebreak10 & point_a + point_b %% 4 %in% c(0, 3)) 
	                  part1 <- 1 - matrices[[serving_player]]$tiebreak10[(point_a + 
	                    1), (point_b + 1)]
	                else if (is.regular.tiebreak) 
	                	part1 <- matrices[[returning_player]]$tiebreak[(point_b + 
	                  1), (point_a + 1)]
	                else
	                	part1 <-  matrices[[returning_player]]$tiebreak10[(point_b + 
	                  1), (point_a + 1)]
	            }
		        else part1 <- 1 - matrices[[serving_player]]$game[(point_a + 
	                1), (point_b + 1)]
	            part1
	        }
		     
		winning_set <- function(game_a, game_b, win_set, is.regular.tiebreak, is.tiebreak10, matrices, returning_player) {
	            if ((is.regular.tiebreak | is.tiebreak10)) {
	                part2 <- as.numeric(win_set)
	            }
	            else if (win_set) {
	               part2 <- 1 - matrices[[returning_player]]$set_tiebreak[(game_b + 
	                    1), (game_a + 1)]
	            }
	            else {
	               part2 <- matrices[[returning_player]]$set_tiebreak[(game_b + 
	                    1), (game_a + 1)]
	            }
	            part2
	        }

	 		winning_match <- function(set_a, set_b, serving_player) {
	            if (set_a >= 2 & set_b <= 1) 
	                part3 <- 1
	            else if(set_b >= 2 & set_a <= 1)
	            	part3 <- 0
	            else
	            	part3 <- matrices[[serving_player]]$match[(set_a + 
	                1), (set_b + 1)]
	        part3
	        }
	        
	        part1 <- winning_game(point_a = point_a, point_b = point_b, 
	            win_game = win_game, is.regular.tiebreak = is.regular.tiebreak, is.tiebreak10 = is.tiebreak10, matrices = matrices)
	        
	        # Check is won game implies won set
	        must.win.set <- win_game & ((game_a == 6 & game_b <= 5) | (game_a == 6 & game_b == 6))

	        must.lose.set <- !win_game & ((game_b == 6 & game_a <= 5) | (game_a == 6 & game_b == 6))
	        
	        must.win.match <- win_set & (set_a >= 1 & set_b <= 1) 
	        must.lose.match <- !win_set & (set_b >= 1 & set_a <= 1) 
	        
	        # Handle inconsistencies            
	        if (must.win.set & win_set) {
	            part2 <- 1
	            if(must.win.match)
	            	part3 <- 1
	            else
	            	part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
	        }
	        else if (must.win.set & !win_set) {
	            part2 <- 0
	            part3 <- 0
	        }
	        else if (must.lose.set & win_set) {
	            part2 <- 0
	            part3 <- 0
	        }
	        else if(must.lose.set & !win_set){
	        		part2 <- 1
	            if(must.lose.match)
	            	part3 <- 0
	            else
	            	part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
	        	}
	        else if (win_game) {
	        		# But doesn't change set
	            part2 <- winning_set(game_a = game_a + 1, game_b = game_b, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, is.tiebreak10 = is.tiebreak10, matrices = matrices, returning_player = returning_player)
	            
	        		if (win_set) 
	                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
	                
	            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
	        }
	        else {
	            part2 <- winning_set(game_a = game_a, game_b = game_b + 
	                1, win_set = win_set, is.regular.tiebreak = is.regular.tiebreak, is.tiebreak10 = is.tiebreak10, matrices = matrices, returning_player = returning_player)

	            if (win_set) 
	                part3 <- winning_match(set_a = set_a + 1, set_b = set_b, serving_player = serving_player)
	                
	            else part3 <- winning_match(set_a = set_a, set_b = set_b + 1, serving_player = serving_player)
	        }
	        part1 * part2 * part3
	    }
    
   	 if ((is.regular.tiebreak | is.tiebreak10)) {
	        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	        type3 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	        type1 + type3
	    }
	    else {
	        type1 <- win_loss(TRUE, TRUE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	        type3 <- win_loss(TRUE, FALSE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	        type2 <- win_loss(FALSE, TRUE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	        type4 <- win_loss(FALSE, FALSE, point_a, point_b, game_a, 
	            game_b, set_a, set_b, serving_player = serving_player, 
	            returning_player = returning_player, matrices = matrices, 
	            is.regular.tiebreak =  is.regular.tiebreak, is.tiebreak10 = is.tiebreak10)
	            
	        type1 + type2 + type3 + type4
	   }
	}
	
	
			W <- assign.weight(server.serve.points)
			
			if(server.serve.points != 0){
				W1 <- assign.weight(server.serve.points + 1)
				server.prob1 <- W1 * server.prob + (1 - W1) * (server.serve.points.won + 1) /(server.serve.points + 1)
	
				W0 <- assign.weight(server.serve.points + 1)
				server.prob0 <- W0 * server.prob + (1 - W0) * (server.serve.points.won + 0) /(server.serve.points + 1)
	
				server.prob <- W * server.prob + (1 - W) * server.serve.points.won/server.serve.points
			}
			else{
				server.prob1 <- server.prob0 <- server.prob	
			}
		
			W <- assign.weight(returner.serve.points)
				
			if(returner.serve.points != 0)
				returner.prob <- W * returner.prob + (1 - W) * returner.serve.points.won/returner.serve.points
	
			#print(c(server.prob, returner.prob))
			
		win <- match_win(
			point_a,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b,
			server.prob,
			returner.prob
			)
	
		score_when_point_lost <- update_score_laver(
			point_a,
			point_b + 1,
			game_a,
			game_b,
			set_a,
			set_b
			)
	
		score_when_point_won <- update_score_laver(
			point_a + 1,
			point_b,
			game_a,
			game_b,
			set_a,
			set_b
			)
		
		if(score_when_point_lost$serve.changed)
			loss_score <- 1 - match_win(
				score_when_point_lost$pointb,
				score_when_point_lost$pointa,
				score_when_point_lost$gameb,
				score_when_point_lost$gamea,
				score_when_point_lost$setb,
				score_when_point_lost$seta,
				returner.prob,
				server.prob0
				)
	
		else		
			loss_score <- match_win(
				score_when_point_lost$pointa,
				score_when_point_lost$pointb,
				score_when_point_lost$gamea,
				score_when_point_lost$gameb,
				score_when_point_lost$seta,
				score_when_point_lost$setb,
				server.prob0,
				returner.prob
				)
	
		if(score_when_point_won$serve.changed)
			win_score <- 1 - match_win(
				score_when_point_won$pointb,
				score_when_point_won$pointa,
				score_when_point_won$gameb,
				score_when_point_won$gamea,
				score_when_point_won$setb,
				score_when_point_won$seta,
				returner.prob,
				server.prob1
				)
	
		else		
			win_score <- match_win(
				score_when_point_won$pointa,
				score_when_point_won$pointb,
				score_when_point_won$gamea,
				score_when_point_won$gameb,
				score_when_point_won$seta,
				score_when_point_won$setb,
				server.prob1,
				returner.prob
				)
	
		if(is.na(win) | is.na(win_score) | is.na(loss_score))
			data.frame(
				server_win = 0.5, 
				server_win_if_point_won = 0.5, 
				server_win_if_point_lost = 0.5,
				serve_updating = server.prob
				)	
		else
			data.frame(
				server_win = win, 
				server_win_if_point_won = win_score, 
				server_win_if_point_lost = loss_score,
				serve_updating = server.prob
				)
	}

	if(format == "bestof3")
		dynamic_in_match_win3(point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points)
	else if(format == "laver")
		dynamic_in_match_laver(point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points)
	else
		dynamic_in_match_win5(point_a, point_b, game_a, game_b, set_a, set_b, server.prob, returner.prob, server.serve.points.won, server.serve.points, returner.serve.points.won, returner.serve.points)	
}