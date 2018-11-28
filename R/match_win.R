#' Conditional Match Win
#'
#'
#' @param point_a Numeric game points won by current server at start of point
#' @param point_b Numeric game points won by current returner at start of point
#' @param game_a Numeric games won by current server at start of point
#' @param game_b Numeric games won by current returner at start of point
#' @param set_a Numeric sets won by current server at start of point
#' @param set_b Numeric sets won by current returner at start of point
#' @param server.prob Numeric 
#' @param returner.prob Numeric 
#' @param bestof3 Logical if bestof3 format
#'
#' @export
match_win <- function (
				point_a, 
				point_b, 
				game_a, 
				game_b, 
				set_a, 
				set_b, 
				server.prob, 
				returner.prob,
				bestof3 = T) 
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
		
			
		    matrices <- iid_player_probs_lookup(server.prob, returner.prob)
		    
		    if(bestof3){
		    		matrices[[1]]$match <- matrices[[1]]$match[2:nrow(matrices[[1]]$match), 
		            2:nrow(matrices[[1]]$match)]
		            
		        matrices[[2]]$match <- matrices[[2]]$match[2:nrow(matrices[[2]]$match), 
		            2:nrow(matrices[[2]]$match)]
			}
		   
		    
		    serving_player <- 1		    	  		    
		    returning_player <- 2
	
			if(bestof3)
				is.regular.tiebreak <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 2
			else
				is.regular.tiebreak <- (game_a == 6 & game_b == 6) & (set_a + set_b) != 4
						
							 
		    playera.won <- FALSE
		    playerb.won <- FALSE
	    	
	    		if(bestof3){
			    if(set_a == 2 & set_b <= 1)
			    	playera.won <- TRUE
				    	
				if(set_b == 2 & set_a <= 1)
				    playerb.won <- TRUE	    		
			   }
			 else{
			    if(set_a == 3 & set_b <= 2)
			    	playera.won <- TRUE
				    	
				if(set_b == 3 & set_a <= 2)
				    playerb.won <- TRUE	    		
			   }			 	 	
			 	
		    
		    if(playera.won)
		    		return(1)
		    
		    if(playerb.won)
		    		return(0)
		    
		    
	   	 if (is.regular.tiebreak) {
		       wingame_winset <- win_loss_chance(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
		        losegame_loseset <- win_loss_chance(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
				
		        wingame_winset + losegame_loseset
		    }
		    else {
		        wingame_winset <- win_loss_chance(TRUE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
		        wingame_loseset <- win_loss_chance(TRUE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
		       losegame_winset <- win_loss_chance(FALSE, TRUE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
		        losegame_loseset <- win_loss_chance(FALSE, FALSE, point_a, point_b, game_a, 
		            game_b, set_a, set_b, serving_player = serving_player, 
		            returning_player = returning_player, matrices = matrices, 
		            is.regular.tiebreak =  is.regular.tiebreak, bestof3 = bestof3)
		     
		        wingame_winset + wingame_loseset + losegame_winset + losegame_loseset
		   }
}
	
	