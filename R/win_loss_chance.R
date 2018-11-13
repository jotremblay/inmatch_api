#' Conditional match win chance given game and set win
#' @export
win_loss_chance <- function(
				win_game = TRUE, 
				win_set = TRUE, 
				point_a, 
		        point_b, 
		        game_a, 
		        game_b, 
		        set_a, 
		        set_b, 
		        is.regular.tiebreak,
		        serving_player = 1, 
		        returning_player = 2, 
		        matrices, 
		        bestof3 = T) {
		 	
		 		# Change of winning game from current points
		 		game_win_chance <- game_win_prob(point_a, point_b, win_game, is.regular.tiebreak, matrices = matrices, serving_player = serving_player, returning_player = returning_player)
		 		
		 		# Check for set-deciding game		 	
		 		tiebreak_set <- ((bestof3 & (set_a + set_b) != 2) | (!bestof3 & (set_a + set_b) != 4))	 		
		 		set_deciding <- (tiebreak_set & game_a == 6 & game_b == 6) |
		 			(game_a >= 5 & (game_a - game_b) >= 1 & win_game) |
		 			(game_b >= 5 & (game_b - game_a) >= 1 & !win_game)
		 		
		 		# Set is equivalent to winning tiebreak game if it goes to tiebreak
		 		if(set_deciding)
		 			set_win_chance <- as.numeric(win_game == win_set) # Set determined by game outcome
		 		else{	
		 		
		 			game_a <- game_a + as.numeric(win_game)		
		 			game_b <- game_b + (1 - as.numeric(win_game))
		 			
		 			# Advantage scenario
		 			if((game_a + game_b) >= 12){ 
			 			game_score <- update_score(0, 0, game_a, game_b, set_a, set_b, bestof3)
		 				game_a <- game_score$gamea
		 				game_b <- game_score$gameb
		 			}		 		
		 			
		 			set_win_chance <- set_win_prob(game_a, game_b, win_set, matrices = matrices, serving_player = serving_player, returning_player = returning_player)		 			
		 		}
		       	
		       	# Match deciding set scenarios
		       	max_sets <- ifelse(bestof3, 2, 3)
		       	
		       	if(set_a >= (max_sets - 1) & win_set)
		       		match_win_chance <- 1
		       	else if(set_b >= (max_sets - 1) & !win_set)
		       		match_win_chance <- 0
		       	else{
		       			       		        
		       	 set_a <- set_a + as.numeric(win_set)
		        	 set_b <- set_b + (1 - as.numeric(win_set))
		 				        
		        match_win_chance <- match_win_prob(set_a, set_b, matrices = matrices, serving_player = serving_player, returning_player = returning_player)	
		        }
		        	        
		        
game_win_chance * set_win_chance * match_win_chance
}