#' Conditional Game Probability
#'
#' Returns game win or loss chance given point score
#'
#' @param point_a Numeric game points won by current server at start of point
#' @param point_b Numeric game points won by current returner at start of point
#' @param win_game Logical if won game
#' @param is.regular.tiebreak Logical if tiebreak game
#' @param matrices List of lookup table for conditional probabilities
#' @param serving_player Numeric (1 or 2) for server matrices
#' @param returning_player Numeric (1 or 2) for server matrices
#'
#' @export
game_win_prob <- function(point_a, point_b, win_game, is.regular.tiebreak, matrices, serving_player = 1, returning_player = 2) {
           
           serving_tiebreak <- function(a, b)
	           	(a + b) %% 4  %in% c(0, 3)
            	# Cases where point score determines game outcome
            	
           	if(point_a >= 4 & (point_a - point_b) >= 2 & !is.regular.tiebreak)
           				part1 <- ifelse(win_game, 1, 0)
			else if(point_b >= 4 & (point_b - point_a) >= 2  & !is.regular.tiebreak)
					part1 <- ifelse(!win_game, 1, 0)
			else if (point_a >= 7 & (point_a - point_b) >= 2 & is.regular.tiebreak) {
	                part1 <- ifelse(win_game, 1, 0)  # Set is same as game for tiebreak
	         }
			else if (point_b >= 7 & (point_b - point_a) >= 2 & is.regular.tiebreak) {
	                 part1 <- ifelse(!win_game, 1, 0) # Set is same as game for tiebreak
	         }	         
           else if (is.regular.tiebreak & win_game){
           		if (is.regular.tiebreak & serving_tiebreak(point_a, point_b)) 
                part1 <- matrices[[serving_player]]$tiebreak[(point_a + 
                    1), (point_b + 1)]
                else 
                	part1 <- 1 - matrices[[returning_player]]$tiebreak[(point_b + 
                  1), (point_a + 1)]
              }
            else if (!is.regular.tiebreak & win_game) 
                part1 <- matrices[[serving_player]]$game[(point_a + 
                  1), (point_b + 1)]
            else if (is.regular.tiebreak & !win_game) {
            	# Determine who is serving
                if (is.regular.tiebreak & serving_tiebreak(point_a, point_b)) 
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
