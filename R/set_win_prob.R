#' Conditional Set Win Probability
#'
#'
#' @param game_a Numeric set points won by current server at start of point
#' @param game_b Numeric set points won by current returner at start of point
#' @param win_set Logical if won set
#' @param matrices List of lookup table for conditional probabilities
#' @param serving_player Numeric (1 or 2) for server matrices
#' @param returning_player Numeric (1 or 2) for server matrices
#'
#' @export
set_win_prob <- function(game_a, game_b, win_set, matrices, serving_player = 1, returning_player = 2) {
	            # Regular cases
	           if (win_set) { # Current server switches
	               part2 <- 1 - matrices[[returning_player]]$set_tiebreak[(game_b + 
	                    1), (game_a + 1)]
	            }
	            else {
	               part2 <- matrices[[returning_player]]$set_tiebreak[(game_b + 
	                    1), (game_a + 1)]
	            }
part2
}
