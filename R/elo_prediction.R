#' Elo-Based Win Prediction
#'
#' This function returns the prediction for a match based on player Elo ratings
#'
#' @param rating1 Numeric of Player 1 Elo rating
#' @param rating2 Numeric of Player 2 Elo rating
#'
#' @examples
#' elo_prediction(2000, 1842)
#'
#' @export
#'
#' @return Match win probability for Player 1
elo_prediction <- function(rating1, rating2, adjust = 0, shrinkage = T){
		
	rating_diff <- rating2 - rating1
	
	pred <- 1 / (1 + 10^(rating_diff / 400 + adjust))
	
	if(shrinkage){
		if(pred > 0.5)
			pred + 0.1 * (0.5 - pred)
		else
			1 - ((1 - pred) + 0.1 * (0.5 - (1 - pred)))
	}
	else
		pred
}

