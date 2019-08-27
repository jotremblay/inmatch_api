#' @export
get_h2h <- function(id1, id2){
	
	adjust <- h2h %>%
			filter(playerid == id1, opponentid == id2)
	
	if(nrow(adjust) == 0){
		adjust <- h2h %>%
			filter(opponentid == id1, playerid == id2)
		if(nrow(adjust) != 0){
			-1 * adjust$adjust
		}
		else{
			0
		}
	}
	else{
		adjust$adjust
	}
	
	
}