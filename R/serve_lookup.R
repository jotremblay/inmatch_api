#' Lookup Barnett and Clark Serve
#'
#' @param Character of player1 name
#' @param Character of player2 name
#'
#' @export
serve_lookup <- function(player1, player2){
	
	matches <- serve_effects %>% dplyr::filter(player %in% c(player1, player2))
	
	if(nrow(matches) == 0){
		quantile(serve_effects$serve, .05) 
	}
	else if(nrow(matches) == 1){
		if(matches$player == player1)
			serve_effects$serve[serve_effects$player == matches$player] - quantile(return_effects$return.impact, .05)
		else
			quantile(serve_effects$serve, .05) 
	}
	else{
	
		matches <- matches[order(matches$matches, decreasing = T),]
		
		serve <- serve_effects$serve[serve_effects$player == player1] - return_effects$return.impact[return_effects$player == player2]
		
	  serve
	}
}