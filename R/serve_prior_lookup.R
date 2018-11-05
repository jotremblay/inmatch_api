#' Get Serve Priors
#'
#' Opponent corrected serve expectation
#'
#' @param player_id. ATP/WTA id for player
#' @param player2_id. ATP/WTA id for player (only for doubles)
#' @param opponent_id. ATP/WTA id for player
#' @param opponent2_id. ATP/WTA id for player (only for doubles)
#' @param mens. Logical if ATP or WTA match
#'
#' @return list of expected serve probabilities
#'
#' @export
#'
serve_prior_lookup <- function(player_id, player2_id = NULL, opponent_id, opponent2_id = NULL, mens){

	if(mens & is.null(player2_id)){
		player.serve <- atp_serve_priors %>% filter(playerid == player_id, opponentid == opponent_id)
	
		opponent.serve <- atp_serve_priors %>% filter(playerid == opponent_id, opponentid == player_id)
		
		list(player = player.serve$prior, opponent = opponent.serve$prior)
	}
	else if(mens & !is.null(player2_id)){
			player.serve <- atp_doubles_serve_priors %>% filter(playerid %in% c(player_id, player2_id), opponentid %in% c(opponent_id, opponent2_id))
			
			opponent.serve <- atp_doubles_serve_priors %>% filter(playerid %in% c(opponent_id, opponent2_id), opponentid %in% c(player_id, player2_id))
	
		list(player = mean(player.serve$prior), opponent = mean(opponent.serve$prior))
		}
	else if(!mens & is.null(player2_id)){
		player.serve <- wta_serve_priors %>% filter(playerid == player_id, opponentid == opponent_id)
	
		opponent.serve <- wta_serve_priors %>% filter(playerid == opponent_id, opponentid == player_id)
		
		list(player = player.serve$prior, opponent = opponent.serve$prior)
	}
	else{
			player.serve <- wta_doubles_serve_priors %>% filter(playerid %in% c(player_id, player2_id), opponentid %in% c(opponent_id, opponent2_id))
			
			opponent.serve <- wta_doubles_serve_priors %>% filter(playerid %in% c(opponent_id, opponent2_id), opponentid %in% c(player_id, player2_id))
	
		list(player = mean(player.serve$prior), opponent = mean(opponent.serve$prior))
	}

}