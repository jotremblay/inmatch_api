#' Current Elo
#'
#' Extracts current elo from stored "elo" data
#'
#' @param id Character id of player
#' @param mens Logical if mens or womens match
#'
#' @return Numeric elo value
#'
#' @export
elo_lookup <- function(id, mens){

	
	if(mens){
		elo <- atp_elo %>% filter(playerid == id)
	
		if(nrow(elo) == 0)
			min(atp_elo$elo) - 100	
		else
			elo$elo		
	}
	else{
		elo <- wta_elo %>% filter(playerid == id)
		
		if(nrow(elo) == 0)
			min(atp_elo$elo) - 100	
		else
			elo$elo	
	}

}