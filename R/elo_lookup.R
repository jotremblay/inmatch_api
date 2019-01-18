#' Current Elo
#'
#' Extracts current elo from stored "elo" data
#'
#' @param id1 Character id of player 1
#' @param id2 Character id of player 2 (if doubles)
#' @param mens Logical if mens or womens match
#' @param default Numerical rating used when ID not found
#'
#' @return Numeric elo value
#'
#' @export
elo_lookup <- function(id1, id2 = NULL, mens, default = 1300){

	if(mens){
		if(is.null(id2)){
		elo <- atp_elo %>% filter(playerid == id1)
	
		if(nrow(elo) == 0 || is.na(elo$elo))
			default	
		else
			elo$elo		
		}
		else{
			
			elo1 <- atp_elo_doubles %>% filter(playerid == id1)
			elo2 <- atp_elo_doubles %>% filter(playerid == id2)

		if(nrow(elo1) == 0 || is.na(elo1$elo))
			elo1 <- default
		else
			elo1 <- elo1$elo	
			

		if(nrow(elo2) == 0 || is.na(elo2$elo))
			elo2 <- default
		else
			elo2 <- elo2$elo	
			
		(elo1 + elo2) / 2				
		}
	}
	else{
		if(is.null(id2)){
		elo <- wta_elo %>% filter(playerid == id1)
		
		if(nrow(elo) == 0 || is.na(elo$elo))
			default
		else
			elo$elo	
		}
		else{
			
			elo1 <- wta_elo_doubles %>% filter(playerid == id1)
			elo2 <- wta_elo_doubles %>% filter(playerid == id2)

		if(nrow(elo1) == 0 || is.na(elo1$elo))
			elo1 <- default
		else
			elo1 <- elo1$elo	
			

		if(nrow(elo2) == 0 || is.na(elo2$elo))
			elo2 <- default
		else
			elo2 <- elo2$elo	
			
		(elo1 + elo2) / 2
		}
	}
}