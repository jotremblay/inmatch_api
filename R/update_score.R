#' Update Score with Advantage Final Set
#'
#' Check score change based on result of current point and return score in terms of serving player of the previous point
#'
#' @param point_a Numeric game points won by current server at start of point
#' @param point_b Numeric game points won by current returner at start of point
#' @param game_a Numeric games won by current server in the current set
#' @param game_b Numeric games won by current returner in the current set
#' @param set_a Numeric sets won by current server
#' @param set_b Numeric sets won by current returner
#' @param set_b Numeric sets won by current returner
#' @param bestof3 Logical indicator if best-of-3 match
#'
#' @export
update_score <- function(pointa, pointb, gamea, gameb, seta, setb, bestof3 = T){
			
			regular.tiebreak <- (gamea + gameb) == 12 & (seta + setb) != ifelse(bestof3, 2, 4)
			
			serve.changed <- FALSE
			
			is.game.won <- function(pointa, pointb, regular.tiebreak){
				
				if(regular.tiebreak)
					pointa >= 7 & (pointa - pointb) >= 2
				else
					pointa >= 4 & (pointa - pointb) >= 2
			}
			
			
			is.set.won <- function(gamea, gameb, tiebreak){
				if(tiebreak)
					gamea >= 7 & gameb <= 6
				else
					gamea >= 6 & (gamea - gameb) >= 2
			}
			
			
			if(is.game.won(pointa, pointb, regular.tiebreak)){
				pointa <- 0
				pointb <- 0
				gamea <- gamea + 1
				serve.changed <- TRUE
			}
									
			if(is.game.won(pointb, pointa, regular.tiebreak)){
				pointa <- 0
				pointb <- 0
				gameb <- gameb + 1
				serve.changed <- TRUE
			}
						
			if(is.set.won(gamea, gameb, regular.tiebreak)){
				gamea <- 0
				gameb <- 0
				seta <- seta + 1
			}
				
			if(is.set.won(gameb, gamea, regular.tiebreak)){
				gamea <- 0
				gameb <- 0
				setb <- setb + 1
			}	
			
			# Point adjustments remaining after game/set outcomes
			if(pointa >= 6 & pointb >= 6 & pointa > pointb & regular.tiebreak){
				pointa <- 6
				pointb <- 5
			}
	
			if(pointa >= 6 & pointb >= 6 & pointa < pointb & regular.tiebreak){
				pointa <- 5
				pointb <- 6
			}
			
			if(pointa >= 6 & pointb >= 6 & pointa == pointb & regular.tiebreak){
				pointa <- 6
				pointb <- 6
			}	
			
			
			if(pointa >= 3 & pointb >= 3 & pointa > pointb & !regular.tiebreak){
				pointa <- 3
				pointb <- 2
			}
	
			if(pointa >= 3 & pointb >= 3 & pointa < pointb & !regular.tiebreak){
				pointa <- 2
				pointb <- 3
			}
			
			if(pointa >= 3 & pointb >= 3 & pointa == pointb & !regular.tiebreak){
				pointa <- 3
				pointb <- 3
			}						
	
			if(gamea >= 6 & gameb >= 6 & gamea == gameb & !regular.tiebreak){
				gamea <- 6
				gameb <- 6
			}		
			
			if(gamea >= 6 & gameb >= 6 & gamea > gameb & !regular.tiebreak){
				gamea <- 6
				gameb <- 5
			}				
			
			if(gamea >= 6 & gameb >= 6 & gamea < gameb & !regular.tiebreak){
				gamea <- 5
				gameb <- 6
			}				
			
			if(regular.tiebreak){
				if(((pointa + pointb) %% 2) == 1) 
					serve.changed <- T
			}
			
			if(serve.changed)						
				data.frame(
					pointa = pointb, 
					pointb = pointa, 
					gamea = gameb, 
					gameb = gamea, 
					seta = setb,
					setb = seta,
					serve.changed = serve.changed
				)
			else
				data.frame(
					pointa = pointa, 
					pointb = pointb, 
					gamea = gamea, 
					gameb = gameb, 
					seta = seta,
					setb = setb,
					serve.changed = serve.changed
				)			
}