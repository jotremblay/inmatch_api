#' Update Score Laver
#'
#' Check score change based on result of current point for Laver Cup format
#'
#' @param point_a Numeric game points won by current server at start of point
#' @param point_b Numeric game points won by current returner at start of point
#' @param game_a Numeric games won by current server in the current set
#' @param game_b Numeric games won by current returner in the current set
#' @param set_a Numeric sets won by current server
#' @param set_b Numeric sets won by current returner
#' @param set_b Numeric sets won by current returner
#'
#' @export
update_score_laver <- function(pointa, pointb, gamea, gameb, seta, setb){
		
		regular.tiebreak <- (gamea + gameb) == 12 & (seta + setb) != 2
		
		tiebreak10 <- (seta + setb) == 2
		
		serve.changed <- FALSE
		
		is.game.won <- function(pointa, pointb, regular.tiebreak, tiebreak10){
			
			if(regular.tiebreak)
				pointa >= 7 & pointb <= 5
			else if(tiebreak10)
				pointa >= 10 & pointb <= 8
			else
				pointa >= 4 & pointb <= 2
		}
		
		is.set.won <- function(gamea, gameb){
			gamea == 6 & gameb <= 4 | gamea == 7 & gameb <= 6 
		}
		
		
		if(pointa == 7 & pointb == 6 & regular.tiebreak){
			pointa <- 6
			pointb <- 5
		}

		if(pointa == 6 & pointb == 7 & regular.tiebreak){
			pointa <- 5
			pointb <- 6
		}
		
		if(pointa == 10 & pointb == 9 & tiebreak10){
			pointa <- 9
			pointb <- 8
		}

		if(pointa == 9 & pointb == 10 & tiebreak10){
			pointa <- 8
			pointb <- 9
		}		

		if(pointa == 4 & pointb == 3 & !(regular.tiebreak | tiebreak10)){
			pointa <- 3
			pointb <- 2
		}

		if(pointa == 3 & pointb == 4 & !(regular.tiebreak | tiebreak10)){
			pointa <- 2
			pointb <- 3
		}
	
		if(is.game.won(pointa, pointb, regular.tiebreak, tiebreak10)){
			pointa <- 0
			pointb <- 0
			if(tiebreak10)
				seta <- seta + 1
			else
				gamea <- gamea + 1
			serve.changed <- TRUE
		}
								
		if(is.game.won(pointb, pointa,  regular.tiebreak, tiebreak10)){
			pointa <- 0
			pointb <- 0
			if(tiebreak10)
				setb <- setb + 1
			else
				gameb <- gameb + 1
			serve.changed <- TRUE
		}
		
			
		if(is.set.won(gamea, gameb)){
			gamea <- 0
			gameb <- 0
			seta <- seta + 1
		}
			
		if(is.set.won(gameb, gamea)){
			gamea <- 0
			gameb <- 0
			setb <- setb + 1
		}	
		
			
		if(regular.tiebreak | tiebreak10){
			if((pointa + pointb) %% 2 == 1)
				serve.changed <- T
		}
		
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
