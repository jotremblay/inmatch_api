#' @export
serve_update <- function(won, points, prior){
			
			assign.weight <- function(points){
			 n0 <- 7500
			 n0 / (n0 + points^2)
			}		
			
			if(points == 0)
				prior
			else{
				W <- assign.weight(points)
				W * prior + (1 - W) * (won / points)
	}
}