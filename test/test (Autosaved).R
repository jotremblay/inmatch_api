library(inmatch)

men <- c(
	"Kevin Anderson" = "ATPA678", 
	"Jack Sock" = "ATPSM25", 
	"Yuki Bhambri" = "ATPBF55",
	"Rafael Nadal" = "ATPN409")

women <- c("Ashleigh Barty" = "WTA318033", 
	"Simona Halep" = "WTA314320",
	"Coco Vandeweghe" = "WTA314464",
	"Karolina Pliskova" = "WTA313974")

# CHECK BASELINE

## SINGLES
get_baseline(player1id = men[1], opponent1id = men[2], mens = T, format = "bestof5")

get_baseline(player1id = women[1], opponent1id = women[2], mens = F, format = "bestof3")

## DOUBLES
get_baseline(player1id = women[1], player2id = women[2], opponent1id = women[3], opponent2id = women[4], mens = F, format = "bestof3")

get_baseline(player1id = men[1], player2id = men[2], opponent1id = men[3], opponent2id = men[4], mens = T, format = "bestof5")


load(file = "~/Software/inmatch_api/test/ms701.RData")

ms701 <- ms701 %>% arrange(set, game, point)

get_win_prob <- Vectorize(get_win_prob)

# Doubles
ms701$format <- "doubles"

doubles <- ms701 %>%
	filter(set <= 2) %>%
	dplyr::mutate(
		player1.serve.prob = 0.65,
		player2.serve.prob = 0.65,
		player1.serve.inmatch = player1.serve.won / player2.serve.points,
		player2.serve.inmatch = player2.serve.won / player2.serve.points,
		malus = player1.serve.inmatch - player2.serve.inmatch,
		prediction = get_win_prob(
			format=format,
			player1.serving=player1.serving,
			player1.won=player1.won,
			player1.serve.prob=player1.serve.prob,
			player2.serve.prob=player2.serve.prob,
			player1.serve.points=player1.serve.points,
			player2.serve.points=player2.serve.points,
			player1.serve.won=player1.serve.won,
			player2.serve.won=player2.serve.won,
			player1.sets=player1.sets,
			player2.sets=player2.sets,
			player1.games=player1.games,
			player2.games=player2.games,
			player1.score=player1.score,
			player2.score=player2.score
		)
	)
	
	
ms701$format <- "bestof5"
	
bestof3 <- ms701 %>%
	filter(set <= 3) %>%
	dplyr::mutate(
		player1.serve.prob = 0.65,
		player2.serve.prob = 0.65,
		player1.serve.inmatch = player1.serve.won / player2.serve.points,
		player2.serve.inmatch = player2.serve.won / player2.serve.points,
		malus = player1.serve.inmatch - player2.serve.inmatch,
		prediction = get_win_prob(
			format=format,
			player1.serving=player1.serving,
			player1.won=player1.won,
			player1.serve.prob=player1.serve.prob,
			player2.serve.prob=player2.serve.prob,
			player1.serve.points=player1.serve.points,
			player2.serve.points=player2.serve.points,
			player1.serve.won=player1.serve.won,
			player2.serve.won=player2.serve.won,
			player1.sets=player1.sets,
			player2.sets=player2.sets,
			player1.games=player1.games,
			player2.games=player2.games,
			player1.score=player1.score,
			player2.score=player2.score		
			)
	)
		
		
	
ms701$format <- "bestof5"
	
bestof5 <- ms701 %>%
	dplyr::mutate(
		player1.serve.prob = 0.65,
		player2.serve.prob = 0.65,
		player1.serve.inmatch = player1.serve.won / player2.serve.points,
		player2.serve.inmatch = player2.serve.won / player2.serve.points,
		malus = player1.serve.inmatch - player2.serve.inmatch,
		prediction = get_win_prob(
			format=format,
			player1.serving=player1.serving,
			player1.won=player1.won,
			player1.serve.prob=player1.serve.prob,
			player2.serve.prob=player2.serve.prob,
			player1.serve.points=player1.serve.points,
			player2.serve.points=player2.serve.points,
			player1.serve.won=player1.serve.won,
			player2.serve.won=player2.serve.won,
			player1.sets=player1.sets,
			player2.sets=player2.sets,
			player1.games=player1.games,
			player2.games=player2.games,
			player1.score=player1.score,
			player2.score=player2.score		
			)
	)
		
	