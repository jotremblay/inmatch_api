library(inmatch)

load(file = "~/Software/inmatch_api/R/sysdata.rda")

iid_player_probs_lookup <- function(pa, pb){
		
			pa <- max(c(pa, 0.5))
			pb <- max(c(pb, 0.5))
		
			pa <- min(c(pa, 0.99))
			pb <- min(c(pb, 0.99))
				
			p1 <- as.character(round(pa, 2))
			p2 <- as.character(round(pb, 2))
			
			id1 <- paste(p1, p2, sep = ":")
			id2 <- paste(p2, p1, sep = ":")
				
			gameA_mat <- regular_game_matrices[[p1]]
			gameB_mat <- regular_game_matrices[[p2]]
			
			tbgameA_mat <- tiebreak_game_matrices[[id1]]
			tbgameB_mat <- tiebreak_game_matrices[[id2]]
			
			settbgameA_mat <- set_win_tiebreak[[id1]]
			settbgameB_mat <- set_win_tiebreak[[id2]]
			
			setadvgameA_mat <- set_win_advantage[[id1]]
			setadvgameB_mat <- set_win_advantage[[id2]]
			
			MA <- advantage_matches[[id1]]
			MB <- advantage_matches[[id2]]
			
		
		list(A = list(game = gameA_mat, tiebreak = tbgameA_mat, set_tiebreak = settbgameA_mat, set_advantage = setadvgameA_mat, match = MA), 
			B = list(game = gameB_mat, tiebreak = tbgameB_mat, set_tiebreak = settbgameB_mat, set_advantage = setadvgameB_mat, match = MB)
			)
		}
			
data <- iid_player_probs_lookup(0.6, 0.65)	


# GAME CHANCES		
game_win_prob(3, 2, T, F, matrices = data) # Chance serving player wins
#  0.8769231

game_win_prob(3, 2, F, F, matrices = data) # Change serving player loses
#  0.1230769

game_win_prob(2, 2, T, T, matrices = data) # Chance serving player wins tiebreak game
#  0.4289967

game_win_prob(3, 3, F, T, matrices = data) # Change serving player loses tiebreak game
#0.434843

game_win_prob(5, 3, T, is.regular.tiebreak = T, matrices = data) # Change serving player loses tiebreak game
# 0.7799404

# SET CHANCES
set_win_prob(6, 4, win_set = T, matrices = data)
# 1

set_win_prob(6, 4, win_set = F, matrices = data) # Should return 0 for inconsistencies?
# 0

set_win_prob(3, 4, win_set = T, matrices = data)
# 0.1075093

set_win_prob(3, 4, win_set = F, matrices = data)
# 0.8924907

## MATCH CHANCES
match_win_prob(3, 0, matrices = data) # Already won
match_win_prob(0, 3, matrices = data) # Already lost
match_win_prob(1, 0, matrices = data) # Already won
# 0.408421

# CONDITIONAL MATCH WIN GIVEN GAME AND SET RESULT

# Winning from event point and game and winning set
win_loss_chance(win_game = T, win_set = T, 1, 1, 1, 1, 0, 0, is.regular.tiebreak = F, matrices = data)

# Winning from event point and game and losing set
win_loss_chance(win_game = T, win_set = F, 1, 1, 1, 1, 0, 0, is.regular.tiebreak = F, matrices = data)


# Winning from tiebreak
win_loss_chance(win_game = T, win_set = T, 1, 1, 6, 6, 0, 0, is.regular.tiebreak = T, matrices = data)

# Winning from advantage
win_loss_chance(win_game = T, win_set = T, 1, 1, 6, 6, 1, 1, is.regular.tiebreak = F, matrices = data)


## MATCH WINS
match_win(3, 2, 2, 2, 0, 0, 0.65, 0.6)

match_win(3, 2, 4, 2, 0, 0, 0.65, 0.6)

match_win(3, 2, 2, 2, 0, 1, 0.65, 0.6)

match_win(3, 2, 4, 2, 0, 1, 0.65, 0.6)
