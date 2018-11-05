library(inmatch)

player1id <- 'f324'
player2id <- 'c977'

get_baseline(player1id, player2id, mens = T, format = 'laver')

# [1] "playerid:f324,opponentid:c977,playerelo:2145.75830762778,opponentelo:1919.97259108237,playerwin:0.785789044578688,opponentwin:0.214210955421312,playerserve:0.731714751562691,opponentserve:0.663932748056341"

get_win_prob(
	player1.score = 0, 
	player2.score = 1, 
	player1.games = 0, 
	player2.games = 0, 
    player1.sets = 0, 
    player2.sets = 0, 
    player1.serve.won = 0, 
    player1.serve.points = 1, 
    player2.serve.won = 0, 
    player2.serve.points = 0, 
    player1.serve.prob = 0.73, 
    player2.serve.prob = 0.66, 
    player1.won = T, 
    player1.serving = T, 
    format = 'laver'
    )
    
# "player1.win.prob:73.2,player1.win.prob.point.won:74.1,player1.win.prob.point.lost:70.7,player1.importance:3.4,player2.win.prob:26.8,player2.win.prob.point.won:29.3,player2.win.prob.point.lost:25.9,player2.importance:3.4"    