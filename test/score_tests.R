# Update score should return serving player status assuming point was just updated

# POINT SCORE
update_score(7, 6, 1, 1, 0, 0, T) 

update_score(7, 7, 6, 6, 1, 1, T) # Advantage set; treat like regular game

update_score(4, 4, 6, 6, 0, 0, T) # Tiebreak game


# GAME SCORE CHECKS
update_score(4, 4, 1, 1, 0, 0, T) # No change

update_score(6, 4, 1, 1, 0, 0, T) # Player A wins game

update_score(4, 6, 1, 1, 0, 0, T) # Player B wins game

# TIEBREAK HANDLING
update_score(7, 5, 6, 6, 0, 0, T) # Player A wins tiebreak

update_score(5, 7, 6, 6, 0, 0, T) # Player B wins tiebreak


# ADVANTAGE SET SCORE CHECKS
update_score(7, 5, 6, 6, 1, 1, T) # Player A wins game but not set

update_score(5, 7, 6, 6, 2, 2, F) # Player A wins game but not set

# MATCH SCORE CHECKS
update_score(7, 5, 6, 6, 1, 0, T) # Player A wins match

update_score(7, 5, 6, 6, 1, 0, F) # Player A wins set but not match

update_score(7, 5, 6, 6, 2, 1, F) # Player A wins match


update_score(5, 7, 6, 6, 0, 1, T) # Player B wins match

update_score(5, 7, 6, 6, 0, 1, F) # Player B wins set but not match

update_score(5, 7, 6, 6, 1, 2, T) # Player B wins match
