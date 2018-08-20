library(sofascoreR)

x <- fromJSON("https://www.sofascore.com/u-tournament/10524/season/14660/json?_=153411861")

events <- x$events$weekMatches$tournaments$events[[1]]

# Laver cup matches
matches <- lapply(events$id, pbp)

save(events, matches, file = "~/Software/inmatch_api/laver_cup.RData")