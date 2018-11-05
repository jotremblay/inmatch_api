# Prepare serve return for atp players
library(devtools)
library(oncourt)
library(GIGEngineDB)
library(sofascoreR)
library(inmatch)

rm(list = ls())

load(file = "~/Software/inmatch_api/R/sysdata.rda")

# database players and ids
con <- make_connection(database = "TennisMatchStats")

player_names <- tbl(con, "Player") %>% collect() 

player_names <- player_names %>%	
	dplyr::mutate(
		name = paste(FirstName, LastName),
		tour = substr(PlayerCode, 1, 3)
	) %>%
	filter(tour %in% c("ATP", "WTA")) %>%
	rename(playerid = PlayerCode)


# oncourt singles players and ids
data(atp_players, package = "oncourt")
data(wta_players, package = "oncourt")

atp_players$tour <- "ATP"
wta_players$tour <- "WTA"

oncourt_players <- rbind(atp_players, wta_players) %>%
	select(name = NAME_P, ID_P, tour)

data(atp_doubles, package = "oncourt")
data(wta_doubles, package = "oncourt")

atp_doubles$tour <- "ATP"
wta_doubles$tour <- "WTA"

oncourt_doubles_players <- rbind(atp_doubles, wta_doubles) %>%
	select(NAME1, NAME2, Player1, Player2, tour)
	
oncourt_doubles_players <- rbind(
	oncourt_doubles_players %>% select(name = NAME1, ID_P = Player1, tour),
	oncourt_doubles_players %>% select(name = NAME2, ID_P = Player2, tour)
)	%>% unique()


# elo ratings and identifier
atp_elo <- current_elo(mens = T, surface = "Hard")	
wta_elo <- current_elo(mens = F, surface = "Hard")	

atp_elo <- atp_elo %>% 
	select(name = Player, elo = Hard, matches = player_match_num)
	
wta_elo <- wta_elo %>% 
	select(name = Player, elo = Hard, matches = player_match_num)	

data(atp_elo_doubles, package = "oncourt")
data(wta_elo_doubles, package = "oncourt")

atp_elo_doubles <- atp_elo_doubles %>% rename(name = player, elo = Elo)
wta_elo_doubles <- wta_elo_doubles %>% rename(name = player, elo = Elo)

# Clean names
player_names$name <- clean_names(player_names$name)
oncourt_players$name <- clean_names(oncourt_players$name)
oncourt_doubles_players$name <- clean_names(oncourt_doubles_players$name)
atp_elo$name <- clean_names(atp_elo$name)
wta_elo$name <- clean_names(wta_elo$name)
atp_elo_doubles$name <- clean_names(atp_elo_doubles$name)
wta_elo_doubles$name <- clean_names(wta_elo_doubles$name)

# Oncourt replace
oncourt_replace <- structure(c("NIKALA SCHOLTZ", "PATRIK BRYDOLF", "SUDANWA SITARAM", 
"VIKTORIA KAMENSKAYA", "VINCE SPADEA", "TEIMURAZ GABASHVILI", 
"SAMUEL GROTH", "VIKTORIA KUTUZOVA", "KIMIKO DATE", "MARGALITA CHAKHNASHVILIRANZINGER", 
"TZIPI OBZILER", "SILVIA SOLERESPINOSA", "YEVHENIIA SAVRANSKA", 
"KYUTAE IM", "ALEXANDER SATSCHKO", "YURIY SCHUKIN", "MIGUELANGEL LOPEZ JAEN", 
"ILIJA BOZOLJAC", "MIKHAIL ELGIN", "ROSANA DE LOS RIOS", "NADEZDA PETROVA", 
"MARIAELENA CAMERIN", "ALIZE CORNET", "MARIANA DUQUEMARINO", 
"CARLA SUAREZ NAVARRO", "MARIAJOSE MARTINEZ SANCHEZ", "LOURDES DOMINGUEZLINO", 
"ANASTASIA YAKIMOVA", "JASON MURRAY KUBLER", "MAREE TEIWA CASEY", 
"HAOCHEN TANG", "ELOISAMARIA COMPOSTIZODE ANDRES", "INIGO CERVANTES", 
"ALEX JR BOGOMOLOV", "PABLO SANTOSGONZALEZ", "ARANTXA PARRASANTONJA", 
"YARASLAV SHYLA", "THIAGO MOURA MONTEIRO", "RUBEN RAMIREZHIDALGO", 
"IRINI GEORGATOU", "LIANAGABRIELA UNGUR", "LESYA TSURENKO", "ROGERIO DUTRA DA SILVA", 
"JUANSEBASTIAN CABAL", "MARKOS KALOVELONIS", "GABRIELLE FAITH ANDREWS", 
"ELENATEODORA CADAR", "NASTASSJA BURNETT", "CHIARA SCHOLL", "DENIS MATSUKEVITCH", 
"JUANPABLO BRZEZICKI", "ALEKSANDR SPIRIN", "THAI KWIATKOWSKI", 
"ALLIE KIICK", "VALERIYA SOLOVYEVA", "INES FERRERSUAREZ", "DIEGO SEBASTIAN SCHWARTZMAN", 
"JULIOCESAR CAMPOZANO", "DENNIS ZIVKOVIC", "ALEKSANDR NEDOVESOV", 
"GARBINE MUGURUZA", "ALIZE LIM", "PABLO CARRENOBUSTA", "SEONG CHAN HONG", 
"SNEHADEVI S REDDY", "RYOTARO MATSUMURA", "GREETJE MINNEN", "LLOYD HARRIS", 
"SOONWOO KWON", "TAYLOR HARRY FRITZ", "EZEKIEL CLARK", "GEORGIA ANDREEA CRACIUN", 
"KARMAN KAUR THANDI", "JIA QI REN", "ANASTASIA ZARYCKA", "LIUDMILA SAMSONOVA", 
"VICTOR ESTRELLA", "SAKETHSAI MYNENI", "DARYA KASATKINA", "YUUYA KIBI", 
"ALAFIA AYENI", "KAYA GORE", "HIMARI SATO", "PATRICIA MAYRACHLEITNER"), .Names = c("NICK SCHOLTZ", 
"PATRK BRYDOLF", "SUDARWA SITARAM", "VICTORIA KAMENSKAYA", "VINCENT SPADEA", 
"TEYMURAZ GABASHVILI", "SAM GROTH", "VIKTORIYA KUTUZOVA", "KIMIKO DATE KRUMM", 
"MARGALITA CHAKHNASHVILI", "TZIPORA OBZILER", "SLVIA SOLERESPINOSA", 
"YEVGENIA SAVRANSKA", "KYU TAE IM", "ALEX SATSCHKO", "YURI SCHUKIN", 
"MIGUEL ANGEL LOPEZ JAEN", "ILIA BOZOLJAC", "MICHAIL ELGIN", 
"ROSSANA DE LOS RIOS", "NADIA PETROVA", "MARIA ELENA CAMERIN", 
"ALIZ CORNET", "MARIANA DUQUEMARIO", "CARLA SUREZ NAVARRO", "MARA JOS MARTNEZ SNCHEZ", 
"LOURDES DOMINGUEZ LINO", "ANASTASIYA YAKIMOVA", "JASON KUBLER", 
"TEIWA CASEY", "HAO CHEN TANG", "ELOISA COMPOSTIZO DE ANDRES", 
"INIGO CERVANTESHUEGUN", "ALEX BOGOMOLOV JR", "PABLO SANTOS", 
"ARANTXA PARRA SANTONJA", "YARASLAU SHYLA", "THIAGO MONTEIRO", 
"RUBEN RAMIREZ HIDALGO", "EIRINI GEORGATOU", "LIANA UNGUR", "LESIA TSURENKO", 
"ROGERIO DUTRA SILVA", "JUAN SEBASTIAN CABAL", "MARKUS KALOVELONIS", 
"GABRIELLE ANDREWS", "ELENATHEODORA CADAR", "NASTASSYA BURNETT", 
"CHICHI SCHOLL", "DENIS MATSUKEVICH", "JUAN PABLO BRZEZICKI", 
"ALEKSANDR IVANOVICH SPIRIN", "THAISON KWIATKOWSKI", "ALEXANDRA KIICK", 
"VALERIA SOLOVIEVA", "INES FERRER SUAREZ", "DIEGO SCHWARTZMAN", 
"JULIO CESAR CAMPOZANO", "DENIS ZIVKOVIC", "OLEKSANDR NEDOVYESOV", 
"GARBIE MUGURUZA", "ALIZ LIM", "PABLO CARRENO BUSTA", "SEONGCHAN HONG", 
"SNEHADEVI REDDY", "RYOTERO MATSUMURA", "GREET MINNEN", "LLOYD GEORGE HARRIS", 
"SOON WOO KWON", "TAYLOR FRITZ", "ZEKE CLARK", "GEORGIA CRACIUN", 
"KARMAN THANDI", "JIA REN", "ANASTASIA ZARYTSKA", "LUDMILLA SAMSONOVA", 
"VICTOR ESTRELLA BURGOS", "SAKETH MYNENI", "DARIA KASATKINA", 
"YUYA KIBI", "OLUKAYODE ALAFIA DAMINA AYENI", "KAYA GORE ", "HIMARI SATO ", "PATRICIA MAYR"
))


for(i in names(oncourt_replace)){
	player_names$name[player_names$name == i] <- oncourt_replace[i]
}

# Merge player table and oncourt
players <- player_names %>%
	left_join(oncourt_players, by = c("name", "tour")) 
	
doubles_players <- player_names %>%
	left_join(oncourt_doubles_players, by = c("name", "tour")) 
		
		
# Check for missing
# missing <- doubles_players[is.na(doubles_players$ID_P),]

# missing <- lapply(missing$ShortName, function(x){
	# x <- grep(paste("^", sub("\\. ?", ".*", x), sep = "", collapse = ""), oncourt_doubles_players$name, ignore = T, val = T)
	# # Check if not included
# setdiff(x, doubles_players$name)
# })

# names(missing) <- doubles_players$name[is.na(doubles_players$ID_P)]

replace <- structure(c("TEYMURAZ GABASHVILI", "ANDREEV A", "JO WILFRIED TSONGA", 
"YEN HSUN LU", "PAUL HENRI MATHIEU", "HERNANDEZ FERNANDEZ J", 
"GUILLERMO GARCIA LOPEZ", "EDOUARD ROGER VASSELIN", "KUBLER J", 
"HORANSKY F", "ADRIAN MENENDEZ MACEIRAS", "INIGO CERVANTES HUEGUN", 
"ALBERT RAMOS", "ROCA BATALLA O", "THIAGO MONTEIRO", "RICARDAS BERANKIS", 
"VIOLA M", "FRANCO SKUGOR", "KARLOVSKIY E", "KADHE A", "CEDRIK MARCEL STEBE", 
"JOHN PATRICK SMITH", "BALDI F", "THAI SON KWIATKOWSKI", "KLAHN B", 
"PIERRE HUGUES HERBERT", "JAN LENNARD STRUFF", "PABLO CARRENO BUSTA", 
"NAGAL S", "PURCELL M", "LLOYD GEORGE MUIRHEAD HARRIS", "KWON S", 
"WATANUKI Y", "HURKACZ H", "MOUTET C", "HUMBERT U", "AUGER ALIASSIME F", 
"POPYRIN A", "BENCHETRIT E", "CARUANA L", "PIROS Z", "CLARKE J", 
"CLARKE J", "MOLLEKER R", "KECMANOVIC M", "GERASIMOV E", "SAKETH MYNENI", 
"JANVIER M", "BAI M", "AREVALO M", "KOLAR Z", "TSENG C", "LPEZ F",  "DUMHUR D"), .Names = c("TEIMURAZ GABASHVILI", 
"IGOR ANDREEV", "JOWILFRIED TSONGA", "YENHSUN LU", "PAULHENRI MATHIEU", 
"OSCAR HERNANDEZ", "GUILLERMO GARCIALOPEZ", "EDOUARD ROGERVASSELIN", 
"JASON MURRAY KUBLER", "FILIP HORANSKY", "ADRIAN MENENDEZMACEIRAS", 
"INIGO CERVANTES", "ALBERT RAMOSVINOLAS", "ORIOL ROCA BATALLA", 
"THIAGO MOURA MONTEIRO", "RICHARD BERANKIS", "MATTEO VIOLA", 
"FRANKO SKUGOR", "EVGENY KARLOVSKIY", "ARJUN KADHE", "CEDRIKMARCEL STEBE", 
"JOHNPATRICK SMITH", "FILIPPO BALDI", "THAI KWIATKOWSKI", "BRADLEY KLAHN", 
"PIERREHUGUES HERBERT", "JANLENNARD STRUFF", "PABLO CARRENOBUSTA", 
"SUMIT NAGAL", "MAX PURCELL", "LLOYD HARRIS", "SOONWOO KWON", 
"YOSUKE WATANUKI", "HUBERT HURKACZ", "CORENTIN MOUTET", "UGO HUMBERT", 
"FELIX AUGER ALIASSIME", "ALEXEI POPYRIN", "ELLIOT BENCHETRIT", 
"LIAM CARUANA", "ZSOMBOR PIROS", "EZEKIEL CLARK", "JAY CLARKE", 
"RUDOLF MOLLEKER", "MIOMIR KECMANOVIC", "EGOR GERASIMOV", "SAKETHSAI MYNENI", 
"MAXIME JANVIER", "YAN BAI", "MARCELO AREVALO", "ZDENEK KOLAR", 
"CHUN HSIN TSENG", "FELICIANO LOPEZ",  "DAMIR DZUMHUR"))


for(i in replace){
	atp_elo$name[atp_elo$name == i] <- names(replace)[replace == i]
}


replace <- structure(c("STEPHANIE FORETZ GACON", "KIMIKO DATE KRUMM", "BARBORA ZAHLAVOVA STRYCOVA", 
"SU WEI HSIEH", "LOURDES DOMINGUEZ LINO", "KATERINA BONDARENKO", 
"HAO CHEN TANG", "KAI CHEN CHANG", "SO RA LEE", "ANNA KAROLINA SCHMIEDLOVA", 
"AN SOPHIE MESTACH", "IRINA CAMELIA BEGU", "LESIA TSURENKO", 
"ROSCA A", "REKA LUCA JANI", "JING JING LU", "SCHOOFS B", "SAVINYKH V", 
"CHI CHI SCHOLL", "NICOLETA CATALINA DASCALU", "CHING WEN HSU", 
"VALERIA SOLOVIEVA", "YING YING DUAN", "CRISTINA ANDREEA MITU", 
"JIL BELEN TEICHMANN", "WALLACE I", "KARMAN THANDI", "DANILOVIC O", 
"KAI LIN ZHANG", "ANNA LENA FRIEDSAM", "YI FAN XU", "KOSTYUK M", "GRGES J", 
"BETHANIE MATTEK SANDS", "SORANA MIHAELA CIRSTEA", "MIRJANA LUCIC BARONI", "MARIANA DUQUE MARINO"
), .Names = c("STEPHANIE FORETZ", "KIMIKO DATE", "BARBORA STRYCOVA", 
"SUWEI HSIEH", "LOURDES DOMINGUEZLINO", "KATERYNA BONDARENKO", 
"HAOCHEN TANG", "KAICHEN CHANG", "SORA LEE", "ANNA SCHMIEDLOVA", 
"ANSOPHIE MESTACH", "IRINACAMELIA BEGU", "LESYA TSURENKO", "IOANA LOREDANA ROSCA", 
"REKALUCA JANI", "JINGJING LU", "BIBIANE SCHOOFS", "VALERIA SAVINYKH", 
"CHIARA SCHOLL", "NICOLETA DASCALU", "CHINGWEN HSU", "VALERIYA SOLOVYEVA", 
"YINGYING DUAN", "CRISTINA MITU", "JIL TEICHMANN", "ISABELLE WALLACE", 
"KARMAN KAUR THANDI", "OLGA DANILOVIC", "KAILIN ZHANG", "ANNALENA FRIEDSAM", 
"YIFAN XU", "MARTA KOSTYUK", "JULIA GOERGES", "BETHANIE MATTEKSANDS", "SORANA CIRSTEA", "MIRJANA LUCICBARONI", "MARIANA DUQUEMARINO"))

for(i in replace){
	wta_elo$name[wta_elo$name == i] <- names(replace)[replace == i]
}


# Top 150 check
top150 <- atp_elo[1:150,] %>%
	left_join(players %>% filter(tour == "ATP"), by = "name")

top150 <- wta_elo[1:150,] %>%
	left_join(players %>% filter(tour != "ATP"), by = "name")

top150 <- (atp_elo_doubles %>% 
	 arrange(-elo))[1:150,] %>%
	left_join(doubles_players %>% filter(tour == "ATP"), by = "name") 

top150 <- (wta_elo_doubles %>% 
	 arrange(-elo))[1:150,] %>%
	left_join(doubles_players %>% filter(tour == "ATP"), by = "name") 


# Assign IDs to elo ratings
atp_elo <- atp_elo %>% 
	right_join(players %>% filter(tour == "ATP"), by = "name") 

wta_elo <- wta_elo %>% 
	right_join(players %>% filter(tour != "ATP"), by = "name") 

# Doubles adjusted with oncourt replacement above
atp_elo_doubles <- atp_elo_doubles %>% 
	right_join(doubles_players %>% filter(tour == "ATP"), by = "name") 

wta_elo_doubles <- wta_elo_doubles %>% 
	right_join(doubles_players %>% filter(tour != "ATP"), by = "name") 


# Create matchup grid for 
atp_matchup_grid <- rbind(expand.grid(
	ID_P = players$ID_P[players$tour == "ATP"], 
	ID_O =  players$ID_P[players$tour == "ATP"]) %>%
	filter(ID_P != ID_O)
	)


serve_priors <- efron_morris(event = 14705, surface = "Hard", atp = T, doubles = F)

atp_serve_priors <- atp_matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))
	

atp_serve_priors <- atp_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)
	
	
atp_doubles_matchup_grid <- rbind(expand.grid(
	ID_P = doubles_players$ID_P[doubles_players$tour == "ATP"], 
	ID_O =  doubles_players$ID_P[doubles_players$tour == "ATP"]) %>%
	filter(ID_P != ID_O)
	)

serve_priors <- efron_morris(event = 14705, surface = "Hard", atp = T, doubles = T)

atp_doubles_serve_priors <- atp_doubles_matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))
	

atp_doubles_serve_priors <- atp_doubles_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)
	
		
serve_priors <- efron_morris(event = 11755, surface = "Hard", atp = F, doubles = F)

wta_matchup_grid <- rbind(expand.grid(
	ID_P = players$ID_P[players$tour != "ATP"], 
	ID_O =  players$ID_P[players$tour != "ATP"]) %>%
	filter(ID_P != ID_O)
	)


wta_serve_priors <- wta_matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))

wta_serve_priors <- wta_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)	


wta_doubles_matchup_grid <- rbind(expand.grid(
	ID_P = doubles_players$ID_P[doubles_players$tour != "ATP"], 
	ID_O =  doubles_players$ID_P[doubles_players$tour != "ATP"]) %>%
	filter(ID_P != ID_O)
	)

serve_priors <- efron_morris(event = 11755, surface = "Hard", atp = F, doubles = T)

wta_doubles_serve_priors <- wta_doubles_matchup_grid %>%
	left_join(serve_priors %>% select(ID_P, serve, event_serve)) %>%
	left_join(serve_priors %>% select(ID_O = ID_P, return))
	

wta_doubles_serve_priors <- wta_doubles_serve_priors %>%
	dplyr::mutate(
		event_serve = mean(event_serve, na.rm = T),
		serve = ifelse(is.na(serve), 0, serve),
		return = ifelse(is.na(return), 0, return),
		prior = event_serve + serve - return
	)
	
# Merge with playerid
atp_serve_priors <- atp_serve_priors %>%
	left_join(players %>% filter(tour == "ATP") %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(players %>% filter(tour == "ATP") %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")
	
wta_serve_priors <- wta_serve_priors %>%
	left_join(players %>% filter(tour != "ATP") %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(players %>% filter(tour != "ATP") %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")	
	
atp_doubles_serve_priors <- atp_doubles_serve_priors %>%
	left_join(doubles_players %>% filter(tour == "ATP") %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(doubles_players %>% filter(tour == "ATP") %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")
	
wta_doubles_serve_priors <- wta_doubles_serve_priors %>%
	left_join(doubles_players %>% filter(tour != "ATP") %>% select(playerid, ID_P), by = "ID_P")	%>%
	left_join(doubles_players %>% filter(tour != "ATP") %>% select(opponentid = playerid, ID_O = ID_P), by = "ID_O")		
	

devtools::use_data(
	atp_elo, 
	atp_elo_doubles,
	wta_elo,
	wta_elo_doubles,
	atp_serve_priors,
	wta_serve_priors,
	atp_doubles_serve_priors,
	wta_doubles_serve_priors,
	advantage_matches,
	regular_game_matrices,
	set_win_advantage,
	set_win_tiebreak,
	tiebreak_game_matrices,
	tiebreak_matches,
	tiebreak10,
	tiebreak10_matches,
	pkg = "~/Software/inmatch_api", 
	internal = TRUE, 
	overwrite = TRUE
	)

