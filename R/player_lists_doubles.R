#' @export
player_lists_doubles <- function(void){
	
	data(wta_players, package = "oncourt")
	
	url <- "https://api.wtatennis.com/tennis/players/ranked?page=NUM&pageSize=100&type=rankDoubles&sort=asc&name=&metric=DOUBLES&at=&nationality="
	
	urls <- sapply(0:9, function(x) sub("NUM", x, url))
	
	fromJSONx <- function(x){
		x <- fromJSON(x)$player		
		rownames(x) <- x$id
	x
	}
	
	rankings <- do.call("rbind", lapply(urls, fromJSONx))
	
	rankings <- data.frame(
			player = rankings$fullName,
			wtaid = paste("WTA", rankings$id, sep = ""),
			stringsAsFactors = F
		)
		
	# Match players to oncourt
	rankings$pattern <- sapply(rankings$player, function(x){
		splitx <- strsplit(x, " ")[[1]]
		paste("^", substr(x, 1, 1), ".*", paste(splitx[length(splitx)], "$", sep = ""), sep = "")
	})
	
	matches <- mapply(
		function(x, y, source){
			if(any(x == source))
				x
			else
				grep(y, source, ignore = T, val = T)
		},
		x = rankings$player,
		y = rankings$pattern,
		MoreArgs = list(source = wta_players$NAME_P)
	)
		
	matches <- sapply(matches, function(x){
		if(length(x) == 1)
			x
		else
			NA
	})		
	
	rankings$player <- unlist(matches) # Name as in oncourt
	
	rankings <- rankings %>%
		left_join(wta_players %>% dplyr::select(player = NAME_P, oncourtid = ID_P), by = "player")

	rankings$oncourtid[rankings$wtaid == "WTA320124"] <- "17142"
	rankings$oncourtid[rankings$wtaid == "WTA326376"] <- "47152"
	rankings$oncourtid[rankings$wtaid == "WTA318352"] <- "13310"
	rankings$oncourtid[rankings$wtaid == "WTA326735"] <- "49081"
	rankings$oncourtid[rankings$wtaid == "WTA314359"] <- "8894"
	rankings$oncourtid[rankings$wtaid == "WTA312660"] <- "6054"
	rankings$oncourtid[rankings$wtaid == "WTA315815"] <- "10132"
	rankings$oncourtid[rankings$wtaid == "WTA328592"] <- "57363"
	rankings$oncourtid[rankings$wtaid == "WTA316694"] <- "12439"
	rankings$oncourtid[rankings$wtaid == "WTA322417"] <- "29825"
	rankings$oncourtid[rankings$wtaid == "WTA323440"] <- "33337"
	rankings$oncourtid[rankings$wtaid == "WTA316942"] <- "11619"
	rankings$oncourtid[rankings$wtaid == "WTA321339"] <- "17759"
	rankings$oncourtid[rankings$wtaid == "WTA324255"] <- "37189"
	rankings$oncourtid[rankings$wtaid == "WTA311885"] <- "6867"
	rankings$oncourtid[rankings$wtaid == "WTA325861"] <- "44617"
	rankings$oncourtid[rankings$wtaid == "WTA320852"] <- "16141"
	rankings$oncourtid[rankings$wtaid == "WTA320342"] <- "17696"
	rankings$oncourtid[rankings$wtaid == "WTA329248"] <- "63052"
	rankings$oncourtid[rankings$wtaid == "WTA326908"] <- "49987"
	rankings$oncourtid[rankings$wtaid == "WTA317351"] <- "12140"
	rankings$oncourtid[rankings$wtaid == "WTA317351"] <- "12140"
	rankings$oncourtid[rankings$wtaid == "WTA328308"] <- "57876"
	rankings$oncourtid[rankings$wtaid == "WTA130591"] <- "438"
	rankings$oncourtid[rankings$wtaid == "WTA324961"] <- "39745"
	rankings$oncourtid[rankings$wtaid == "WTA322733"] <- "25366"
	rankings$oncourtid[rankings$wtaid == "WTA313285"] <- "6866"
	rankings$oncourtid[rankings$wtaid == "WTA328910"] <- "60939"
	
	# Cannot match "WTA320386"
	rankings <- rankings %>% filter(wtaid != "WTA320386")
	
	## Missing
	if(any(is.na(rankings$oncourtid))){
		print(rankings %>% filter(is.na(oncourtid)))
		stop("Some player ids could not be found.")
	}
			 
	wta_event_players_doubles <- rankings %>%
		dplyr::mutate_all(
			funs(as.character)
		)		
	
	save(wta_event_players_doubles, file = "~/Software/inmatch_api_10/data/wta_event_players_doubles.RData")
				
	url <- "https://www.atpworldtour.com/en/rankings/doubles?rankDate=&rankRange=0-1000&ajax=true"
	
	page <- read_html(url)
	
	ids <- page %>%
		html_nodes("table.mega-table") %>%
		html_nodes("a") %>%
		html_attr("href")
		
	ids <- grep("players", ids, val = T)
	
	atp_links <- data.frame(
		player = sapply(ids, function(x) strsplit(x, "/")[[1]][4]),
		atpid = paste("ATP", toupper(sapply(ids, function(x) strsplit(x, "/")[[1]][5])), sep = ""),
		stringsAsFactors = F,
		row.names = NULL
	) %>%
	unique()
	
	data(atp_players, package = "oncourt")
	
	# Match players to oncourt
	atp_links$pattern <- gsub("-", ".*", atp_links$player)
	
	# Attempt to match each ranked player to their oncourt name
	matches <- mapply(
		function(x, y, source){
			result <- grep(paste(y, "$", sep = "", collapse = ""), source, ignore = T, val = T)
			if(length(result) == 0){
				splitx <- strsplit(x, "-")[[1]]
				pattern <- paste(paste("^", substr(splitx[1], 1, 1), sep = ""), ".*", paste(splitx[length(splitx)], "$", sep = ""), sep = "")
				result <- grep(pattern, source, ignore = T, val = T)
				if(length(result) == 0){
					result <- grep(splitx[length(splitx)], source, ignore = T, val = T)
					}
				else
					result
				}
			else
				result
		},
		x = atp_links$player,
		y = atp_links$pattern,
		MoreArgs = list(source = atp_players$NAME_P)
	)
	
	
	matches <- sapply(matches, function(x){
		if(length(x) == 1)
			x
		else
			NA
	})	
	
	atp_links$player <- matches
	
	atp_links <- atp_links %>%
		left_join(atp_players %>% dplyr::select(player = NAME_P, oncourtid = ID_P), by = "player")

	atp_links$oncourtid[atp_links$atpid == "ATPCF88"] <- "22323"
	atp_links$oncourtid[atp_links$atpid == "ATPKF17"] <- "28762"
	atp_links$oncourtid[atp_links$atpid == "ATPN503"] <- "7345"
	atp_links$oncourtid[atp_links$atpid == "ATPL733"] <- "7462"
	atp_links$oncourtid[atp_links$atpid == "ATPW571"] <- "12921"	
	atp_links$oncourtid[atp_links$atpid == "ATPG753"] <- "3992"	
	atp_links$oncourtid[atp_links$atpid == "ATPG892"] <- "28852"
	atp_links$oncourtid[atp_links$atpid == "ATPAB17"] <- "21499"	
	atp_links$oncourtid[atp_links$atpid == "ATPSG64"] <- "7818"	
	atp_links$oncourtid[atp_links$atpid == "ATPBE69"] <- "10404"	
	atp_links$oncourtid[atp_links$atpid == "ATPTD47"] <- "29889"
	atp_links$oncourtid[atp_links$atpid == "ATPR913"] <- "8440"	
	atp_links$oncourtid[atp_links$atpid == "ATPSN44"] <- "19812"	
	atp_links$oncourtid[atp_links$atpid == "ATPSF89"] <- "1742"	
	atp_links$oncourtid[atp_links$atpid == "ATPKG72"] <- "30419"	
	atp_links$oncourtid[atp_links$atpid == "ATPCN08"] <- "34964"
	atp_links$oncourtid[atp_links$atpid == "ATPG820"] <- "23462"
	atp_links$oncourtid[atp_links$atpid == "ATPC813"] <- "4025"
	atp_links$oncourtid[atp_links$atpid == "ATPDA30"] <- "18457"	
	atp_links$oncourtid[atp_links$atpid == "ATPSI82"] <- "8354"	
	atp_links$oncourtid[atp_links$atpid == "ATPMC18"] <- "2034"
	atp_links$oncourtid[atp_links$atpid == "ATPU107"] <- "7348"	
	atp_links$oncourtid[atp_links$atpid == "ATPKF24"] <- "28869"
	atp_links$oncourtid[atp_links$atpid == "ATPPE42"] <- "14889"	
	atp_links$oncourtid[atp_links$atpid == "ATPPG56"] <- "24069"	
	atp_links$oncourtid[atp_links$atpid == "ATPD0A3"] <- "48854"
	atp_links$oncourtid[atp_links$atpid == "ATPSM71"] <- "17003"
	atp_links$oncourtid[atp_links$atpid == "ATPRB04"] <- "22555"
	atp_links$oncourtid[atp_links$atpid == "ATPA0E2"] <- "68074"
	atp_links$oncourtid[atp_links$atpid == "ATPS0DH"] <- "44548"	
	atp_links$oncourtid[atp_links$atpid == "ATPD0AR"] <- "46407"
	atp_links$oncourtid[atp_links$atpid == "ATPM0AA"] <- "48644"
	atp_links$oncourtid[atp_links$atpid == "ATPWB14"] <- "33154"
	atp_links$oncourtid[atp_links$atpid == "ATPH0AG"] <- "52188"
	atp_links$oncourtid[atp_links$atpid == "ATPD0DT"] <- "72574"
	atp_links$oncourtid[atp_links$atpid == "ATPP0DF"] <- "64916"		
	atp_links$oncourtid[atp_links$atpid == "ATPQ019"] <- "754"
	atp_links$oncourtid[atp_links$atpid == "ATPL480"] <- "631"
	atp_links$oncourtid[atp_links$atpid == "ATPB0AC"] <- "34609"	
	atp_links$oncourtid[atp_links$atpid == "ATPWB07"] <- "33234"
	atp_links$oncourtid[atp_links$atpid == "ATPTE33"] <- "33645"
	atp_links$oncourtid[atp_links$atpid == "ATPR09V"] <- "44217"
	atp_links$oncourtid[atp_links$atpid == "ATPMS03"] <- "30555"

	
	## Missing
	if(any(is.na(atp_links$oncourtid))){
		print(atp_links %>% filter(is.na(oncourtid)))
		stop("Some player ids could not be found.")
	}
	
	atp_event_players_doubles <- atp_links %>%
		dplyr::mutate_all(
			funs(as.character)
		)
	
	save(atp_event_players_doubles, file = "~/Software/inmatch_api_10/data/atp_event_players_doubles.RData")

print("Saved datasets to data folder")
}
