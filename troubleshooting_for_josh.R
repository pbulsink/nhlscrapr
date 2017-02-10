fgd <- full.game.database()

#subset to a few games at the start of the season
game_ids <- subset(fgd, season == 20162017 & gcode <= 20012)

#scrape
dummy = download.games(games = game_ids, wait = 30)

#process
process.games(games = game_ids, override.download = FALSE)

#compile
compile.all.games(output.file="NHL_Playbyplay_2016-7.RData", seasons=20162017, new.game.table = game_ids)
