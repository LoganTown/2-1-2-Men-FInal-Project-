# Welcome message
cat("Welcome to the Logan, Landon, and Eli's Call of Duty Data Analyzer!\n")

# Imports functions from other programs
source("eli-get-by-cat.R")
source("logan-get-by-filter.r")
source("getData_LJMA.R")

esportsData <- data_array

# Test 1: Get player profile #passed #slight issue with duplicate profiles? maybe choose the first or something
cat("Running Test 1: Get player profile\n")
getPlayerProfile(esportsData, "Aqua")

# Test 2: Get player list #passed
cat("\nRunning Test 2: Get player list\n")
getPlayerListRunner(esportsData)






# Test 3: Compare players
cat("\nRunning Test 3: Compare players\n") #failed
comparePlayers(esportsData, "Alex","Aqua")

# Test 4: Compare specific stats of multiple players #failed
#cat("\nRunning Test 4: Compare specific stats of multiple players\n")
playerNames <- strsplit("Alex, Aqua", ",")[[1]]
playerNames <- trimws(playerNames)
statsToCompare <- strsplit("kills, wins, hits", ",")[[1]]
statsToCompare <- trimws(statsToCompare)
compareSpecificStats(esportsData, playerNames, statsToCompare)







# Test 5: Get highest stat #passed
cat("\nRunning Test 5: Get highest stat\n")
getHighestStat(esportsData, "kills")

# Test 6: Get stat list #passed
cat("\nRunning Test 6: Get stat list\n")
getStatListRunner(esportsData)