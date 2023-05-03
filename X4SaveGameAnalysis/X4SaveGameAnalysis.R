# X4SaveGameAnalysis v1.1.1
# Created by Beamer Miasma 2019-now

# I'll claim no copyrights myself but if you try to make money off of this you'll probably get Egosoft on your
# case for marketing their IP, so don't do that. If you want to use the visualizations I create here as guides
# for your own work feel free, the credits go to the plotly and ggplot teams for providing these visualization
# libraries.

# OPTIONS
#
# A general note on file and directory paths:
# Use forward slashes (/) instead of backslashes.
# On most OSes "~" should expand to the user's documents folder in which the /Egosoft/X4 folder is located

# The path to the user's X4 folder
# Examples:
# path.X4user <- "~/Egosoft/X4"
# path.X4user <- "C:/Users/MyAccount/Documents/Egosoft/X4
path.X4user <- "~/Egosoft/X4"

# The path where the csv data files are stored. Should be OK if you followed the quick start instructions.
# Examples:
# path.CSVs <- "C:/Users/MyAccount/Documents/X4SaveGameAnalysis/data"
path.CSVs <- "~/X4SaveGameAnalysis/data"

# The path where the output images and html files and visualization libraries will be saved
path.Output <- "~/X4SaveGameAnalysis/output"

# When set to TRUE:
# - undiscovered sectors will be hidden from the map and contested sectors table
# - the detailed resource information sunburst plots will not be shown
# - undiscovered ships, stations and sectors will not be shown/counted in various sunburst plots and tables
# Example:
# spoilers.hide <- TRUE
spoilers.hide <- FALSE

# If you want to use a specific savegame rather than the most recent, enter the full path to it here
# Examples:
# savegame.override <- "C:/Users/MyAccount/Documents/Egosoft/X4/123456789/save/save_006.xml.gz"
# savegame.override <- "~/Egosoft/X4/123456789/save/quicksave.xml.gz"
# To automatically detect the most recent savegame, use:
savegame.override <- ""

# After installing new DLC (and making a new save with that DLC active), either delete the "cache_resources_*.csv"
# files from your CSV folder (the path set above with the path.CSVs option, by default it's /X4SaveGameAnalysis/data
# under your Documents folder), or set this value to TRUE and run the script once to force the resource cache to
# refresh, and then set it back to FALSE again.
cache.forceRefresh <- FALSE

# compress the cache files to gzip to save some disk space
cache.compress <- TRUE

# image type used. I recommend "svg" for best results but you can change this to "png" or "jpg" if you prefer
graphs.imagetype <- "svg"

# these values should give good graph sizes for a 1920x1080 screen for both vector and raster image types
# better not mess with them unless your bar plots are way too small/large
# These do not affect the Map, Sunburst plots and tables
graphs.width <- 16
graphs.height <- 9
graphs.units <- "in"      # valid values: "in", "cm", "mm", "px"
graphs.dpi <- 96          # only meaningful for raster image types


# HERE BE DRAGONS!
# This is as far as you need to go if things work
# If they don't and you don't have any experience with R programming it's unlikely you'll be able to fix it
# Some knowledge of XQuery and regular expressions is also advisable
# Changing anything below this line may break the script and will void your already non-existent warranty
# However, this is also where you need to be to experiment, learn, and add or change stuff
# Choices, choices... and backups


# PACKAGES USED
# You can install these manually from RStudio's [Tools] -> [Install Packages] menu if you prefer
# However the script will check if they are installed and should automatically download and install them if not
#
# XML         (for XML parsing)
# stringr     (string utilities)
# plyr        (set operations)
# dplyr       (set operations)
# reshape2    (set operations)
# DT          (html data tables)
# ggplot2     (bar/line/area plots)
# ggpubr      (additional ggplot utilities)
# plotly      (map and sunburst plots)
# scales      (graph colours)
# dichromat   (graph colours)
# htmlwidgets (utilities for html output)
# htmltools   (utilities for html output)
# svglite     (svg image output)
# utils       (general R utilities)

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Loading (and installing if necessary) required packages"))
requiredPackages <- c("XML", "stringr", "plyr", "dplyr", "reshape2", "DT", "ggplot2", "scales", "htmlwidgets", "htmltools", "dichromat",
                      "plotly", "ggpubr","svglite","utils")
for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) { install.packages(p) }
  library(p, character.only = TRUE)
}

# Initializing helper variables and functions
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Initializing helper variables and functions"))

movingAverage <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }

mixedRainbow <- function(n) { rainbow(n)[unlist(sapply(1:(reps <- ceiling(n/6)), "seq.int", n, reps))] }

fillNAs <- function(values) {
  idx <- !is.na(values)
  return(c(NA, values[idx])[cumsum(idx)+1])
}

Propercase <- function(value) { return(paste0(toupper(substring(value, 1, 1)), substring(value, 2))) }

# Faction names, colours etc
factions.levels <-  c("PLA", "ARG", "ANT", "TEL", "MIN", "HOP",
                      "PAR", "ALI", "HAT", "SCA", "ZYA", "FRF",
                      "FAF", "XEN", "KHK", "PIO", "BUC", "RIP",
                      "TER", "VIG", "YAK", "NIL", "BOR")
factions.names <-   c("Player","Argon Federation","Antigone Republic","Teladi Company","Ministry of Finance","Holy Order of the Pontifex",
                      "Godrealm of the Paranid","Alliance of the Word","Hatikvah Free League","Scale Plate Pact","Zyarth Patriarchy","Free Families",
                      "Fallen Families","Xenon","Kha'ak","Segaris Pioneers","Duke's Buccaneers","Riptide Rakers",
                      "Terran Protectorate","Vigour Syndicate","Yaki","Ownerless", "Boron")
factions.colours <- c("#33f23a", "#0450f2", "#4c91d3", "#a2b927", "#8eb48c", "#f26ca5",
                      "#7a03f2", "#aa37c2", "#1beaf0", "#7e7732", "#fc691a", "#f19600",
                      "#ff4848", "#c10200", "lightpink", "#39ad9b", "#5500f4", "#5683a3",
                      "#bdd2fb", "#988397", "#fe8ffa", "#808080", "blue")
sector.owners <-    c("player", "argon", "antigone", "teladi", "ministry", "holyorder",
                      "paranid", "alliance", "hatikvah", "scaleplate", "split", "freesplit",
                      "fallensplit", "xenon", "khaak", "pioneers", "buccaneers", "scavenger",
                      "terran", "loanshark", "yaki", "ownerless", "boron")

wares.levels <- c("advancedcomposites",
                  "advancedelectronics",
                  "antimattercells",
                  "antimatterconverters",
                  "bofu",
                  "bogas",
                  "cheltmeat",
                  "claytronics",
                  "computronicsubstrate",
                  "dronecomponents",
                  "energycells",
                  "engineparts",
                  "fieldcoils",
                  "foodrations",
                  "graphene",
                  "helium",
                  "hullparts",
                  "hydrogen",
                  "ice",
                  "majasnails",
                  "meat",
                  "medicalsupplies",
                  "metallicmicrolattice",
                  "methane",
                  "microchips",
                  "missilecomponents",
                  "nividium",
                  "nostropoil",
                  "ore",
                  "plankton",
                  "plasmaconductors",
                  "quantumtubes",
                  "rawscrap",
                  "refinedmetals",
                  "scanningarrays",
                  "scruffinfruits",
                  "shieldcomponents",
                  "silicon",
                  "siliconcarbide",
                  "siliconwafers",
                  "smartchips",
                  "sojabeans",
                  "sojahusk",
                  "spices",
                  "sunriseflowers",
                  "superfluidcoolant",
                  "teladianium",
                  "turretcomponents",
                  "water",
                  "weaponcomponents",
                  "wheat")
wares.names <- c("Advanced Composites",
                 "Advanced Electronics",
                 "Antimatter Cells",
                 "Antimatter Converters",
                 "Bofu",
                 "Bo-gas",
                 "Chelt Meat",
                 "Claytronics",
                 "Computronic Substrate",
                 "Drone Components",
                 "Energy Cells",
                 "Engine Parts",
                 "Field Coils",
                 "Food Rations",
                 "Graphene",
                 "Helium",
                 "Hull Parts",
                 "Hydrogen",
                 "Ice",
                 "Maja Snails",
                 "Meat",
                 "Medical Supplies",
                 "Metallic Microlattice",
                 "Methane",
                 "Microchips",
                 "Missile Components",
                 "Nividium",
                 "Nostrop Oil",
                 "Ore",
                 "Plankton",
                 "Plasma Conductors",
                 "Quantum Tubes",
                 "Raw Scrap",
                 "Refined Metals",
                 "Scanning Arrays",
                 "Scruffin Fruit",
                 "Shield Components",
                 "Silicon",
                 "Silicon Carbide",
                 "Silicon Wafers",
                 "Smart Chips",
                 "Soja Beans",
                 "Soja Husk",
                 "Spices",
                 "Sunrise Flowers",
                 "Superfluid Coolant",
                 "Teladianium",
                 "Turret Components",
                 "Water",
                 "Weapon Components",
                 "Wheat")

# factor for ship sizes
shipsize.levels <- c("XS", "S", "M", "L", "XL", "XXL")

# setting some options for DT and data.frame
opt <- options(stringsAsFactors = FALSE, DT.options = list(lengthChange = FALSE, paging = TRUE, searching = FALSE))

# Creating output path
if (!dir.exists(paste0(path.Output, "/files"))) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Creating output path:", path.Output))
  dir.create(paste0(path.Output, "/files"), recursive = TRUE)
}

# Reading ship model info from csv
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Reading ship model info from csv"))
df.shipdata <- read.table(file = paste0(path.CSVs, "/X4_ship_models.csv"), sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
for (c in c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) {
  df.shipdata[,c] <- as.numeric(df.shipdata[,c])
}

# Reading cluster & sector info from csv
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Reading cluster & sector info from csv"))
df.clusterdata <- read.csv(file = paste0(path.CSVs, "/X4_clusters.csv"))
df.sectordata <- read.csv(file = paste0(path.CSVs, "/X4_sectors.csv"))

# Reading special names from csv
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Reading special names from csv"))
df.namedata <- read.table(file = paste0(path.CSVs, "/X4_special_names.csv"), sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")

# palette for non-faction graphs (sorry, I'm colourblind, this probably looks horrible)
plot.palette <- c(dichromat_pal("BrowntoBlue.10")(10)[c(1,2,3,4,5,6,8,10)],
                  dichromat_pal("BluetoGreen.14")(14)[c(1,2,3,5,6,10,12,14)],
                  dichromat_pal("GreentoMagenta.16")(16)[c(10,12,14,15,16)])


# Let's do this!

# find savegame
if (savegame.override != "") {
  if (!file.exists(savegame.override)) {
    stop("savegame.override option set to non-existant file")
  } else {
    save.file <- savegame.override
  }
} else {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding most recent savegame"))
  # path to save files determined automatically
  save.path <- paste0((l <- list.dirs(path.X4user))[which(str_detect(l, paste0("/Egosoft/X4/[0-9]+/save$")))], "/")
  df.files <- data.frame(filename = dir(save.path, ".*[.]xml[.]gz$|.*[.]xml$"), modify_date = file.mtime(paste0(save.path, dir(save.path, ".*[.]xml[.]gz$|.*[.]xml$"))))
  if (any(str_detect(df.files$filename, "temp"))) {
    stop("Game is saving, try again in a minute.")
  }
  save.file <- paste0(save.path, df.files[first(order(df.files$modify_date, decreasing = TRUE)), 1])
}

# parse savegame xml tree
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Reading savegame:", save.file))
result <- xmlParse(file = save.file, addFinalizer = TRUE)

# find game GUID
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing Game GUID, custom names"))
game.guid <- unlist(xpathSApply(result, "/savegame/info/game", xmlGetAttr, "guid"))
game.factionname <- unlist(xpathSApply(result, "/savegame/universe/factions/faction[@id='player']/custom/name", xmlGetAttr, "name"))
if (!is.null(game.factionname)) { factions.names[1] <- game.factionname }
game.playername <- unlist(xpathSApply(result, "/savegame/info/player", xmlGetAttr, "name"))
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Found Game GUID:", game.guid))
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Found player name:", game.playername))
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Found player faction name:", factions.names[1]))

cache.resources <- paste0(path.CSVs, "/cache_resources_", game.guid, ".csv")
cache.resources.gz <- paste0(cache.resources, ".gz")
cache.log <- paste0(path.CSVs, "/cache_log_", game.guid, ".csv")
cache.log.gz <- paste0(cache.log, ".gz")
cache.tradelog <- paste0(path.CSVs, "/cache_tradelog_", game.guid, ".csv")
cache.tradelog.gz <- paste0(cache.tradelog, ".gz")

# checking if cache compression changed
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Checking cache compression changes"))
if (cache.compress && file.exists(cache.resources)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Compressing resource cache"))
  df.cache <- read.table(file = cache.resources, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- gzfile(cache.resources.gz)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.resources)
} else if (!cache.compress && file.exists(cache.resources.gz)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Decompressing resource cache"))
  df.cache <- read.table(file = cache.resources.gz, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- file(cache.resources)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.resources.gz)
}
if (cache.compress && file.exists(cache.log)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Compressing log cache"))
  df.cache <- read.table(file = cache.log, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- gzfile(cache.log.gz)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.log)
} else if (!cache.compress && file.exists(cache.log.gz)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Decompressing log cache"))
  df.cache <- read.table(file = cache.log.gz, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- file(cache.log)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.log.gz)
}
if (cache.compress && file.exists(cache.tradelog)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Compressing tradelog cache"))
  df.cache <- read.table(file = cache.tradelog, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- gzfile(cache.tradelog.gz)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.tradelog)
} else if (!cache.compress && file.exists(cache.tradelog.gz)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Decompressing tradelog cache"))
  df.cache <- read.table(file = cache.tradelog.gz, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  cache.file <- file(cache.tradelog)
  write.table(df.cache, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
  file.remove(cache.tradelog.gz)
}
if (cache.compress) {
  cache.resources <- cache.resources.gz
  cache.log <- cache.log.gz
  cache.tradelog <- cache.tradelog.gz
}

# find clusters, sectors, stations and ships
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing Universe"))
df.universe <- ldply(xpathSApply(result, "/savegame/universe//component[@class='station' or contains(@class,'ship') or @class='sector' or @class='cluster']", xmlAttrs), "rbind", .id = NULL)
df.universe <- df.universe[-which(is.na(df.universe$connection)),]
row.names(df.universe) <- NULL

# adding cluster and sector IDs to their child nodes
idx.clusters <- which(df.universe$class == "cluster")
df.universe[idx.clusters, "cluster.id"] <- df.universe$id[idx.clusters]
df.universe[idx.clusters, "cluster.macro"] <- df.universe$macro[idx.clusters]
idx.sectors <- which(df.universe$class == "sector")
df.universe[idx.sectors, "sector.id"] <- df.universe$id[idx.sectors]
df.universe[idx.sectors, "sector.macro"] <- df.universe$macro[idx.sectors]
df.universe$cluster.id <- fillNAs(df.universe$cluster.id)
df.universe$cluster.macro <- fillNAs(df.universe$cluster.macro)
df.universe$sector.id <- fillNAs(df.universe$sector.id)
df.universe$sector.macro <- fillNAs(df.universe$sector.macro)

# prepare sector info (ownership, contested, resources)
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing sector info -> df.sectors"))
df.sectors <- df.universe[which(df.universe$class == 'sector'),]
if (!("contested" %in% colnames(df.sectors))) { df.sectors$contested <- NA }
df.sectors <- left_join(df.sectors[,c("id","macro","code","owner","knownto","contested","sector.id","sector.macro","cluster.id","cluster.macro")], df.sectordata[,c("macro","name","race")], by = c("sector.macro" = "macro"))
df.sectors$name[is.na(df.sectors$name)] <- df.sectors$sector.macro[is.na(df.sectors$name)]

# read resource cache or generate it if it doesn't exist yet
if ((!file.exists(cache.resources)) || (cache.forceRefresh)) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing sector resource info"))
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "NOTE: This may take several minutes but will only need to run once after you start a new game or after installing DLC"))
  
  ns.sectors <- xpathApply(result, "/savegame/universe//component[@class='sector']")
  idx <- 1
  df.resourceareas <- data.frame()
  while (idx <= length(ns.sectors)) {
    df.temp2 <- ldply(xpathSApply(ns.sectors[[idx]], ".|resourceareas/descendant::node()", xmlAttrs, addFinalizer = TRUE), "rbind", .id = NULL)
    if (ncol(df.temp2) > 1) {
      df.temp <- df.temp2[,c("macro","ware","max","time")]
      df.temp$max <- as.numeric(df.temp$max)
      df.temp$time <- as.numeric(df.temp$time)
      df.temp$macro <- fillNAs(df.temp$macro)
      df.temp$ware <- fillNAs(df.temp$ware)
      df.temp2 <- df.temp[(!is.na(df.temp$max) | !is.na(df.temp$time)), c("macro","ware","max","time")]
      df.resourceareas <- rbind(df.resourceareas, df.temp2)
    }
    idx <- idx + 1
  }
  df.resourceareas$recharge <- df.resourceareas$max / df.resourceareas$time
  df.temp2 <- dcast(df.resourceareas[,c("macro","ware","recharge")], macro~ware, sum, value.var = "recharge")
  rm(df.resourceareas)
  
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Saving resource info to cache file:", cache.resources))
  if (cache.compress) {
    cache.file <- gzfile(cache.resources)
  } else {
    cache.file <- file(cache.resources)
  }
  write.table(df.temp2, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
} else {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing sector resource info -> found resource cache file, reading", cache.resources))
  df.temp2 <- read.table(file = cache.resources, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  for (cl in colnames(df.temp2)[-1]) { df.temp2[,cl] <- as.numeric(df.temp2[,cl]) }
}  
df.sectors <- left_join(df.sectors, df.temp2, by = "macro")

# find all player owned objects
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing player owned objects -> df.playerowned"))
df.playerowned <- df.universe[which(df.universe$owner == 'player' & !(df.universe$class %in% c("sector","cluster"))), c("id","class","name","code","macro","connection","spawntime","thruster","sector.id")]
df.playerowned$spawntime <- as.numeric(as.character(df.playerowned$spawntime))

# figure out fleet hierarchies... this was a head ache, it works but can probably be done better
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing fleet hierarchies -> df.wings"))
df.temp <- ldply(xpathSApply(result,
                             "/savegame/universe//component[(@class='station' or contains(@class,'ship')) and @owner='player']",
                             function(subordinate) { c("follower" = xmlGetAttr(subordinate, "id"), "connection.id" = xpathSApply(subordinate, "connections/connection[@connection='commander']/connected", xmlGetAttr, "connection")) }
                            ), "rbind")
rm(df.temp2)
try(df.temp2 <- melt(ldply(xpathSApply(result,
                                       "/savegame/universe//component[(@class='station' or contains(@class,'ship')) and @owner='player']",
                                       function(commander) { c("leader" = xmlGetAttr(commander, "id"), "connection.id" = unlist(xpathSApply(commander, "connections/connection[@connection='subordinates']", xmlGetAttr, "id"))) }
                                      ), "rbind", .id = NULL), id = "leader")[,c("leader","value")], silent = TRUE)
if (!identical(find("df.temp2"), character(0))) {
  colnames(df.temp2)[2] <- "connection.id"
  df.wings <- inner_join(df.temp2, df.temp[,c(2,1)], by = "connection.id", na_matches = "never")[,c(1,3)]
}

# find all NPCs employed by the player
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing player employed NPCs -> df.npcs"))
df.temp <- data.frame()

# df.npcs <- try({df.temp <- as.data.frame(t(xpathSApply(result, "/savegame/universe//component[@class='npc' and @owner='player']", xmlAttrs, addFinalizer = TRUE)))
#                 df.temp[,c("id","name",intersect(colnames(df.temp),c("code")))] })
df.temp <- as.data.frame(
  do.call(rbind, lapply(
    xpathSApply(result, "/savegame/universe//component[@class='npc' and @owner='player']", xmlAttrs, addFinalizer = TRUE),
    function(x) x[!(names(x) %in% c("lastspeaktime"))]
  ))
)

df.npcs <- df.temp[,c("id","name",intersect(colnames(df.temp),c("code")))]

if (is.data.frame(df.npcs)) {
  df.temp <- df.npcs[, c("id"), drop = FALSE]
  df.temp$numSkills <- xpathSApply(result, "/savegame/universe//component[@class='npc' and @owner='player']/traits", xmlSize, addFinalizer = TRUE)
  df.temp2 <- as.data.frame(t(xpathSApply(result, "/savegame/universe//component[@class='npc' and @owner='player']/traits/skill[@value]", xmlAttrs, addFinalizer = TRUE)))
  df.temp2$value <- as.integer(df.temp2$value)
  df.temp <- dcast(cbind(df.temp[rep(row(df.temp[1]), df.temp$numSkills), "id", drop = FALSE], df.temp2), id ~ type)
  df.npcs <- inner_join(df.npcs, df.temp, by = "id")  #[,c("id","name","code","piloting","engineering","boarding","management","morale")]
  df.npcs[is.na(df.npcs)] <- 0
} else {
  rm(df.npcs)
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "-> No NPCs found"))
}

# gather the log
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing log entries -> df.log"))
df.log <- ldply(xpathSApply(result, "/savegame/log/entry[boolean(@category)=false or (@category='upkeep' and @title!='Trade Completed')]", xmlAttrs, addFinalizer = TRUE), "rbind", .id = NULL)
df.log$time <- as.numeric(as.character(df.log$time))
df.log$money <- as.numeric(as.character(df.log$money))
df.log$category[is.na(df.log$category)] <- ""
df.log <- unique(df.log[,c("time","category","title","text","money","component")])

# read old log entries from cache and update the cache with the new entries
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Loading and merging log cache"))
if (file.exists(cache.log)) {
  df.cache <- read.table(file = cache.log, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  df.cache <- df.cache[which(df.cache$category == "" | (df.cache$category == "upkeep" & df.cache$title != "Trade Completed")),]
  df.cache$time <- as.numeric(df.cache$time)
  df.cache$money <- as.numeric(df.cache$money)
  df.cache$category[is.na(df.cache$category)] <- ""
  mintime.categories <- aggregate(df.log[, "time", drop = FALSE], by = list(category = df.log$category), FUN = "min")
  for (idx in row(mintime.categories[1])) { df.cache <- df.cache[-which(df.cache$category == mintime.categories$category[idx] & df.cache$time >= mintime.categories$time[idx]),] }
  df.log <- unique(rbind(df.cache, df.log))
}
if (cache.compress) {
  cache.file <- gzfile(cache.log)
} else {
  cache.file <- file(cache.log)
}
write.table(df.log, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")

# find various info on stations (station personel, population, nr of modules, estimate mass and hull)
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing player owned stations -> df.stations"))
df.stations <- df.playerowned[which(df.playerowned$class == "station"),]
if (NROW(df.stations) > 0) {
  df.stations$connection <- NULL
  df.stations$thruster <- NULL
  df.stations$name <- as.factor(df.stations$name)
  df.temp <- ldply(xpathSApply(result, "/savegame/universe//component[@class='station']",
                               function(station) { unlist(setNames(c("id" = xmlGetAttr(station, "id"),
                                                                     "post" = xpathSApply(station, "control/post", xmlGetAttr, "component"),
                                                                     "amount" = xpathSApply(station, "workforces/workforce", xmlGetAttr, "amount")),
                                                                   c("id", xpathSApply(station, "control/post", xmlGetAttr, "id"),
                                                                     xpathSApply(station, "workforces/workforce", xmlGetAttr, "race"))) ) }
                              ), "rbind")
  cols.workforce <- c()
  cols.managers <- c()
  for (idx in 2:ncol(df.temp)) {
    if (identical(str_which(df.temp[[idx]], "[^0-9]"), integer(0))) {
      colnames(df.temp)[idx] <- paste0("workforce.", colnames(df.temp)[idx])
      df.temp[,idx] <- as.integer(df.temp[,idx])
      cols.workforce <- c(cols.workforce, idx)
    } else {
      colnames(df.temp)[idx] <- paste0(colnames(df.temp)[idx], ".id")
      cols.managers <- c(cols.managers, idx)
    }
  }
  df.stations <- left_join(df.stations, df.temp[,c(1, cols.managers, cols.workforce)], by = "id")
  df.stations$manager.id <- as.factor(df.stations$manager.id)
  
  df.temp <- ldply(xpathSApply(result, "/savegame/universe//component[@class='station']",
                               function(station) { c("id" = xmlGetAttr(station, "id"),
                                                     "index" = xpathSApply(station, "construction/sequence/entry", xmlGetAttr, "index")) }
                              ), "bind_rows")
  df.temp2 <- melt(df.temp, id.vars = "id")[,c(1,3)]
  df.temp2$value <- as.numeric(df.temp2$value)
  df.temp <- setNames(aggregate(df.temp2$value, by = list(df.temp2$id), FUN = "max", na.rm = TRUE), c("id","modules"))
  df.stations <- left_join(df.stations, df.temp, by = "id")
  df.universe <- left_join(df.universe, df.temp, by = "id")
  df.stations$hull <- df.stations$modules * 250000
  df.universe$hull[df.universe$class == "station"] <- df.universe$modules[df.universe$class == "station"] * 250000
  df.stations$mass <- df.stations$hull / 300
  df.universe$mass[df.universe$class == "station"] <- df.universe$hull[df.universe$class == "station"] / 300
} else {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "-> No player owned stations found"))
}

# find info on ships (name, pilot, crew, hull, etc)
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing player owned ships -> df.ships"))
df.ships <- df.playerowned[which(df.playerowned$class != "station"),]
df.ships$size <- factor(toupper(str_match(df.ships$class, "ship_(.+)")[,2]), levels = shipsize.levels, ordered = TRUE)
df.ships$model <- as.factor(df.shipdata$model[match(df.ships$macro, df.shipdata$macro)])
df.ships$name[is.na(df.ships$name)] <- as.character(df.ships$model[is.na(df.ships$name)])
df.ships$name[is.na(df.ships$name)] <- as.character(df.ships$macro[is.na(df.ships$name)])
df.ships$name <- as.factor(df.ships$name)
df.temp <- ldply(xpathSApply(result, "/savegame/universe//component[contains(@class,'ship')]",
                             function(ship) { unlist(setNames(c("id" = xmlGetAttr(ship, "id"), "post" = xpathSApply(ship, "control/post", xmlGetAttr, "component")),
                                                              c("id", xpathSApply(ship, "control/post", xmlGetAttr, "id")))) }
                            ), "rbind")
df.ships <- left_join(df.ships, df.temp, by = "id")

if (!identical(find("df.npcs"), character(0))) {
  df.ships <- left_join(df.ships, setNames(df.npcs[,c("id","name","piloting")], c("id","pilot.name","pilot.skill")), by = c("aipilot" = "id"))
  # add employment info to NPCs
  df.npcs[df.npcs$id %in% df.ships$aipilot, "role"] <- "pilot (ship)"
  df.npcs[df.npcs$id %in% df.ships$engineer, "role"] <- "engineer (ship)"
  df.npcs[df.npcs$id %in% df.stations$manager.id, "role"] <- "manager (station)"
  df.npcs[df.npcs$id %in% df.stations$engineer.id, "role"] <- "engineer (station)"
  df.npcs[df.npcs$id %in% df.stations$shiptrader.id, "role"] <- "shiptrader (station)"
}

# adding ship names to df.playerowned too, need them later
idx <- which(df.playerowned$class != "station" & is.na(df.playerowned$name))
df.playerowned$name[idx] <- df.shipdata$model[match(df.playerowned$macro[idx], df.shipdata$macro)]
idx <- which(df.playerowned$class != "station" & is.na(df.playerowned$name))
df.playerowned$name[idx] <- as.character(df.playerowned$macro[idx])

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Parsing economylog -> df.tradelog"))
# get trades from economylog
df.tradelog <- ldply(xpathSApply(result, "/savegame/economylog/entries[@type='trade']/log", xmlAttrs), "rbind")
# get referenced removed objects
df.temp <- ldply(xpathSApply(result, "/savegame/economylog/removed/object", xmlAttrs), "rbind")
# add name/code/owner for removed objects
df.tradelog[,c("seller.name","seller.code","seller.owner")] <- df.temp[match(df.tradelog$seller, df.temp$id), c("name","code","owner")]

# determine proxy sellers (subordinate traders)
idx <- which(df.tradelog$seller %in% df.wings$follower)
df.tradelog[idx, "seller.proxy.id"] <- df.tradelog$seller[idx]
# set seller to commander of proxy
df.tradelog$seller[idx]<- df.wings$leader[match(df.tradelog$seller[idx], df.wings$follower)]
# add name/code/owner for player owned seller objects
idx <- which(is.na(df.tradelog$seller.name) & df.tradelog$seller %in% df.playerowned$id)
df.tradelog[idx, c("seller.name","seller.code")] <- df.playerowned[match(df.tradelog$seller[idx], df.playerowned$id), c("name","code")]
df.tradelog$seller.owner[idx] <- "player"
# add name/code/owner for NPC owned seller objects
idx <- which(is.na(df.tradelog$seller.name))
df.tradelog[idx, c("seller.name","seller.code","seller.owner")] <- df.universe[match(df.tradelog$seller[idx], df.universe$id), c("name","code","owner")]
# construct name from faction code and ship model name
idx <- which(is.na(df.tradelog$seller.name))
df.tradelog$seller.name[idx] <- paste(factions.levels[match(df.tradelog$seller.owner[idx], sector.owners)], df.shipdata$model[match(df.universe$macro[match(df.tradelog$seller.code[idx], df.universe$code)], df.shipdata$macro)] )
# if it wasn't a ship it must have been a station
idx <- intersect(idx, which(str_detect(df.tradelog$seller.name, "^.{3} NA$")))
df.tradelog$seller.name[idx] <- sub("NA", "Station", df.tradelog$seller.name[idx], fixed = TRUE)
# adding names of special objects (story stations etc)
idx <- which(df.tradelog$seller.name %in% df.namedata$id)
df.tradelog$seller.name[idx] <- df.namedata$name[match(df.tradelog$seller.name[idx], df.namedata$id)]
# add seller proxy name/code
idx <- which(df.tradelog$seller.proxy.id %in% df.playerowned$id)
df.tradelog[idx, c("seller.proxy.name","seller.proxy.code")] <- df.playerowned[match(df.tradelog$seller.proxy.id[idx], df.playerowned$id), c("name","code")]

# same exercise once more for buyer & buyer proxy
df.tradelog[,c("buyer.name","buyer.code","buyer.owner")] <- df.temp[match(df.tradelog$buyer, df.temp$id), c("name","code","owner")]

idx <- which(df.tradelog$buyer %in% df.wings$follower)
df.tradelog[idx, "buyer.proxy.id"] <- df.tradelog$buyer[idx]
df.tradelog$buyer[idx]<- df.wings$leader[match(df.tradelog$buyer[idx], df.wings$follower)]

idx <- which(is.na(df.tradelog$buyer.name) & df.tradelog$buyer %in% df.playerowned$id)
df.tradelog[idx, c("buyer.name","buyer.code")] <- df.playerowned[match(df.tradelog$buyer[idx], df.playerowned$id), c("name","code")]
df.tradelog$buyer.owner[idx] <- "player"
idx <- which(is.na(df.tradelog$buyer.name))
df.tradelog[idx, c("buyer.name","buyer.code","buyer.owner")] <- df.universe[match(df.tradelog$buyer[idx], df.universe$id), c("name","code","owner")]
idx <- which(is.na(df.tradelog$buyer.name))
df.tradelog$buyer.name[idx] <- paste(factions.levels[match(df.tradelog$buyer.owner[idx], sector.owners)], df.shipdata$model[match(df.universe$macro[match(df.tradelog$buyer.code[idx], df.universe$code)], df.shipdata$macro)] )
idx <- intersect(idx, which(str_detect(df.tradelog$buyer.name, "^.{3} NA$")))
df.tradelog$buyer.name[idx] <- sub("NA", "Station", df.tradelog$buyer.name[idx], fixed = TRUE)
idx <- which(df.tradelog$buyer.name %in% df.namedata$id)
df.tradelog$buyer.name[idx] <- df.namedata$name[match(df.tradelog$buyer.name[idx], df.namedata$id)]

idx <- which(df.tradelog$buyer.proxy.id %in% df.playerowned$id)
df.tradelog[idx, c("buyer.proxy.name","buyer.proxy.code")] <- df.playerowned[match(df.tradelog$buyer.proxy.id[idx], df.playerowned$id), c("name","code")]

# housekeeping
df.tradelog$seller.faction <- factor(factions.levels[match(df.tradelog$seller.owner, sector.owners)], levels = factions.levels, ordered = TRUE)
df.tradelog$buyer.faction <- factor(factions.levels[match(df.tradelog$buyer.owner, sector.owners)], levels = factions.levels, ordered = TRUE)
df.tradelog$time <- as.numeric(df.tradelog$time)
df.tradelog$price <- as.numeric(df.tradelog$price) / 100.0
df.tradelog$v <- as.integer(df.tradelog$v)
df.tradelog$money <- as.integer(df.tradelog$price * df.tradelog$v)
df.tradelog$ware <- factor(df.tradelog$ware, levels = wares.levels, ordered = TRUE)
colnames(df.tradelog)[match(c("ware","buyer","seller","v"), colnames(df.tradelog))] <- c("commodity","buyer.id","seller.id","amount")
df.tradelog <- df.tradelog[,c("time","commodity","price","amount","money",
                              "seller.faction","seller.id","seller.name","seller.code","seller.proxy.id","seller.proxy.name","seller.proxy.code",
                              "buyer.faction","buyer.id","buyer.name","buyer.code","buyer.proxy.id","buyer.proxy.name","buyer.proxy.code")]
# that wasn't so hard was it? better save this to a cache
# read old log entries from cache and update the cache with the new entries
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Loading and merging tradelog cache"))
if (file.exists(cache.tradelog)) {
  df.cache <- read.table(file = cache.tradelog, sep = "\t", row.names = NULL, header = TRUE, colClasses = "character")
  df.cache$time <- as.numeric(df.cache$time)
  df.cache$price <- as.numeric(df.cache$price)
  df.cache$amount <- as.integer(df.cache$amount)
  df.cache$money <- as.integer(df.cache$money)
  df.cache$seller.faction <- factor(df.cache$seller.faction, levels = factions.levels, ordered = TRUE)
  df.cache$buyer.faction <- factor(df.cache$buyer.faction, levels = factions.levels, ordered = TRUE)
  df.cache$commodity <- factor(df.cache$commodity, levels = wares.levels, ordered = TRUE)
  mintime <- min(df.tradelog$time)
  df.cache <- df.cache[-which(df.cache$time > mintime),]
  df.tradelog <- unique(rbind(df.cache, df.tradelog))
}
if (cache.compress) {
  cache.file <- gzfile(cache.tradelog)
} else {
  cache.file <- file(cache.tradelog)
}
write.table(df.tradelog, file = cache.file, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")
#write.table(df.tradelog, file = cache.tradelog, sep = "\t", row.names = FALSE, col.names = TRUE, qmethod = "double")

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Gathering sales -> df.sales"))
df.sales <- df.tradelog[which(df.tradelog$seller.faction == "PLA" & df.tradelog$buyer.faction != "PLA"), c("time","money","seller.name","seller.code","amount","commodity","buyer.faction","buyer.name","buyer.code")]
df.sales$commodity <- as.character(df.sales$commodity)

# find ship constructions
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding ship constructions -> df.sales"))
df.temp <- df.log[which(df.log$category == "upkeep" & df.log$title == "Ship constructed"),]
if (NROW(df.temp) > 0) {
  df.temp[,c("wares","parse2")] <- str_split(df.temp$text, fixed(" finished construction at station: "), 2, TRUE)
  df.temp[,c("seller","parse3")] <- str_split(df.temp$parse2, fixed(". They have paid"), 2, TRUE)
  df.temp$seller.name <- str_split(df.temp$seller, " [(][A-Z]{3}-[0-9]{3}[)]", 2, TRUE)[,1]
  df.temp$seller.code <- str_extract(df.temp$seller, "[A-Z]{3}-[0-9]{3}")
  df.temp$amount <- 1
  df.temp$commodity <- "Ship construction"
  df.temp[,c("buyer.faction","buyer.name")] <- str_split(df.temp$wares, fixed(" "), 2, TRUE)
  df.temp$buyer.code <- str_extract(df.temp$buyer.name, "[A-Z]{3}-[0-9]{3}")
  df.temp$buyer.name <- str_split(df.temp$buyer.name, " [(]*[A-Z]{3}-[0-9]{3}[)]*", 2, TRUE)[,1]
  df.temp$location <- NA
  df.temp$money <- floor(df.temp$money / 100.0)
  df.sales <- rbind(df.sales, df.temp[,colnames(df.sales)])
}

# find ship repairs
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding ship repairs -> df.sales"))
df.temp <- df.log[which(df.log$category == "upkeep" & df.log$title == "Ship repaired"),]
if (NROW(df.temp) > 0) {
  df.temp[,c("wares","parse2")] <- str_split(df.temp$text, fixed(" finished repairing at station: "), 2, TRUE)
  df.temp[,c("seller","parse3")] <- str_split(df.temp$parse2, fixed(". They have paid"), 2, TRUE)
  df.temp$seller.name <- str_split(df.temp$seller, " [(][A-Z]{3}-[0-9]{3}[)]", 2, TRUE)[,1]
  df.temp$seller.code <- str_extract(df.temp$seller, "[A-Z]{3}-[0-9]{3}")
  df.temp$amount <- 1
  df.temp$commodity <- "Ship repair"
  df.temp[,c("buyer.faction","buyer.name")] <- str_split(df.temp$wares, fixed(" "), 2, TRUE)
  df.temp$buyer.code <- str_extract(df.temp$buyer.name, "[A-Z]{3}-[0-9]{3}")
  df.temp$buyer.name <- str_split(df.temp$buyer.name, " [(]*[A-Z]{3}-[0-9]{3}[)]*", 2, TRUE)[,1]
  df.temp$money <- floor(df.temp$money / 100.0)
  df.temp$location <- NA
  df.sales <- rbind(df.sales, df.temp[,colnames(df.sales)])
}

# find ship resupplies
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding ship resupplies -> df.sales"))
df.temp <- df.log[which(df.log$category == "upkeep" & df.log$title == "Ship resupplied"),]
if (NROW(df.temp) > 0) {
  df.temp[,c("wares","parse2")] <- str_split(df.temp$title, fixed(" finished resupplying at station: "), 2, TRUE)
  df.temp[,c("seller","parse3")] <- str_split(df.temp$parse2, fixed(". They have paid"), 2, TRUE)
  df.temp$seller.name <- str_split(df.temp$seller, " [(][A-Z]{3}-[0-9]{3}[)]", 2, TRUE)[,1]
  df.temp$seller.code <- str_extract(df.temp$seller, "[A-Z]{3}-[0-9]{3}")
  df.temp$amount <- 1
  df.temp$commodity <- "Ship resupply"
  df.temp[,c("buyer.faction","buyer.name")] <- str_split(df.temp$wares, fixed(" "), 2, TRUE)
  df.temp$buyer.code <- str_extract(df.temp$buyer.name, "[A-Z]{3}-[0-9]{3}")
  df.temp$buyer.name <- str_split(df.temp$buyer.name, " [(]*[A-Z]{3}-[0-9]{3}[)]*", 2, TRUE)[,1]
  df.temp$location <- NA
  df.temp$money <- floor(df.temp$money / 100.0)
  df.sales <- rbind(df.sales, df.temp[,colnames(df.sales)])
}

# some housekeeping on the sales table
if (!identical(find("df.sales"), character(0))) {
  df.sales <- df.sales[order(df.sales$time, decreasing = TRUE),]
  # adding names for ships/stations that still have default name
  # df.temp <- df.playerowned[!is.na(df.playerowned$name), c("code","name")]
  # df.sales$seller.name[is.na(df.sales$seller.name)] <- df.temp$name[match(df.sales$seller.code[is.na(df.sales$seller.name)], df.temp$code)]
  # df.sales$buyer.faction <- factor(as.character(df.sales$buyer.faction), levels = factions.levels, ordered = TRUE)
  df.sales$seller.name <- as.factor(df.sales$seller.name)
  df.sales$seller.code <- as.factor(df.sales$seller.code)
  df.sales$commodity <- as.factor(df.sales$commodity)
  # df.sales$location <- as.factor(df.sales$location)
  # df.sales$money <- as.integer(df.sales$money)
  # df.sales$amount <- as.integer(df.sales$amount)
} else {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "-> No sales found"))
}

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Gathering buys -> df.buys"))
df.buys <- df.tradelog[which(df.tradelog$seller.faction != "PLA" & df.tradelog$buyer.faction == "PLA"), c("time","money","buyer.name","buyer.code","amount","commodity","seller.faction","seller.name","seller.code")]
df.buys$money <- -df.buys$money

# gathering info on destroyed objects
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing destroyed player owned objects -> df.destroyed"))
df.temp <- df.log[which(df.log$category == "upkeep" & str_detect(df.log$title, "was destroyed by")),]
if (NROW(df.temp) > 0) {
  df.temp[,c("object","parse2")] <- str_split(df.temp$title, fixed(" in sector "), 2, TRUE)
  df.temp[,c("location","parse3")] <- str_split(df.temp$parse2, fixed(" was destroyed by "), 2, TRUE)
  df.temp[,c("killer","parse4")] <- str_split(df.temp$parse3, fixed("."), 2, TRUE)
  df.temp$object <- as.factor(df.temp$object)
  df.temp$location <- as.factor(df.temp$location)
  df.temp$killer <- as.factor(df.temp$killer)
  df.destroyed <- df.temp[,c("time","object","location","killer")]
}

# gathering account transfers
# Note: I think the exact text of the log entry changed between v4 and v5, checking both texts to be sure
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Preparing station manager account transfers -> df.transfers"))
df.transfers <- NULL
df.temp <- df.log[which(df.log$category == "upkeep" & str_detect(df.log$title, "Received surplus of")),]
if ((NROW(df.temp) > 0) && (!identical(find("df.npcs"), character(0)))) {
  df.temp[, c("money","manager.name")] <- str_split(df.temp$title, "( of )|( Credits from )", 3, TRUE)[,2:3]
  df.temp$manager.name <- substr(df.temp$manager.name, 1, nchar(df.temp$manager.name)-1)
  df.temp$money <- as.numeric(str_replace_all(df.temp$money, fixed(","), ""))
  df.temp <- left_join(left_join(df.temp[,c("time","money","manager.name")],
                                 df.npcs[df.npcs$role == "manager (station)" ,c("name","id")],
                                 by = c("manager.name" = "name") ),
                       df.stations[,c("manager.id","code","name")],
                       by = c("id" = "manager.id"))[,c(1,2,4,5,6)]
  colnames(df.temp)[3:5] <- c("station.id","station.code","station.name")
  df.transfers <- df.temp
}
df.temp <- df.log[which(df.log$category == "upkeep" & str_detect(df.log$title, "Received surplus from")),]
if (NROW(df.temp) > 0) {
  df.temp[, c("station.name")] <- str_split(df.temp$title, "( surplus from )|( in )", 3, TRUE)[,2]
  df.temp <- left_join(df.temp[,c("time","money","station.name")],
                       df.stations[,c("id","code","name")],
                       by = c("station.name" = "name"))[,c(1,2,4,5,3)]
  colnames(df.temp)[3:5] <- c("station.id","station.code","station.name")
  df.temp$money <- df.temp$money / 100
  df.transfers <- rbind(df.transfers, df.temp)
}

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding pirate harassments -> df.pirates"))
df.pirates <- df.log[str_detect(df.log$title, fixed("Pirate Harassment")), c("time","text")]
if (NROW(df.pirates) > 0) {
  df.pirates <- cbind(df.pirates[,"time",drop=FALSE], setNames(as.data.frame(str_split(df.pirates$text, " in |[.]?.{1}\\\\012.{1}", simplify = TRUE))[,c(1,2,4,5)], c("ship","sector.name","pirate","response")))
  df.pirates <- left_join(df.pirates, df.sectors[,c("name","sector.macro")], by = c("sector.name" = "name"))
  df.pirates$ship.code <- str_extract(df.pirates$ship, "[A-Z]{3}-[0-9]{3}$")
  df.pirates$ship.name <- sub(" [A-Z]{3}-[0-9]{3}$", "", df.pirates$ship)
  df.pirates$pirate.code <- str_extract(df.pirates$pirate, "[A-Z]{3}-[0-9]{3}$")
  df.pirates$pirate.faction <- factor(str_extract(df.pirates$pirate, "^[A-Z]{3}"), levels = factions.levels, ordered = TRUE)
  df.pirates$pirate.name <- sub("^[A-Z]{3} ", "", sub(" [A-Z]{3}-[0-9]{3}$", "", df.pirates$pirate))
  df.pirates$response <- factor(sub("^Response: ", "", df.pirates$response))
  df.pirates <- df.pirates[,c("time","ship.name","ship.code","sector.macro","sector.name","pirate.name","pirate.code","pirate.faction","response")]
} else {
  pirates_cols = c("time","ship.name","ship.code","sector.macro","sector.name","pirate.name","pirate.code","pirate.faction","response")
  df.pirates <- data.frame(matrix(nrow = 0, ncol = length(pirates_cols)))
  colnames(df.pirates) = pirates_cols
}


message(paste(format(Sys.time(), "%H:%M:%OS3"), "Finding police interdictions -> df.police"))
df.police <- df.log[str_detect(df.log$title, fixed("Police Interdiction")), c("time","text")]
df.police <- cbind(df.police[,"time",drop=FALSE], setNames(as.data.frame(str_split(df.police$text, " in | by | police to stop |[.]?.{1}\\\\012.{1}", simplify = TRUE))[,c(1,2,4,6)], c("ship","sector.name","faction.name","response")))
df.police <- left_join(df.police, df.sectors[,c("name","sector.macro")], by = c("sector.name" = "name"))
df.police$police.faction <- factor(factions.levels[match(df.police$faction, factions.names)], levels = factions.levels, ordered = TRUE)
df.police$ship.code <- str_extract(df.police$ship, "[A-Z]{3}-[0-9]{3}$")
df.police$ship.name <- sub(" [A-Z]{3}-[0-9]{3}$", "", df.police$ship)
df.police$response <- factor(sub("^Response: ", "", df.police$response))
df.police <- df.police[,c("time","ship.name","ship.code","sector.macro","sector.name","police.faction","response")]


# enough data gathering, let's generate some visuals now
logged_hours <- (max(df.log$time) - min(df.log$time)) / 3600.0
time.now <- max(df.log$time)
if (!identical(find("df.destroyed"), character(0))) { df.destroyed$HoursAgo <- (time.now - df.destroyed$time) / 3600.0 }
history.hours <- 3
time.limit <- time.now - 3600 * history.hours
dashboard.html <- "<html>"

# sector map
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Generating sector map"))
plot.title <- "Sector map"
overlay.hours <- 24
time.overlay <- time.now - 3600 * 24

# aspect ratio calculations
df.plot.clusters <- setNames(df.clusterdata[df.clusterdata$macro %in% df.sectors$cluster.macro, c(2,4,5,1)], c("x","y","name","macro"))
df.plot.clusters <- df.plot.clusters[order(df.plot.clusters$y, df.plot.clusters$x),]
#df.plot.clusters <- setNames(df.clusterdata[order(df.clusterdata$z, df.clusterdata$x),c(2,4,5,1)], c("x","y","name","macro"))
x.div <- 20000000   #min(df.plot.clusters$x[df.plot.clusters$x > 0.0]) * 4.0 / 3.0
y.div <- 17320000   #min(df.plot.clusters$y[df.plot.clusters$y > 0.0]) * 2.0
x.range <- c(-230000000, 200000000)   #range(df.plot.clusters$x) + c(-x.div, x.div)
y.range <- c(-86600000 , 147220000)   #range(df.plot.clusters$y) + c(-y.div, y.div)
if (spoilers.hide) { df.plot.clusters <- df.plot.clusters[which(df.plot.clusters$macro %in% unique(df.sectors$cluster.macro[which(df.sectors$knownto == "player")])),] }

# get sectors and add some plot attributes
df.plot.sectors <- left_join(left_join(df.plot.clusters, df.sectordata[,c("cluster","macro")], by = c("macro" = "cluster"), multiple = "all")[,c(1,2,3,5)], df.sectors, by = c("macro.y" = "macro"))[,c("x","y","owner","contested","name.y","knownto")]
colnames(df.plot.sectors)[5] <- "name"

if (spoilers.hide) { df.plot.sectors <- df.plot.sectors[which(df.plot.sectors$knownto == "player"),] }
df.plot.sectors$colour <- factions.colours[match(df.plot.sectors$owner, sector.owners)]
df.plot.sectors$ownername <- factions.names[match(df.plot.sectors$owner, sector.owners)]
df.plot.sectors$sizecat <- "b"
# adjusting x,y coords and names of sectors in multi-sector clusters. There's probably a dynamic way to do this
xadjmin <- c("Earth","Saturn 2","Titan","Emperor's Pride IV","Litany of Fury XII","Savage Spur II","Atiya's Misfortune I","Faulty Logic VII","Black Hole Sun IV",
             "Hatikvah's Choice I","Tharka's Cascade XVII","Nopileos' Fortune II","Scale Plate Green VII","Turquoise Sea IX",
             "Ianamus Zura IV","Thuruk's Demise III","Guiding Star VII","Hewa's Twin I","Hewa's Twin III")
xadjplus <- c("The Moon","Emperor's Pride VI","Litany of Fury IX","Savage Spur I","Atiya's Misfortune III","Faulty Logic I","Black Hole Sun V",
              "Hatikvah's Choice III","Tharka's Cascade XV","Grand Exchange I","Grand Exchange IV","Nopileos' Fortune VI","Scale Plate Green I","Turquoise Sea X",
              "Ianamus Zura VII","Thuruk's Demise II First Impact","Guiding Star V","Hewa's Twin II","Hewa's Twin IV The Cove","Avarice I","Avarice V Dead End")
yadjmin <- c("The Moon","Titan","Emperor's Pride IV","Litany of Fury IX","Savage Spur II","Atiya's Misfortune III","Faulty Logic VII","Black Hole Sun V",
             "Hatikvah's Choice I","Tharka's Cascade XV","Grand Exchange I","Nopileos' Fortune VI","Scale Plate Green I","Turquoise Sea X",
             "Ianamus Zura IV","Thuruk's Demise II First Impact","Guiding Star V","Hewa's Twin I","Hewa's Twin IV The Cove","Avarice V Dead End")
yadjplus <- c("Earth","Saturn 2","Emperor's Pride VI","Litany of Fury XII","Savage Spur I","Atiya's Misfortune I","Faulty Logic I","Black Hole Sun IV",
              "Hatikvah's Choice III","Tharka's Cascade XVII","Grand Exchange IV","Nopileos' Fortune II","Scale Plate Green VII","Turquoise Sea IX",
              "Ianamus Zura VII","Thuruk's Demise III","Guiding Star VII","Hewa's Twin II","Hewa's Twin III","Avarice I")
alladj <- unique(c(xadjmin, xadjplus, yadjmin, yadjplus))
# there's always a few that want to be speshul
df.plot.text <- df.plot.sectors[df.plot.sectors$name %in% c(alladj, "Grand Exchange III", "Saturn 1", "Avarice IV"),]
df.plot.text$altname <- gsub(" [12IVX]+.*", "", df.plot.text$name)

df.plot.sectors$sizecat[df.plot.sectors$name %in% alladj] <- "s"
df.plot.sectors$x[df.plot.sectors$name %in% xadjmin] <- df.plot.sectors$x[df.plot.sectors$name %in% xadjmin] - x.div/8
df.plot.sectors$x[df.plot.sectors$name %in% xadjplus] <- df.plot.sectors$x[df.plot.sectors$name %in% xadjplus] + x.div/8
df.plot.sectors$y[df.plot.sectors$name %in% yadjmin] <- df.plot.sectors$y[df.plot.sectors$name %in% yadjmin] - y.div/4
df.plot.sectors$y[df.plot.sectors$name %in% yadjplus] <- df.plot.sectors$y[df.plot.sectors$name %in% yadjplus] + y.div/4
df.plot.sectors$x[which(df.plot.sectors$name == "Grand Exchange III")] <- df.plot.sectors$x[which(df.plot.sectors$name == "Grand Exchange III")] - x.div/4
df.plot.sectors$sizecat[which(df.plot.sectors$name == "Grand Exchange III")] <- "s"
df.plot.sectors$x[which(df.plot.sectors$name == "Saturn 1")] <- df.plot.sectors$x[which(df.plot.sectors$name == "Saturn 1")] + x.div/4
df.plot.sectors$sizecat[which(df.plot.sectors$name == "Saturn 1")] <- "s"
df.plot.sectors$x[which(df.plot.sectors$name == "Avarice IV")] <- df.plot.sectors$x[which(df.plot.sectors$name == "Avarice IV")] - x.div/4
df.plot.sectors$sizecat[which(df.plot.sectors$name == "Avarice IV")] <- "s"

df.temp <- df.plot.sectors[df.plot.sectors$name %in% df.plot.text$name,]
df.temp$altname <- gsub(".*? ([IVX12]+[ ]?.*)", "\\1", df.temp$name)
df.plot.text <- rbind(df.plot.text[!duplicated(df.plot.text$altname),], df.temp)
df.plot.text <- df.plot.text[!(df.plot.text$sizecat == "b" & df.plot.text$altname %in% c("The Moon","Titan","Earth")),]
df.plot.sectors$altname <- df.plot.sectors$name
df.plot.text <- rbind(df.plot.text, df.plot.sectors[!df.plot.sectors$name %in% df.plot.text$name,])
df.plot.text$x[which(df.plot.text$altname == "Saturn")] <- df.plot.text$x[which(df.plot.text$altname == "Saturn")] - x.div/8
df.plot.text$x[which(df.plot.text$altname == "Avarice")] <- df.plot.text$x[which(df.plot.text$altname == "Avarice")] + x.div/8
df.plot.text$x[which(df.plot.text$altname == "Grand Exchange")] <- df.plot.text$x[which(df.plot.text$altname == "Grand Exchange")] + x.div/8
df.plot.text <- df.plot.text[!is.na(df.plot.text$altname),]

message(paste(format(Sys.time(), "%H:%M:%OS3"), "df.plot.police"))
df.temp <- df.police[df.police$time > time.overlay, c("sector.name","sector.macro")]
df.temp <- aggregate(df.temp$sector.macro, by = list(name = df.temp$sector.name), FUN = "length")
colnames(df.temp)[2] <- "interdictions"
df.plot.police <- inner_join(df.plot.sectors, df.temp, by = "name")[,c("x","y","knownto","sizecat","interdictions","name")]
df.plot.police$scale <- df.plot.police$interdictions / max(df.plot.police$interdictions, na.rm = TRUE)

message(paste(format(Sys.time(), "%H:%M:%OS3"), "df.plot.pirates"))
df.temp <- df.pirates[df.pirates$time > time.overlay, c("sector.name","sector.macro")]
if (NROW(df.temp) > 0) {
  df.temp <- aggregate(df.temp$sector.macro, by = list(name = df.temp$sector.name), FUN = "length")
}
colnames(df.temp)[2] <- "harassments"

if (NROW(df.temp) > 0) {
  df.plot.pirates <- inner_join(df.plot.sectors, df.temp, by = "name")[,c("x","y","knownto","sizecat","harassments","name")]
  df.plot.pirates$scale <- df.plot.pirates$harassments / max(df.plot.pirates$harassments, na.rm = TRUE)
} else {
  df.plot.pirates <- data.frame(x=numeric(0),y=numeric(0),knownto=character(0),sizecat=character(0), harassments=integer(0), name=character(0), scale=numeric(0))
}


df.temp <- left_join(left_join(left_join(df.plot.sectors[,"name", drop = FALSE],
                                         df.plot.police[,c("name","interdictions")],
                                         by = "name"),
                               df.plot.pirates[,c("name","harassments")],
                               by = "name"),
                     df.sectors[,c("name", intersect(wares.levels, colnames(df.sectors)))],
                     by = "name" )
for (res in intersect(wares.levels, colnames(df.sectors))) {
  quant <- quantile(df.temp[which(df.temp[,res] > 0.0), res], na.rm = TRUE)
  df.temp[,res] <- case_when(df.temp[,res] >= quant[4] ~ 3,
                             df.temp[,res] >= quant[3] ~ 2,
                             df.temp[,res] > 0.0       ~ 1,
                             TRUE                      ~ 0)
}
df.plot.sectors <- left_join(df.plot.sectors, df.temp, by = "name")
df.plot.sectors$tooltip <- paste0("<b>", df.plot.sectors$name, "</b>", "<br>",
                                  ifelse(!is.na(df.plot.sectors$contested), paste(df.plot.sectors$ownername, "<b>(Contested)</b>"), df.plot.sectors$ownername), "<br>",
                                  ifelse(is.na(df.plot.sectors$interdictions), "", paste0(overlay.hours, "h Police Interdictions: ", df.plot.sectors$interdictions, "<br>")),
                                  ifelse(is.na(df.plot.sectors$harassments), "", paste0(overlay.hours, "h Pirate Harassments: ", df.plot.sectors$harassments, "<br>")),
                                  "<b>Resources</b>")
lvls <- c("Low","Medium","High")
for (lvl in rev(seq_along(lvls))) {
  df.plot.sectors$tooltip <- paste0(df.plot.sectors$tooltip, "<br>", lvls[lvl], ": ")
  for (res in intersect(wares.levels, colnames(df.plot.sectors))) {
    idx <- which(df.plot.sectors[,res] == lvl)
    df.plot.sectors$tooltip[idx] <- paste0(df.plot.sectors$tooltip[idx], wares.names[match(res, wares.levels)], ", ")
  }
  idx <- which(rowSums(df.plot.sectors[,intersect(wares.levels, colnames(df.plot.sectors))] == lvl) == 0)
  df.plot.sectors$tooltip[idx] <- paste0(df.plot.sectors$tooltip[idx], "None")
  df.plot.sectors$tooltip[-idx] <- substr(df.plot.sectors$tooltip[-idx], 1, nchar(df.plot.sectors$tooltip[-idx]) - 2)
}

message(paste(format(Sys.time(), "%H:%M:%OS3"), "makeMap"))

# sizex/sizey = image size (keep it at 16x9 AR), marker.big/small = marker sizes in px, marker.border = owner colour border in px,
# marker.contested.big/small = size of the contested crosses
# font.size.map/legend = font sizes for the map/legend text
# sizex <- 1536; sizey <- 864; marker.big <- 62; marker.small <- 25; marker.border <- 6; marker.contested.big <- 45; marker.contested.small <- 20
makeMap <- function(sizex, sizey, marker.big, marker.small, marker.border, marker.contested.big, marker.contested.small, font.size.map = 8, font.size.legend = 13) {
  marker.opacity <- 0.6
  marker.symbol <- "hexagon2-open"
  marker.contested <- "diamond-x"
  marker.pirates <- "star-triangle-down"
  marker.police <- "star"
  p <- plot_ly(width = sizex, height = sizey,
               type = "scatter", mode = "markers", name = "Cluster Outlines", hoverinfo = "skip",
               showlegend = TRUE, legendgroup = "Base Map",
               legendgrouptitle = list(font = list(color = "#b0b0b0"), text = "Base Map"),
               x = df.plot.clusters$x, y = df.plot.clusters$y,
               marker = list(color = "#B0B0B0", opacity = marker.opacity, size = marker.big + marker.border,
                             symbol = marker.symbol, line = list(width = 2)))
  p <- p %>% add_trace(type = "scatter", mode = "markers", name = "Sector Outlines", hoverinfo = "skip",
                       showlegend = TRUE, legendgroup = "Base Map",
                       x = df.plot.sectors$x, y = df.plot.sectors$y,
                       marker = list(color = "#F0F0F0", opacity = marker.opacity,
                                     size = ifelse(df.plot.sectors$sizecat == "b", marker.big + marker.border, marker.small + marker.border),
                                     symbol = marker.symbol, line = list(width = 2)))
  p <- p %>% add_trace(type = "scatter", mode = "markers", name = "Contested Sectors", hoverinfo = "skip",
                       showlegend = TRUE, legendgroup = "Overlays", visible = "legendonly", legendrank = 1001,
                       legendgrouptitle = list(font = list(color = "#b0b0b0"), text = "Overlays"),
                       x = df.plot.sectors$x[!is.na(df.plot.sectors$contested)], y = df.plot.sectors$y[!is.na(df.plot.sectors$contested)],
                       marker = list(color = "#EEEE33", opacity = marker.opacity,
                                     size = ifelse(df.plot.sectors$sizecat[!is.na(df.plot.sectors$contested)] == "b", marker.contested.big, marker.contested.small),
                                     symbol = marker.contested, line = list(color = "#ffffff", opacity = marker.opacity, width = 1)))
  p <- p %>% add_trace(type = "scatter", mode = "markers", name = paste0("Police Interdictions (", overlay.hours, "h)"), hoverinfo = "skip",
                       showlegend = TRUE, legendgroup = "Overlays", visible = "legendonly", legendrank = 1001,
                       x = df.plot.police$x, y = df.plot.police$y,
                       marker = list(color = "#3333EE", opacity = marker.opacity,
                                     size = as.integer(1 + round(df.plot.police$scale * ifelse(df.plot.police$sizecat == "b", marker.contested.big - 1, marker.contested.small - 1))),
                                     symbol = marker.police, line = list(color = "#ffffff", opacity = marker.opacity, width = 1)))
  p <- p %>% add_trace(type = "scatter", mode = "markers", name = paste0("Pirate Harassments (", overlay.hours, "h)"), hoverinfo = "skip",
                       showlegend = TRUE, legendgroup = "Overlays", visible = "legendonly", legendrank = 1001,
                       x = df.plot.pirates$x, y = df.plot.pirates$y,
                       marker = list(color = "#EE3333", opacity = marker.opacity,
                                     size = as.integer(1 + round(df.plot.pirates$scale * ifelse(df.plot.pirates$sizecat == "b", marker.contested.big - 1, marker.contested.small - 1))),
                                     symbol = marker.pirates, line = list(color = "#ffffff", opacity = marker.opacity, width = 1)))
  
  for (o in intersect(sector.owners, df.plot.sectors$owner)) {
    df.temp <- df.plot.sectors[which(df.plot.sectors$owner == o),]
    df.temp <- df.temp[order(df.temp$contested, na.last = FALSE),]
    pcol <- head(df.temp$colour, 1)
    oname <- head(df.temp$ownername, 1)
    p <- p %>% add_trace(type = "scatter", mode = "markers", name = oname, hoverinfo = "text",
                         showlegend = TRUE, legendgroup = "Factions", legendgrouptitle = list(font = list(color = "#b0b0b0"), text = "Factions"), legendrank = 999,
                         x = df.temp$x, y = df.temp$y,
                         #hovertext = paste(df.temp$name, ifelse(!is.na(df.temp$contested), paste(df.temp$ownername, "(Contested)"), df.temp$ownername), sep = "<br>"),
                         hovertext = df.temp$tooltip,
                         marker = list(color = pcol, opacity = marker.opacity,
                                       size = ifelse(df.temp$sizecat == "b", marker.big, marker.small),
                                       symbol = marker.symbol,
                                       line = list(width = marker.border)))
  }
  p <- p %>% add_trace(type = "scatter", mode = "markers+text", name = "Sector Names", hoverinfo = "skip",
                       showlegend = TRUE, legendgroup = "Base Map",
                       textfont = list(size = font.size.map, color = "#f0f060a0"),
                       x = df.plot.text$x, y = df.plot.text$y, text = paste0("<b>", gsub(" (?![IVX]+ )(?![IVX]+$)(?!of )(?!to )(?!Sun$)(?!Plate )(?!First )(?!Dead )", "<br>", df.plot.text$altname, perl = TRUE), "</b>"),
                       marker = list(color = "#000000", opacity = 0.0, size = 0,
                                     line = list(color = "#000000", opacity = 0.0, width = 0)))
  p <- p %>% layout(paper_bgcolor = "#0f0f0f", plot_bgcolor = "#0f0f0f", autosize = FALSE,
                    margin = list(b = 0, l = 0, r = 0, t = 0),
                    legend = list(x = 0.0, y = 1.0, itemsizing = "constant",
                                  groupclick = "toggleitem",
                                  orientation = "h", traceorder = "grouped",
                                  font = list(size = font.size.legend, color = "#b0b0b0"),
                                  bgcolor = "#0f0f0f00"),
                    xaxis = list(range = x.range, fixedrange = TRUE,
                                 visible = FALSE, showgrid = FALSE, showline = FALSE, showticklabels = FALSE, ticks = ""),
                    yaxis = list(range = y.range, fixedrange = TRUE,
                                 visible = FALSE, showgrid = FALSE, showline = FALSE, showticklabels = FALSE, ticks = ""))
  return(p)
}
# make the map. If you want to change the map size you will also have to adjust the marker sizes to fit.
# there's no magic formula, use basic calculus and trial&error :)
# this works for 1920x1080 in case you want it as desktop background: makeMap(1920, 1080, 80, 33, 7, 55, 26)
p <- makeMap(1536, 864, 62, 25, 6, 44, 20)
saveWidget(p, paste0(path.Output, "/files/", plot.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")

# I suck at html/css, so let's keep things simple
dashboard.html <- paste0(dashboard.html, "<p>")
dashboard.html <- paste0(dashboard.html, tags$iframe(src = paste0("files/", plot.title, "_", game.guid, ".html"), style = "margin:0; width:1536px; height:864px; border:none; overflow:hidden;", scrolling = "no"))
dashboard.html <- paste0(dashboard.html, "</p>")

# This still works but requires a 3rd party tool
# https://github.com/plotly/orca
# However it's reportedly deprecated on plotly side so not sure how long it will work
# You can just save the map as png from the hover menu at the top right of the map instead
# message(paste(format(Sys.time(), "%H:%M:%OS3"), "Generating sector map as desktop wallpaper image (requires orca)"))
# p <- makeMap(1920, 1080, 80, 33, 7, 55, 26, 9)
# orca(p, "../Pictures/X4Map.png")

# generating the bar and stacked area plots
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Generating bar and area plots"))
sample_smoothing <- 1
avg_smoothing <- 12

if (!identical(find("df.sales"), character(0))) {
  maxtime <- max(df.sales$time)
  
  plot.title <- "Ship Construction per Faction"
  df.temp <- df.sales[which(df.sales$money > 0 & df.sales$commodity == "Ship construction"), c("time","buyer.faction","money")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    df.plot <- data.frame(time = trunc((df.temp$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, buyer = df.temp$buyer.faction, money = as.integer(df.temp$money))
    df.plot$buyer[is.na(df.plot$buyer)] <- "NIL"
    vals.time <- unique(df.plot$time)
    vals.buyers <- unique(df.plot$buyer)
    df.plot <- rbind(df.plot, data.frame(time = rep(vals.time, times = length(vals.buyers)), buyer = rep(vals.buyers, each = length(vals.time)), money = 0))
    df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time, buyer = df.plot$buyer), sum)
    df.plot <- df.plot[order(df.plot$buyer, df.plot$time),]
    colnames(df.plot)[3] <- "money"
    df.plotavg <- aggregate(df.temp$money, list(time = trunc((df.temp$time - maxtime) / 3600)), sum)
    df.plotavg <- df.plotavg[order(df.plotavg$time),]
    colnames(df.plotavg)[2] <- "money"
    try(df.plotavg$money <- movingAverage(df.plotavg$money, avg_smoothing), silent = TRUE)
    df.plotavg <- df.plotavg[!is.na(df.plotavg$money),]
    p <- ggplot(df.plot, aes(x = time, y = money/1000000)) + geom_col(aes(fill = buyer)) + geom_line(aes(x = time, y = money/1000000), df.plotavg, linetype = 2) + labs(title = plot.title, x = "Hours until Now", y = "Credits/hour (millions)", fill = "Faction") + theme(legend.position = "bottom") + scale_fill_manual(values = factions.colours[unique(df.plot$buyer)])
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
    for (b in vals.buyers) {
      df.plot$earnings[df.plot$buyer == b] <- cumsum(df.plot$money[df.plot$buyer == b])
    }
    plot.title <- paste(plot.title, "(cumulative)")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    p <- ggplot(df.plot, aes(x = time, y = earnings/1000000)) + geom_area(aes(fill = buyer)) + labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Faction") + theme(legend.position = "bottom") + scale_fill_manual(values = factions.colours[unique(df.plot$buyer)])
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
  }
  
  plot.title <- "Commodity Sales per Faction"
  df.temp <- df.sales[which(df.sales$money > 0 & !is.na(df.sales$buyer.faction) & !df.sales$commodity %in% c("Ship construction","Ship repair","Ship resupply")), c("time","buyer.faction","money")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    df.plot <- data.frame(time = trunc((df.temp$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, faction = df.temp$buyer.faction, money = df.temp$money)
    vals.time <- unique(df.plot$time)
    vals.factions <- unique(df.plot$faction)
    df.plot <- rbind(df.plot, data.frame(time = rep(vals.time, times = length(vals.factions)), faction = rep(vals.factions, each = length(vals.time)), money = 0))
    df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time, faction = df.plot$faction), sum)
    df.plot <- df.plot[order(df.plot$faction, df.plot$time),]
    colnames(df.plot)[3] <- "money"
    df.plotavg <- aggregate(df.temp$money, list(time = trunc((df.temp$time - maxtime) / 3600)), sum)
    df.plotavg <- df.plotavg[order(df.plotavg$time),]
    colnames(df.plotavg)[2] <- "money"
    df.plotavg$money <- movingAverage(df.plotavg$money, min(avg_smoothing, abs(min(df.plotavg$time))))
    df.plotavg <- df.plotavg[!is.na(df.plotavg$money),]
    p <- ggplot(df.plot, aes(x = time, y = money/1000000)) + geom_col(aes(fill = faction)) + geom_line(aes(x = time, y = money/1000000), df.plotavg, linetype = 2) + labs(title = plot.title, x = "Hours until Now", y = "Credits/hour (millions)", fill = "Faction") + theme(legend.position = "bottom") + scale_fill_manual(values = factions.colours[unique(df.plot$faction)])
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
    for (b in vals.factions) {
      df.plot$earnings[df.plot$faction == b] <- cumsum(df.plot$money[df.plot$faction == b])
    }
    plot.title <- paste(plot.title, "(cumulative)")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    p <- ggplot(df.plot, aes(x = time, y = earnings/1000000)) + geom_area(aes(fill = faction)) + labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Faction") + theme(legend.position = "bottom") + scale_fill_manual(values = factions.colours[unique(df.plot$faction)])
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
  }
  
  plot.title <- "Commodity Sales per Seller"
  df.temp <- df.sales[which(df.sales$money > 0 & !df.sales$commodity %in% c("Ship construction","Ship repair","Ship resupply")), c("time","seller.name","seller.code","money")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    df.temp$seller.name <- as.character(df.temp$seller.name)
    idx <- which(!(df.temp$seller.code %in% df.stations$code))
    df.temp$seller.name[idx] <- paste0(df.temp$seller.name[idx], " (", df.temp$seller.code[idx], ")")
    df.temp$seller.name <- as.factor(df.temp$seller.name)
    df.plot <- data.frame(time = trunc((df.temp$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, seller = df.temp$seller.name, money = df.temp$money)
    df.plot <- df.plot[!is.na(df.plot$seller),]
    vals.time <- unique(df.plot$time)
    vals.sellers <- unique(df.plot$seller)
    df.plot <- rbind(df.plot, data.frame(time = rep(vals.time, times = length(vals.sellers)), seller = rep(vals.sellers, each = length(vals.time)), money = 0))
    df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time, seller = df.plot$seller), "sum")
    df.plot <- df.plot[order(df.plot$seller, df.plot$time),]
    colnames(df.plot)[3] <- "money"
    df.plotavg <- aggregate(df.temp$money, list(time = trunc((df.temp$time - maxtime) / 3600)), sum)
    df.plotavg <- df.plotavg[order(df.plotavg$time),]
    colnames(df.plotavg)[2] <- "money"
    df.plotavg$money <- movingAverage(df.plotavg$money, min(c(avg_smoothing, nrow(df.plotavg)/2)))
    df.plotavg <- df.plotavg[!is.na(df.plotavg$money),]
    p <- ggplot(df.plot, aes(x = time, y = money/1000000)) + geom_col(aes(fill = seller)) + geom_line(aes(x = time, y = money/1000000), df.plotavg, linetype = 2) + labs(title = plot.title, x = "Hours until Now", y = "Credits/hour (millions)", fill = "Seller") + theme(legend.position = "bottom") + scale_fill_manual(values = mixedRainbow(length(unique(df.plot$seller))))
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
    for (b in vals.sellers) {
      df.plot$earnings[df.plot$seller == b] <- cumsum(df.plot$money[df.plot$seller == b])
    }
    plot.title <- paste(plot.title, "(cumulative)")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
    p <- ggplot(df.plot, aes(x = time, y = earnings/1000000)) + geom_area(aes(fill = seller)) + labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Seller") + theme(legend.position = "bottom") + scale_fill_manual(values = mixedRainbow(length(unique(df.plot$seller))))
    ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
    dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
  }
  
  if (!identical(find("df.buys"), character(0))) {
    plot.title <- "Commodity Buys per Buyer"
    df.temp <- df.buys[which(df.buys$money < 0), c("time","buyer.name","buyer.code","money")]
    if (nrow(df.temp) > 0) {
      message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
      df.temp$buyer.name <- as.character(df.temp$buyer.name)
      idx <- which(!(df.temp$buyer.code %in% df.stations$code))
      df.temp$buyer.name[idx] <- paste0(df.temp$buyer.name[idx], " (", df.temp$buyer.code[idx], ")")
      df.temp$buyer.name <- as.factor(df.temp$buyer.name)
      df.plot <- data.frame(time = trunc((df.temp$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, buyer = df.temp$buyer.name, money = -df.temp$money)
      df.plot <- df.plot[!is.na(df.plot$buyer),]
      vals.time <- unique(df.plot$time)
      vals.buyers <- unique(df.plot$buyer)
      df.plot <- rbind(df.plot, data.frame(time = rep(vals.time, times = length(vals.buyers)), buyer = rep(vals.buyers, each = length(vals.time)), money = 0))
      df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time, buyer = df.plot$buyer), sum)
      df.plot <- df.plot[order(df.plot$buyer, df.plot$time),]
      colnames(df.plot)[3] <- "money"
      df.plotavg <- aggregate(-df.temp$money, list(time = trunc((df.temp$time - maxtime) / 3600)), "sum")
      df.plotavg <- df.plotavg[order(df.plotavg$time),]
      colnames(df.plotavg)[2] <- "money"
      df.plotavg$money <- movingAverage(df.plotavg$money, min(c(avg_smoothing, nrow(df.plotavg)/2)))
      df.plotavg <- df.plotavg[!is.na(df.plotavg$money),]
      p <- ggplot(df.plot, aes(x = time, y = money/1000000)) + geom_col(aes(fill = buyer)) + geom_line(aes(x = time, y = money/1000000), df.plotavg, linetype = 2) + labs(title = plot.title, x = "Hours until Now", y = "Credits/hour (millions)", fill = "Buyer") + theme(legend.position = "bottom") + scale_fill_manual(values = mixedRainbow(length(unique(df.plot$buyer))))
      ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
      dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
      for (b in vals.buyers) {
        df.plot$earnings[df.plot$buyer == b] <- cumsum(df.plot$money[df.plot$buyer == b])
      }
      plot.title <- paste(plot.title, "(cumulative)")
      message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
      p <- ggplot(df.plot, aes(x = time, y = earnings/1000000)) + geom_area(aes(fill = buyer)) + labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Buyer") + theme(legend.position = "bottom") + scale_fill_manual(values = mixedRainbow(length(unique(df.plot$buyer))))
      ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
      dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
    }
    
    plot.title <- "Costs vs Profits"
    tl <- max(c(min(df.tradelog$time), min(df.log$time)))
    df.temp <- df.sales[which(df.sales$money > 0 & df.sales$time > tl), c("time","money")]
    df.temp2 <- df.buys[which(df.buys$money < 0 & df.buys$time > tl), c("time","money")]
    if (nrow(df.temp) > 0) {
      message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
      df.plot <- data.frame(time = trunc((df.temp$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, money = df.temp$money)
      df.plot2 <- data.frame(time = trunc((df.temp2$time - maxtime) / (sample_smoothing*3600)) * sample_smoothing, money = df.temp2$money)
      vals.time <- seq(min(c(df.plot$time, df.plot2$time)), 0)
      df.plot <- rbind(df.plot, data.frame(time = vals.time, money = 0))
      df.plot2 <- rbind(df.plot2, data.frame(time = vals.time, money = 0))
      df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time), "sum")
      df.plot <- df.plot[order(df.plot$time),]
      df.plot2 <- aggregate(df.plot2$money / sample_smoothing, list(time = df.plot2$time), "sum")
      df.plot2 <- df.plot2[order(df.plot2$time),]
      colnames(df.plot)[2] <- "money.in"
      colnames(df.plot2)[2] <- "money.out"
      df.plot <- cbind(df.plot, df.plot2[, "money.out", drop = FALSE])
      df.plotavg <- aggregate(df.plot$money.in + df.plot$money.out, list(time = df.plot$time), "sum")
      df.plotavg <- df.plotavg[order(df.plotavg$time),]
      colnames(df.plotavg)[2] <- "money"
      df.plotavg$money <- movingAverage(df.plotavg$money, min(c(avg_smoothing, nrow(df.plotavg)/2)))
      df.plotavg <- df.plotavg[!is.na(df.plotavg$money),]
      p <- ggplot(df.plot) +
        geom_col(aes(x = time, y = money.out/1000000, fill = "Costs")) +
        geom_col(aes(x = time, y = (money.in + money.out)/1000000, fill = "Profits")) +
        geom_line(aes(x = time, y = money/1000000), df.plotavg, linetype = 2) +
        labs(title = plot.title, x = "Hours until Now", y = "Credits/hour (millions)", fill = "Legend") +
        theme(legend.position = "bottom") +
        scale_fill_manual(values = c("#FF4040FF", "#404040FF"))
      ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
      dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
      df.plot$profits <- cumsum(df.plot$money.in + df.plot$money.out)
      df.plot$costs <- cumsum(df.plot$money.out)
      plot.title <- paste(plot.title, "(cumulative)")
      message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
      p <- ggplot(df.plot) +
        geom_area(aes(x = time, y = costs/1000000, fill = "Costs")) +
        geom_area(aes(x = time, y = profits/1000000, fill = "Profits")) +
        labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Legend") +
        theme(legend.position = "bottom") +
        scale_fill_manual(values = c("#FF4040FF", "#404040FF"))
      ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
      dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
    }
  }
}

if (!identical(find("df.transfers"), character(0)) && !is.null(df.transfers)) {
  plot.title <- "Account Transfers per Station (cumulative)"
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title))
  df.plot <- data.frame(time = trunc((df.transfers$time - time.now) / (sample_smoothing*3600)) * sample_smoothing, station = as.character(df.transfers$station.name), money = df.transfers$money, stringsAsFactors = FALSE)
  df.plot$station[is.na(df.plot$station)] <- as.character(df.transfers$station.code[is.na(df.transfers$station.name)])
  vals.time <- seq(min(df.plot$time), 0, 1)
  vals.stations <- unique(df.plot$station)
  df.plot <- rbind(df.plot, data.frame(time = rep(vals.time, times = length(vals.stations)), station = rep(vals.stations, each = length(vals.time)), money = 0))
  df.plot <- aggregate(df.plot$money / sample_smoothing, list(time = df.plot$time, station = df.plot$station), sum)
  df.plot <- df.plot[order(df.plot$station, df.plot$time),]
  colnames(df.plot)[3] <- "money"
  for (b in vals.stations) {
    df.plot$earnings[df.plot$station == b] <- cumsum(df.plot$money[df.plot$station == b])
    df.plot$rank[df.plot$station == b] <- max(df.plot$earnings[df.plot$station == b])
  }
  df.plot <- df.plot[order(df.plot$rank, df.plot$station, df.plot$time),]
  df.plot$station <- factor(df.plot$station, levels = unique(df.plot$station), ordered = TRUE)
  p <- ggplot(df.plot, aes(x = time, y = earnings/1000000)) + geom_area(aes(fill = station)) + labs(title = plot.title, x = "Hours until Now", y = "Credits (millions)", fill = "Station") + theme(legend.position = "bottom") + scale_fill_manual(values = mixedRainbow(length(unique(df.plot$station))))
  ggsave(paste0(path.Output, "/files/", plot.title, "_", game.guid, ".", graphs.imagetype), p, device = graphs.imagetype, width = graphs.width, height = graphs.height, units = graphs.units, dpi = graphs.dpi)
  dashboard.html <- paste0(dashboard.html, "<p><img src=\"files/", paste0(plot.title, "_", game.guid, ".", graphs.imagetype), "\"></p>")
}

message(paste(format(Sys.time(), "%H:%M:%OS3"), "Generating sunburst plots"))
sbFactionColor <- function(faction, level) {
  cols <- col2rgb(factions.colours[faction])
  alpha <- rep(255 - (level-1) * 48, NCOL(cols))
  return(rgb(cols["red",], cols["green",], cols["blue",], alpha, maxColorValue = 255))
}

sbplots <- c()

if (!identical(find("df.sales"), character(0))) {
  plot.title1 <- paste0(history.hours, "h Ship Sales per Wharf")
  df.temp <- df.sales[which(df.sales$time > time.limit & df.sales$commodity == "Ship construction" & df.sales$money > 0), c("seller.name","buyer.faction","commodity","buyer.name","money","amount")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
    plot.total <- sum(df.temp$money)
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", color = "#FFFFFF", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(df.temp[, "money", drop = FALSE], by = list(station = df.temp$seller.name), FUN = "sum")
    df.temp2$station <- as.factor(as.character(df.temp2$station))
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$station, label = df.temp2$station, value = df.temp2$money, parent = "total", color = rgb(colorRamp(c("brown","orange"))((as.integer(df.temp2$station))/max(as.integer(df.temp2$station))), maxColorValue = 255)))
    df.temp2 <- aggregate(df.temp[, "money", drop = FALSE], by = list(station = df.temp$seller.name, faction = df.temp$buyer.faction), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$station, df.temp2$faction, sep = ">>"), label = df.temp2$faction, value = df.temp2$money, parent = df.temp2$station, color = sbFactionColor(df.temp2$faction, 1)))
    df.temp2 <- aggregate(df.temp[, c("money","amount")], by = list(station = df.temp$seller.name, faction = df.temp$buyer.faction, ship = df.temp$buyer.name), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$station, df.temp2$faction, df.temp2$ship, sep = ">>"), label = paste0(df.temp2$amount, " x<br>", df.temp2$ship), value = df.temp2$money, parent = paste(df.temp2$station, df.temp2$faction, sep = ">>"), color = sbFactionColor(df.temp2$faction, 2)))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    sbplots <- c(sbplots, paste0("files/", plot.title1, "_", game.guid, ".html"))
  }
  
  plot.title2 <- paste0(history.hours, "h Ship Sales per Faction")
  df.temp <- df.sales[which(df.sales$time > time.limit & df.sales$commodity == "Ship construction" & df.sales$money > 0), c("seller.name","buyer.faction","commodity","buyer.name","money","amount")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
    plot.total <- sum(df.temp$money)
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", color = "#FFFFFF", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(df.temp[, "money", drop = FALSE], by = list(faction = df.temp$buyer.faction), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$faction, label = df.temp2$faction, value = df.temp2$money, parent = "total", color = sbFactionColor(df.temp2$faction, 1)))
    df.temp2 <- aggregate(df.temp[, c("money","amount")], by = list(faction = df.temp$buyer.faction, ship = df.temp$buyer.name), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$ship, sep = ">>"), label = paste0(df.temp2$amount, " x<br>", df.temp2$ship), value = df.temp2$money, parent = df.temp2$faction, color = sbFactionColor(df.temp2$faction, 2)))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title2, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"))
  }
  
  plot.title1 <- paste0(history.hours, "h Commodity Sales by Faction")
  df.temp <- df.sales[which(df.sales$time > time.limit & df.sales$money > 0 & !df.sales$commodity %in% c("Ship construction","Ship repair","Ship resupply")), c("seller.name","seller.code","buyer.faction","commodity","buyer.name","money","amount")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
    df.temp$seller.name <- as.character(df.temp$seller.name)
    idx <- which(!(df.temp$seller.code %in% df.stations$code))
    df.temp$seller.name[idx] <- paste0(df.temp$seller.name[idx], " (", df.temp$seller.code[idx], ")")
    df.temp$seller.name <- as.factor(df.temp$seller.name)
    df.temp$commodity <- wares.names[match(df.temp$commodity, wares.levels)]
    plot.total <- sum(df.temp$money)
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", color = "#FFFFFF", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(df.temp$money, by = list(faction = df.temp$buyer.faction), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$faction, label = df.temp2$faction, value = df.temp2$x, parent = "total", color = sbFactionColor(df.temp2$faction, 1)))
    df.temp2 <- aggregate(df.temp$money, by = list(seller = df.temp$seller.name, faction = df.temp$buyer.faction), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$seller, sep = "-"), label = df.temp2$seller, value = df.temp2$x, parent = df.temp2$faction, color = sbFactionColor(df.temp2$faction, 2)))
    df.temp2 <- aggregate(df.temp$money, by = list(commodity = df.temp$commodity, seller = df.temp$seller.name, faction = df.temp$buyer.faction), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$commodity, df.temp2$faction, df.temp2$seller, sep = "-"), label = df.temp2$commodity, value = df.temp2$x, parent = paste(df.temp2$faction, df.temp2$seller, sep = "-"), color = sbFactionColor(df.temp2$faction, 3)))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE, format = "f"), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color))
    p <- p %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    
    plot.title2 <- paste0(history.hours, "h Commodity Sales by Commodity")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(df.temp$money, by = list(commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$commodity, label = df.temp2$commodity, value = df.temp2$x, parent = "total"))
    df.temp2 <- aggregate(df.temp$money, by = list(seller = df.temp$seller.name, commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$commodity, df.temp2$seller, sep = "-"), label = df.temp2$seller, value = df.temp2$x, parent = df.temp2$commodity))
    df.temp2 <- aggregate(df.temp$money, by = list(faction = df.temp$buyer.faction, seller = df.temp$seller.name, commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$seller, df.temp2$commodity, sep = "-"), label = df.temp2$faction, value = df.temp2$x, parent = paste(df.temp2$commodity, df.temp2$seller, sep = "-")))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE, format = "f"), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value)
    p <- p %>% layout(title = list(text = plot.title2, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    
    sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"), paste0("files/", plot.title1, "_", game.guid, ".html"))
  }
}

if (!identical(find("df.buys"), character(0))) {
  plot.title2 <- paste0(history.hours, "h Commodity Buys by Commodity")
  df.temp <- df.buys[which(df.buys$time > time.limit & df.buys$money < 0), c("seller.name","seller.faction","commodity","buyer.name","buyer.code","money","amount")]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
    df.temp$buyer.name <- as.character(df.temp$buyer.name)
    idx <- which(!(df.temp$buyer.code %in% df.stations$code))
    df.temp$buyer.name[idx] <- paste0(df.temp$buyer.name[idx], " (", df.temp$buyer.code[idx], ")")
    df.temp$buyer.name <- as.factor(df.temp$buyer.name)
    df.temp$commodity <- wares.names[match(df.temp$commodity, wares.levels)]
    plot.total <- sum(-df.temp$money)
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(-df.temp$money, by = list(commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$commodity, label = df.temp2$commodity, value = df.temp2$x, parent = "total"))
    df.temp2 <- aggregate(-df.temp$money, by = list(buyer = df.temp$buyer.name, commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$commodity, df.temp2$buyer, sep = "-"), label = df.temp2$buyer, value = df.temp2$x, parent = df.temp2$commodity))
    df.temp2 <- aggregate(-df.temp$money, by = list(faction = df.temp$seller.faction, buyer = df.temp$buyer.name, commodity = df.temp$commodity), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$buyer, df.temp2$commodity, sep = "-"), label = df.temp2$faction, value = df.temp2$x, parent = paste(df.temp2$commodity, df.temp2$buyer, sep = "-")))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE, format = "f"), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value)
    p <- p %>% layout(title = list(text = plot.title2, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    
    sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"))

    plot.title2 <- paste0(history.hours, "h Commodity Buys by Buyer")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
    plot.total <- sum(-df.temp$money)
    df.plot <- data.frame(ID = "total", label = "Total", value = plot.total, parent = "", stringsAsFactors = FALSE)
    df.temp2 <- aggregate(-df.temp$money, by = list(buyer = df.temp$buyer.name), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$buyer, label = df.temp2$buyer, value = df.temp2$x, parent = "total"))
    df.temp2 <- aggregate(-df.temp$money, by = list(commodity = df.temp$commodity, buyer = df.temp$buyer.name), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$buyer, df.temp2$commodity, sep = "-"), label = df.temp2$commodity, value = df.temp2$x, parent = df.temp2$buyer))
    df.temp2 <- aggregate(-df.temp$money, by = list(faction = df.temp$seller.faction, commodity = df.temp$commodity, buyer = df.temp$buyer.name), FUN = "sum")
    df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$commodity, df.temp2$buyer, sep = "-"), label = df.temp2$faction, value = df.temp2$x, parent = paste(df.temp2$buyer, df.temp2$commodity, sep = "-")))
    df.plot$label[1] <- paste(df.plot$label[1], paste0(format(df.plot$value[1], trim = TRUE, big.mark = ","), " Cr."), paste(format(round(plot.total / ((time.now - time.limit) / 3600.0)), trim = TRUE, big.mark = ","), "Cr/h"), sep = "<br>")
    df.plot$label[-1] <- paste(df.plot$label[-1], paste0(format(df.plot$value[-1], trim = TRUE, big.mark = ","), " Cr."), paste0(format(100.0 * df.plot$value[-1] / plot.total, digits = 1, scientific = FALSE, trim = TRUE, format = "f"), "%"), sep = "<br>")
    p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value)
    p <- p %>% layout(title = list(text = plot.title2, font = list(size = 18)), margin = list(t = 40))
    saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    
    sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"))
  }
}

sbSectorColor <- function(owner, level) {
  cols <- col2rgb(factions.colours[match(owner, sector.owners)])
  alpha <- rep(255 - (level-1) * 32, NCOL(cols))
  return(rgb(cols["red",], cols["green",], cols["blue",], alpha, maxColorValue = 255))
}
if (!spoilers.hide) {
  plot.title1 <- "Total Sector recharge per Resource"
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
  df.temp <- melt(df.sectors[,intersect(colnames(df.sectors), c("id","helium","hydrogen","ice","methane","nividium","ore","silicon","rawscrap"))], "id", na.rm = TRUE)
  df.temp$variable <- as.character(df.temp$variable)
  df.temp2 <- aggregate(df.temp$value, by = list(resource = df.temp$variable), FUN = "sum")
  df.temp$percentage <- as.integer(round(10000.0 * df.temp$value / df.temp2$x[match(df.temp$variable, df.temp2$resource)], digits = 0))
  df.temp <- df.temp[which(df.temp$percentage > 0),]
  df.temp2 <- aggregate(df.temp$percentage, by = list(resource = df.temp$variable), FUN = "sum")
  df.plot <- rbind(data.frame(ID = "root", label = "Mining<br>Resources", value = sum(df.temp2$x), parent = ""),
                   data.frame(ID = df.temp2$resource, label = wares.names[match(df.temp2$resource, wares.levels)], value = df.temp2$x, parent = "root"))
  df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp$variable, df.temp$id, sep = ">>"), label = paste(df.sectors$name[match(df.temp$id, df.sectors$id)], paste0(0.01 * df.temp$percentage, " % (", round(df.temp$value, digits = 2), "/s)"), sep = "<br>"), value = df.temp$percentage, parent = df.temp$variable))
  p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value) %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
  saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
  sbplots <- c(sbplots, paste0("files/", plot.title1, "_", game.guid, ".html"))
  
  plot.title2 <- "Resource availability per Sector"
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
  df.temp <- melt(df.sectors[,intersect(colnames(df.sectors), c("id","owner","helium","hydrogen","ice","methane","nividium","ore","silicon","rawscrap"))], c("id","owner"), na.rm = TRUE)
  df.temp$variable <- as.character(df.temp$variable)
  df.temp2 <- aggregate(df.temp$value, by = list(resource = df.temp$variable), FUN = "sum")
  df.temp$percentage <- as.integer(round(10000.0 * df.temp$value / df.temp2$x[match(df.temp$variable, df.temp2$resource)], digits = 0))
  df.temp <- df.temp[which(df.temp$value > 0),]
  df.temp2 <- aggregate(df.temp$percentage, by = list(id = df.temp$id), FUN = "sum")
  df.temp3 <- aggregate(df.temp$percentage, by = list(faction = df.temp$owner), FUN = "sum")
  df.plot <- rbind(data.frame(ID = "root", label = "Resource availability<br>by Sector owner", value = sum(df.temp2$x), parent = "", color = "#FFFFFFFF"),
                   data.frame(ID = df.temp3$faction, label = factions.levels[match(df.temp3$faction, sector.owners)], value = df.temp3$x, parent = "root", color = sbSectorColor(df.temp3$faction, 1)),
                   data.frame(ID = df.temp2$id, label = df.sectors$name[match(df.temp2$id, df.sectors$id)], value = df.temp2$x, parent = df.sectors$owner[match(df.temp2$id, df.sectors$id)], color = sbSectorColor(df.sectors$owner[match(df.temp2$id, df.sectors$id)], 2)))
  df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp$id, df.temp$variable, sep = ">>"), label = paste(wares.names[match(df.temp$variable, wares.levels)], paste0(0.01 * df.temp$percentage, " %"), sep = "<br>"), value = df.temp$percentage, parent = df.temp$id, color = sbSectorColor(df.temp$owner, 3)))
  p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title2, font = list(size = 18)), margin = list(t = 40))
  saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
  sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"))
}

plot.title1 <- "Station modules per sector"
message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
df.temp <- df.universe[(df.universe$class == "station"), c("macro","owner","sector.macro","modules","knownto")]
df.temp$modules[is.na(df.temp$modules)] <- 1
if (spoilers.hide) {
  df.temp <- df.temp[which(df.temp$knownto == "player"),]
  df.temp <- df.temp[which(df.temp$sector.macro %in% df.sectors$macro[which(df.sectors$knownto == "player")]),]
}
df.temp$faction <- factions.levels[match(df.temp$owner, sector.owners)]
df.temp$sector.name <- df.sectors$name[match(df.temp$sector.macro, df.sectors$sector.macro)]
df.temp$sector.owner <- factions.levels[match(df.sectors$owner[match(df.temp$sector.macro, df.sectors$sector.macro)], sector.owners)]
df.plot <- data.frame(ID = "root", label = "Station modules<br>per sector", value = sum(df.temp$modules), parent = "", color = "#ffffff")
df.temp2 <- aggregate(df.temp$modules, by = list(sector.owner = df.temp$sector.owner), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = df.temp2$sector.owner, label = df.temp2$sector.owner, value = df.temp2$x, parent = "root", color = factions.colours[match(df.temp2$sector.owner, factions.levels)]))
df.temp2 <- aggregate(df.temp$modules, by = list(sector.owner = df.temp$sector.owner, sector.name = df.temp$sector.name), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$sector.name, df.temp2$sector.owner), label = df.temp2$sector.name, value = df.temp2$x, parent = df.temp2$sector.owner, color = factions.colours[match(df.temp2$sector.owner, factions.levels)]))
df.temp2 <- aggregate(df.temp$modules, by = list(faction = df.temp$faction, sector.owner = df.temp$sector.owner, sector.name = df.temp$sector.name), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$sector.name, df.temp2$sector.owner), label = df.temp2$faction, value = df.temp2$x, parent = paste(df.temp2$sector.name, df.temp2$sector.owner), color = factions.colours[match(df.temp2$faction, factions.levels)]))
p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
sbplots <- c(sbplots, paste0("files/", plot.title1, "_", game.guid, ".html"))

plot.title1 <- "Ship hull mass per sector"
message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
df.temp <- df.universe[str_detect(df.universe$class, fixed("ship")), c("class","macro","owner","sector.macro","knownto")]
if (spoilers.hide) {
  df.temp <- df.temp[which(df.temp$knownto == "player"),]
  df.temp <- df.temp[which(df.temp$sector.macro %in% df.sectors$macro[which(df.sectors$knownto == "player")]),]
}
df.temp$size <- toupper(str_replace(df.temp$class, fixed("ship_"), ""))
df.temp <- df.temp[df.temp$size != "XS",]
df.temp$faction <- factions.levels[match(df.temp$owner, sector.owners)]
df.temp$sector.name <- df.sectors$name[match(df.temp$sector.macro, df.sectors$sector.macro)]
df.temp$sector.owner <- factions.levels[match(df.sectors$owner[match(df.temp$sector.macro, df.sectors$sector.macro)], sector.owners)]
df.temp <- left_join(df.temp, setNames(df.shipdata[,c("macro","mass")], c("macro","value")), by = "macro")
df.temp$value[is.na(df.temp$value)] <- 1.0
df.plot <- data.frame(ID = "root", label = "Ship hull mass<br>per sector", value = sum(df.temp$value), parent = "", color = "#ffffff")
df.temp2 <- aggregate(df.temp$value, by = list(sector.owner = df.temp$sector.owner), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = df.temp2$sector.owner, label = df.temp2$sector.owner, value = df.temp2$x, parent = "root", color = factions.colours[match(df.temp2$sector.owner, factions.levels)]))
df.temp2 <- aggregate(df.temp$value, by = list(sector.owner = df.temp$sector.owner, sector.name = df.temp$sector.name), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$sector.name, df.temp2$sector.owner), label = df.temp2$sector.name, value = df.temp2$x, parent = df.temp2$sector.owner, color = factions.colours[match(df.temp2$sector.owner, factions.levels)]))
df.temp2 <- aggregate(df.temp$value, by = list(faction = df.temp$faction, sector.owner = df.temp$sector.owner, sector.name = df.temp$sector.name), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$sector.name, df.temp2$sector.owner), label = df.temp2$faction, value = df.temp2$x, parent = paste(df.temp2$sector.name, df.temp2$sector.owner), color = factions.colours[match(df.temp2$faction, factions.levels)]))
df.temp2 <- aggregate(df.temp$value, by = list(size = df.temp$size, faction = df.temp$faction, sector.owner = df.temp$sector.owner, sector.name = df.temp$sector.name), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$size, df.temp2$faction, df.temp2$sector.name, df.temp2$sector.owner), label = df.temp2$size, value = df.temp2$x, parent = paste(df.temp2$faction, df.temp2$sector.name, df.temp2$sector.owner), color = factions.colours[match(df.temp2$faction, factions.levels)]))
p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
sbplots <- c(sbplots, paste0("files/", plot.title1, "_", game.guid, ".html"))

plot.title1 <- "Activity per faction"
message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title1))
df.temp <- df.universe[(df.universe$class == "station") | str_detect(df.universe$class, fixed("ship")), c("id","class","macro","owner","sector.macro","knownto")]
if (spoilers.hide) {
  df.temp <- df.temp[which(df.temp$knownto == "player"),]
  df.temp <- df.temp[which(df.temp$sector.macro %in% df.sectors$macro[which(df.sectors$knownto == "player")]),]
}
df.temp$size <- toupper(str_replace(df.temp$class, fixed("ship_"), ""))
df.temp <- df.temp[df.temp$size != "XS",]
df.temp$faction <- factions.levels[match(df.temp$owner, sector.owners)]
df.temp$sector.name <- df.sectors$name[match(df.temp$sector.macro, df.sectors$sector.macro)]
df.temp$sector.owner <- factions.levels[match(df.sectors$owner[match(df.temp$sector.macro, df.sectors$sector.macro)], sector.owners)]
df.temp <- left_join(df.temp, setNames(df.shipdata[,c("macro","mass")], c("macro","value")), by = "macro")
df.temp$value[df.temp$size == "STATION"] <- df.universe$mass[match(df.temp$id[df.temp$size == "STATION"], df.universe$id)] / 10.0
df.temp$value[is.na(df.temp$value)] <- 1.0
df.plot <- data.frame(ID = "root", label = "Activity<br>per faction", value = sum(df.temp$value), parent = "", color = "#ffffff")
df.temp2 <- aggregate(df.temp$value, by = list(faction = df.temp$faction), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = df.temp2$faction, label = df.temp2$faction, value = df.temp2$x, parent = "root", color = factions.colours[match(df.temp2$faction, factions.levels)]))
df.temp2 <- aggregate(df.temp$value, by = list(faction = df.temp$faction, sector.name = df.temp$sector.name, sector.owner = df.temp$sector.owner), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$sector.name), label = df.temp2$sector.name, value = df.temp2$x, parent = df.temp2$faction, color = factions.colours[match(df.temp2$sector.owner, factions.levels)]))
df.temp2 <- aggregate(df.temp$value, by = list(faction = df.temp$faction, sector.name = df.temp$sector.name, size = df.temp$size), FUN = sum)
df.plot <- rbind(df.plot, data.frame(ID = paste(df.temp2$faction, df.temp2$sector.name, df.temp2$size), label = df.temp2$size, value = df.temp2$x, parent = paste(df.temp2$faction, df.temp2$sector.name), color = factions.colours[match(df.temp2$faction, factions.levels)]))
p <- plot_ly(df.plot, type = "sunburst", branchvalues = "total", hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, values = ~value, marker = list(colors = ~color)) %>% layout(title = list(text = plot.title1, font = list(size = 18)), margin = list(t = 40))
saveWidget(p, paste0(path.Output, "/files/", plot.title1, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
sbplots <- c(sbplots, paste0("files/", plot.title1, "_", game.guid, ".html"))

if (!identical(find("df.wings"), character(0))) {
  plot.title2 <- "Fleet Compositions"
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", plot.title2))
  df.temp2 <- df.sectors[,c("id","name","code","owner","contested")]
  df.temp2$label <- df.temp2$name
  idx <- which(is.na(df.temp2$label) & (df.temp2$owner != "ownerless"))
  df.temp2$label[idx] <- paste0(df.temp2$owner[idx], " owned (", df.temp2$code[idx], ")")
  idx <- which(is.na(df.temp2$label))
  df.temp2$label[idx] <- "unowned"
  idx <- which(df.temp2$contested == 1)
  df.temp2$label[idx] <- paste0(df.temp2$label[idx], "<br><b>CONTESTED</b>")
  df.plot <- data.frame(ID = (sectors <- unique(df.playerowned$sector.id)), label = df.temp2$label[match(sectors, df.temp2$id)], parent = "", color = sbSectorColor(df.sectors$owner[match(sectors, df.sectors$id)], 1), stringsAsFactors = FALSE)
  df.temp <- df.stations[which(df.stations$id %in% df.wings$leader & !(df.stations$id %in% df.wings$follower)), c("id","name","code","sector.id")]
  df.temp$name <- as.character(df.temp$name)
  df.temp$name[is.na(df.temp$name)] <- "Unnamed Station"
  df.plot <- rbind(df.plot, data.frame(ID = df.temp$id, label = paste0(df.temp$name, "<br>", df.temp$code), parent = df.temp$sector.id, color = sbSectorColor(df.sectors$owner[match(df.temp$sector.id, df.sectors$id)], 2), stringsAsFactors = FALSE))
  df.temp2 <- df.ships[which(df.ships$id %in% df.wings$leader & !(df.ships$id %in% df.wings$follower)), c("id","name","code","sector.id")]
  if (nrow(df.temp2) > 0) {
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$id, label = paste0(df.temp2$name, "<br>", df.temp2$code), parent = df.temp2$sector.id, color = sbSectorColor(df.sectors$owner[match(df.temp2$sector.id, df.sectors$id)], 2), stringsAsFactors = FALSE))
    df.temp <- rbind(df.temp, df.temp2)
  }
  colnames(df.temp)[1] <- "follower"
  level <- 3
  while (any(df.temp$follower %in% df.wings$leader)) {
    df.temp2 <- inner_join(inner_join(df.temp[, "follower", drop = FALSE], df.wings, by = c("follower" = "leader"), multiple = "all"), df.ships[, c("id","name","code","sector.id")], by = c("follower.y" = "id"))
    colnames(df.temp2)[1:2] <- c("id","follower")
    df.plot <- rbind(df.plot, data.frame(ID = df.temp2$follower, label = paste0(df.temp2$name, "<br>", df.temp2$code), parent = df.temp2$id, color = sbSectorColor(df.sectors$owner[match(df.temp2$sector.id, df.sectors$id)], level), stringsAsFactors = FALSE))
    df.temp <- df.temp2
    level <- level + 1
  }
  df.temp2 <- df.ships[-which((df.ships$id %in% df.wings$leader) | (df.ships$id %in% df.wings$follower)), c("id","name","code","sector.id")]
  df.plot <- rbind(df.plot, data.frame(ID = df.temp2$id, label = paste0(df.temp2$name, "<br>", df.temp2$code), parent = df.temp2$sector.id, color = sbSectorColor(df.sectors$owner[match(df.temp2$sector.id, df.sectors$id)], 2), stringsAsFactors = FALSE))
  p <- plot_ly(df.plot, type = "sunburst", maxdepth = 3, hoverinfo = "label", ids = ~ID, labels = ~label, parents = ~parent, marker = list(colors = ~color)) %>% layout(title = list(text = paste(factions.names[1], plot.title2), font = list(size = 18)), margin = list(t = 40))
  saveWidget(p, paste0(path.Output, "/files/", plot.title2, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
  sbplots <- c(sbplots, paste0("files/", plot.title2, "_", game.guid, ".html"))
}

# adding sunburst plots to the html in iframes
dashboard.html <- paste0(dashboard.html, "<p>")
for (plt in sbplots) {
  dashboard.html <- paste0(dashboard.html, tags$iframe(src = plt, style = "margin:0; width:50%; height:768px; border:none; overflow:hidden;", scrolling = "no"))
}
dashboard.html <- paste0(dashboard.html, "</p>")

# generating the tables
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Generating tables"))
if (!identical(find("df.sales"), character(0))) {
  table.title <- paste0("Gross Earnings per Seller - ", history.hours, "h")
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
  df.temp <- df.sales[which(df.sales$time > time.limit & df.sales$money > 0),]
  if (nrow(df.temp) > 0) {
    df.sellers <- aggregate(data.frame(money = df.temp$money, count = 1, volume = df.temp$amount), list(Seller = paste0(df.temp$seller.name, " (", df.temp$seller.code, ")")), sum)
    df.sellers$avg1 <- round(df.sellers$money / df.sellers$count)
    df.sellers$avg2 <- round(df.sellers$money / df.sellers$volume)
    df.sellers$avg3 <- round(df.sellers$volume / df.sellers$count)
    df.sellers$avg4 <- round(df.sellers$money / ((time.now - time.limit) / 3600.0))
    df.sellers$avg5 <- round(df.sellers$count / ((time.now - time.limit) / 3600.0), 2)
    colnames(df.sellers)[2] <- "Earnings"
    colnames(df.sellers)[3] <- "Trades"
    colnames(df.sellers)[4] <- "Items"
    colnames(df.sellers)[5] <- "Cr/Trade"
    colnames(df.sellers)[6] <- "Cr/Item"
    colnames(df.sellers)[7] <- "Items/Trade"
    colnames(df.sellers)[8] <- "Cr/Hour"
    colnames(df.sellers)[9] <- "Trades/Hour"
    df.sellers <- df.sellers[order(df.sellers$`Cr/Hour`, decreasing = TRUE),]
    saveWidget(datatable(df.sellers, rownames = FALSE, width = 12*128, class = "display nowrap", caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    dashboard.html <- paste0(dashboard.html, "<p>",
                             tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
                             "</p>")
  
    table.title <- paste0("Gross Earnings per Ware or Service - ", history.hours, "h")
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
    df.commodities <- aggregate(data.frame(money = df.temp$money, count = 1, volume = df.temp$amount), list(Commodity = df.temp$commodity), sum)
    df.commodities$avg1 <- round(df.commodities$money / df.commodities$count)
    df.commodities$avg2 <- round(df.commodities$money / df.commodities$volume)
    df.commodities$avg3 <- round(df.commodities$volume / df.commodities$count)
    df.commodities$avg4 <- round(df.commodities$money / ((time.now - time.limit) / 3600.0))
    df.commodities$avg5 <- round(df.commodities$count / ((time.now - time.limit) / 3600.0), 2)
    colnames(df.commodities)[2] <- "Earnings"
    colnames(df.commodities)[3] <- "Trades"
    colnames(df.commodities)[4] <- "Items"
    colnames(df.commodities)[5] <- "Cr/Trade"
    colnames(df.commodities)[6] <- "Cr/Item"
    colnames(df.commodities)[7] <- "Items/Trade"
    colnames(df.commodities)[8] <- "Cr/Hour"
    colnames(df.commodities)[9] <- "Trades/Hour"
    df.commodities$Commodity <- coalesce(wares.names[match(df.commodities$Commodity, wares.levels)], df.commodities$Commodity)
    df.commodities <- df.commodities[order(df.commodities$`Cr/Hour`, decreasing = TRUE),]
    saveWidget(datatable(df.commodities, rownames = FALSE, width = 12*128, caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    dashboard.html <- paste0(dashboard.html, "<p>",
                             tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
                             "</p>")
  } else {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "-> No sales in the past", history.hours, "hours"))
  }
  
  table.title <- paste0("Gross Earnings per Constructed Ship Type - ", history.hours, "h")
  df.temp <- df.sales[which(df.sales$time > time.limit & df.sales$commodity == "Ship construction" & df.sales$money > 0),]
  if (nrow(df.temp) > 0) {
    message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
    df.shipbuilds <- aggregate(data.frame(money = df.temp$money, count = 1, volume = df.temp$amount), list(Faction = df.temp$buyer.faction, Ship = df.temp$buyer.name), sum)
    df.shipbuilds$avg1 <- round(df.shipbuilds$money / df.shipbuilds$count)
    df.shipbuilds$avg2 <- round(df.shipbuilds$money / df.shipbuilds$volume)
    df.shipbuilds$avg3 <- round(df.shipbuilds$volume / df.shipbuilds$count)
    df.shipbuilds$avg4 <- round(df.shipbuilds$money / ((time.now - time.limit) / 3600.0))
    df.shipbuilds$avg5 <- round(df.shipbuilds$count / ((time.now - time.limit) / 3600.0), 2)
    colnames(df.shipbuilds)[3] <- "Earnings"
    colnames(df.shipbuilds)[4] <- "Trades"
    colnames(df.shipbuilds)[5] <- "Items"
    colnames(df.shipbuilds)[6] <- "Cr/Trade"
    colnames(df.shipbuilds)[7] <- "Cr/Item"
    colnames(df.shipbuilds)[8] <- "Items/Trade"
    colnames(df.shipbuilds)[9] <- "Cr/Hour"
    colnames(df.shipbuilds)[10] <- "Trades/Hour"
    df.shipbuilds <- df.shipbuilds[order(df.shipbuilds$`Cr/Hour`, decreasing = TRUE),]
    saveWidget(datatable(df.shipbuilds, rownames = FALSE, width = 12*128, caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
    dashboard.html <- paste0(dashboard.html, "<p>",
                             tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
                             "</p>")
  }
}

table.title <- "Last 50 Destroyed Objects"
if (!identical(find("df.destroyed"), character(0))) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
  df.temp <- df.destroyed[order(df.destroyed$HoursAgo), c("HoursAgo","object","location","killer","time")]
  df.temp$HoursAgo <- format(df.temp$HoursAgo, digits = 1, nsmall = 1, scientific = FALSE)
  if (nrow(df.temp) > 50) {
    df.temp <- df.temp[1:50,]
  }
  colnames(df.temp)[1] <- "Hours Ago"
  colnames(df.temp)[2] <- "Object"
  colnames(df.temp)[3] <- "Location"
  colnames(df.temp)[4] <- "Killer"
  colnames(df.temp)[5] <- "Timestamp"
  saveWidget(datatable(df.temp, rownames = FALSE, width = 12*128, caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
  dashboard.html <- paste0(dashboard.html, "<p>",
                           tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
                           "</p>")
}

# too spoilery for release :p
# table.title <- "Pirate Ships"
# df.pirates <- df.universe[which(is.na(df.universe$state) & df.universe$class != "ship_xs" & df.universe$owner %in% c("fallensplit","scaleplate","yaki")),]
# if (nrow(df.pirates)) {
#   message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
#   df.pirates <- setNames(left_join(left_join(df.pirates, df.sectors, by = c("sector.macro" = "macro"))[,c(1,2,3,4,23,55,50)], df.shipdata, by = "macro")[,c(6,7,9,8,3,4,28)], c("Sector","SectorOwner","Class","Ship","Code","owner","Role"))
#   df.pirates <- df.pirates[which(df.pirates$Role == "military"),]
#   df.pirates$Role <- NULL
#   df.pirates <- df.pirates[order(df.pirates$Class, df.pirates$Sector, df.pirates$Code),]
#   saveWidget(datatable(df.pirates, rownames = FALSE, width = 12*128, caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
#   dashboard.html <- paste0(dashboard.html, "<p>",
#                            tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
#                            "</p>")
# }

table.title <- "Contested Sectors"
df.temp <- df.sectors[which(df.sectors$contested == 1), c("owner","name","knownto")]
if (spoilers.hide) { df.temp <- df.temp[which(df.temp$knownto == "player"),] }
df.temp$knownto <- NULL
if (NROW(df.temp) > 0) {
  message(paste(format(Sys.time(), "%H:%M:%OS3"), "->", table.title))
  df.temp <- df.temp[order(df.temp$owner != "player", df.temp$owner, df.temp$name),]
  colnames(df.temp) <- c("Owner","Sector")
  df.temp$Owner <- factions.names[match(df.temp$Owner, sector.owners)]
  saveWidget(datatable(df.temp, rownames = FALSE, width = 12*128, caption = tags$caption(table.title)), paste0(path.Output, "/files/", table.title, "_", game.guid, ".html"), selfcontained = FALSE, libdir = "lib")
  dashboard.html <- paste0(dashboard.html, "<p>",
                           tags$iframe(src = paste0("files/", table.title, "_", game.guid, ".html"), style = "margin:0; width:100%; height:512px; border:none; overflow:hidden;", scrolling = "no"),
                           "</p>")
}

# finishing and writing main html file
dashboard.html <- paste0(dashboard.html, "</html>\n")
cat(dashboard.html, file = paste0(path.Output, "/dashboard_", game.guid, ".html"))

# open the html file in the default browser, this may not work if you're on UNIX/Linux
# type help("browseURL") and look under Details for info on how to fix it
message(paste(format(Sys.time(), "%H:%M:%OS3"), "Attempting to open dashboard file in default browser:", paste0(path.Output, "/dashboard_", game.guid, ".html")))
utils::browseURL(paste0(path.Output, "/dashboard_", game.guid, ".html"))


# free up memory hog pointers & collect garbage
options(opt)
#stop()
free(result)
rm(list = (l <- ls())[str_which(l, "df.(?!temp.*)(?!plot.*)(?!cache.*)(?!files.*)(?!commodities)(?!destroyed)(?!sellers)(?!shipbuilds)", negate = TRUE)])
gc()
gc()

# free() and/or rm() followed by gc() doesn't seem to clear memory the R session grabs when using XML parser
# restarting the R session seems to be the only way to free up that memory:
.rs.restartR()
