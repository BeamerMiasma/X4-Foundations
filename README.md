# X4-Foundations
Savegame analysis and visualization for X4: Foundations using R Script.

WARNING:
I never wrote this with the intent to release it, but considering the interest and enthousiasm in response to a post I made on reddit I decided to make it available. This is very much a community resource, NOT a fully fleshed out release-ready mod (and it will probably never be). I will update it for future DLC and additions I make if I find the time, but I do not offer much active support. You may find more info here: https://www.reddit.com/r/X4Foundations/comments/11bkwbh/my_friends_say_i_take_the_game_too_seriously/

PURPOSE:
Analyze the heck out of the X4 savegame for the purpose of making visualizations and data tables related to your ship construction, wares sales, sector ownership, fleet compositions, sector resources, etc.

IMPORTANT:
- This is for version 5.10. The 6.00 beta is not supported, and I will not update this for 6.00 until after it is officially released. Earlier versions should mostly work and DLCs are not required.
- X4 (still!) does not put ware BUYS in the log, only sales are logged. I find this extremely aggravating, but that's how the situation is. This means that you will get a great overview of your sales, but the script can tell you nothing about your buys. There was a mod ( https://www.nexusmods.com/x4foundations/mods/26 ) that could fix this (and the script can use that info), but looking at the comments it does not work anymore, but may be easy to fix. I don't know, I play unmodded these days, YMMV. Maybe some day I'll work up the courage to process the economylog but initial tests were not very promising. It's huge and XML is dreadfully inefficient.
- If you have mods that add ships, you will probably break some things. Ship model info is in the CSV files in the X4SaveGameAnalysis/data folder, you may be able to solve it by adding lines to the ship data file. If you have mods that add or move sectors, you will definitely break some things. Sector data is in the CSV files, you may be able to solve SOME issues by editing the relevant file but the map will almost definitely be borked without script changes.

QUICK START (Windows)
1. Download the X4SaveGameAnalysisRelease.zip package from this GitHub page. Open it and drag the X4SaveGameAnalysis folder from the ZIP to your Documents folder.
2. Go to https://posit.co/download/rstudio-desktop/ and download and install R and RStudio.
3. Start up RStudio, Menu [File] -> [Open File], browse to your Documents/X4SaveGameAnalysis folder and open the X4SaveGameAnalysis.R file.
4. Menu [Code] -> [Source] to run the script.
5. The first run will take several minutes, so be patient. Several library packages will need to be downloaded and installed by the script the very first time you run it. Further, on the first analysis of any game campaign the resource recharge for the entire universe will be extracted, this is a lengthy process. This info is static so will be saved to a cache file, on any subsequent runs it will use that cache file which is much faster.
6. On finishing, the script should automatically open the dashboard page in your browser. If it doesn't do so automatically, go to your Documents/X4SaveGameAnalysis/output folder and open the "dashboard_(long hexadecimal string).html" file.
7. If you see errors in the output while running the script, something broke (warnings are OK, they're not really... warnings, more like attention points for developers). Congratulations, you were just promoted to the advanced class. Study the script comments at the top of the script, if it's an issue with paths you should be able to fix it there.

QUICK START (Linux)
1. Read the above section and script comments and translate it to nerd. You're smart, you can do it.

QUICK START (Mac)
1. Is X4 even available for Mac? I sincerely have no idea, you're on your own. Maybe stuff just works since that is kinda the point of having a Mac.


NOTES OF INTEREST

The top of the script has some options you can set, see the script comments. Of interest to some may be the "spoilers.hide" option. Set this to TRUE to hide undiscovered sectors and ships from the map and graphs and to hide the resource recharge graphs.

Some notes on the graphs:
- The Map is interactive. Click on a legend entry on the map to toggle it on/off.
- The sunburst plots are interactive too. Click on any of the slices on the sunburst graphs to zoom in on that slice. Clicking on the center takes you back again one level at a time. The animation may get a bit choppy in plots with thousands of slices.
- Tables can be sorted by clicking on the column headers.
- The resource graphs do NOT show the amount of resources that are currently available in the sector, but rather how fast those resources regenerate. It's not meant to find you a cargo hold full of some specific resource fast, but rather for long-term planning, to figure out what resources your empire may be lacking and which sectors to go after to improve that situation.
- Ship hull mass per sector: hull mass is based on the bare mass of the ship's hull, without engines, turrets, weapons etc.
- Activity per faction: Station mass is estimated based on number of modules and a reasonable average mass per module, and then divided by 10. Ship mass (bare hull) is added to this. Even a small station component has a higher mass than a capital ship, so stations tend to dominate this graph.
- All of this is reverse engineered without much or any documentation. My interpretation of some data points may be wrong.
- Sorry for the colours in the graphs that don't use faction colours. My colour vision is bad, it probably looks horrible.

The script will keep a cache of your old log entries since the game removes these from the savegame after a while. The bar and stacked cumulative graphs work fine with 100s of hours of data (and the sunbursts and trade tables are limited to the last 3 hours anyway), however if you want to reduce the length you can delete the "cache_log_(long hexadecimal string).csv" file in the X4SaveGamneAnalysis/data folder where the "(long hexadecimal string)" is the same as the one in the dashboard html file. The amount of data in the savegame log probably depends on how many sales you make per minute, in my game it typically contains around 8-10 hours worth of sales data. The amount of time the bar graphs show after you run the script the first time should be a good indication of how much log is kept in your savegame.

This dashboard is very much designed around the way I play. If you play significantly differently it may not be to your liking and may even error out. The main focus is ship and intermediate production and sale. There's nothing here related to boarding, data vaults, quests and many other game aspects. It's also not meant for small fry. Unless you have at least 1 station, 1 fleet and 1 sale it's quite possible it won't even work.

The visualizations are just the top level. The main task of the script is to prepare a number of data tables that these visualizations are based on. After running the script, go to Menu [View] -> [Show Environment] to see the available data tables (all named df.* ). Click on any of them to browse their contents and use them to add your own tables and graphs.

The XML parser is very memory hungry. Processing my 1 GB savegame (120 MB compressed) easily costs 16 GB of memory. Trying to analyze a big game on an <16 GB machine is probably not a good idea. The script will handle both compressed and uncompressed savegames without noticable difference in speed on an SSD. If you have a magnetic HDD, you're probably best off using compressed savegames as the bottleneck is likely going to be your storage speed, not your CPU.

About R Script: Similar to python in usage and architecture but more aimed at statistical analysis. There's a lot of cross-pollination, many of the libraries I use here are also available for python and java. R just happened to be the language I needed to practice with. https://en.wikipedia.org/wiki/R_(programming_language)


HISTORY:
v1.0 - Initial release for game version 5.10
