# X4-Foundations
Savegame analysis and visualization for X4: Foundations using R Script.

WARNING: I never wrote this with the intent to release it, but considering the interest and enthousiasm in response to a post I made on reddit I decided to make it available. This is very much a community resource, NOT a fully fleshed out release-ready mod (and it will probably never be). I will update it for future DLC and additions I make if I find the time, but I do not offer much active support. You may find more info here: https://www.reddit.com/r/X4Foundations/comments/11bkwbh/my_friends_say_i_take_the_game_too_seriously/

PURPOSE:
Analyze the heck out of the X4 savegame for the purpose of making visualizations and data tables related to your ship construction, wares sales, sector ownership, fleet compositions, sector resources, etc.

IMPORTANT:
- This is for version 5.10. The 6.00 beta is not supported, and I will not update this for 6.00 until after it is officially released. Earlier versions should mostly work and DLCs are not required.
- X4 (still!) does not put ware BUYS in the log, only sales are logged. I find this extremely aggravating, but that's how the situation is. This means that you will get a great overview of your sales, but the script can tell you nothing about your buys. There was a mod ( https://www.nexusmods.com/x4foundations/mods/26 ) that could fix this (and the script can use that info), but looking at the comments it does not work anymore, but may be easy to fix. I don't know, I play unmodded these days, YMMV. Maybe some day I'll work up the courage to process the economylog but initial tests were not very promising. It's huge and XML is dreadfully inefficient.
- If you have mods that add ships, you will probably break some things. Ship model info is in the CSV files, you may be able to solve it by adding lines to the ship data file. If you have mods that add or change sectors, you will definitely break some things. Sector data is in the CSV files, you may be able to solve SOME issues by editing the relevant file but the map will almost definitely be borked without script changes.

QUICK START (Windows)
1. Download the X4SaveGameAnalysisRelease.zip package from this GitHub page. Open it and drag the X4SaveGameAnalysis folder from the ZIP to your Documents folder.
2. Go to https://posit.co/download/rstudio-desktop/ and download and install R and RStudio.
3. Start up RStudio, Menu [File] -> [Open File], browse to your Documents/X4SaveGameAnalysis folder and open the X4SaveGameAnalysis.R file.
4. Menu [Code] -> [Source] to run the script.
5. The first run will take several minutes, so be patient. Several library packages will need to be downloaded and installed by the script the very first time you run it. Further, on the first analysis of any game campaign the resource recharge for the entire universe will be extracted, this is a lengthy process. This info is static so will be saved to a cache file, on any subsequent runs it will use that cache file which is much faster.
6. On finishing, the script should automatically open the dashboard page in your browser. If it doesn't do so automatically, go to your Documents/X4SaveGameAnalysis/output folder and open the "dashboard_(long hexadecimal string).html" file.
7. If you see errors in the output while running the script, something broke. Congratulations, you were just promoted to the advanced class. Study the script comments and see if you can fix it.

QUICK START (Linux)
1. Study the above section and script comments and translate it to nerd. You're smart, you can do it.

QUICK START (Mac)
1. Is X4 even available for Mac? I sincerely have no idea, you're on your own. Maybe stuff just works since that is kinda the point of having a Mac.


NOTES OF INTEREST

This dashboard is very much designed around the way I play. If you play significantly differently it may not be to your liking. However the visualizations are just the top level. The main task of the script is to prepare a number of data tables that these visualizations are based on. After running the script, go to Menu [View] -> [Show Environment] to see the available data tables (all named df.* ). Click on any of them to browse their contents and use them to add your own tables and graphs.

Some notes on some of the graphs:
- (soon)

About R Script: Think of it as python but more aimed at statistical analysis. There's a lot of cross-pollination, many of the libraries I use here are also available for python and java. R just happened to be the language I needed to practice with. https://en.wikipedia.org/wiki/R_(programming_language)


HISTORY:
v1.0 - Initial release for game version 5.10
