# Exporting ERs to xlsx

There are two functions that can export fishery ERs by stock

* `exportFisheryERs(fram.db = NA, run.names, report.dir = "./report/")`
* `postSeasonTable3Summary(fram.db = NA, run.names, combine.GS = TRUE, data.dir = "./csv/")`

Both functions take a fram.db character string (or NA to select using pop up menu) and a vector of Fram run names. For example:

	exportFisheryERs(run.names = c("bc-bkCoho2010 final final",
                               "bc-bkCoho2011 final final",
                               "bc-bkCoho2012 final final",
                               "bc-bkCoho2013 final final"))
							   
Will promt the user to select the FRAM database with the listed run.names.

exportFisheryERs() exports a .xlsx document where each sheet contains the stock ERs per fishery and timestep for each listed run.name

postSeasonTable3Summary() exports a .xlsx document where each sheet contains table 3 (see annual report) for each listed run.name








