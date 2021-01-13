# Exporting Preason FRAM models to .xlsx

You can export the data from a preseason FRAM model run using the following R function:

** `getPreseasonERs(pre.season.fram.db = NA,
                   pre.season.run.name,
                   run.year,
                   pre.season.tamm = NA,
                   pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                   pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                   template.dir = "./templates/",
                   report.dir = "./report/",
                   data.dir = "./csv/",
                   combine.GS = TRUE)`**

                        
You will requrie:
*  `pre.season.fram.db`: The FRAM .mdb file with the preseaon run.  Defaults to NA, which prompts a selection screen allowing the user to select the file.  Can be manually set by specifing the file location relative to your current working directory (using "\\" or "/" in the path)
* `pre.season.run.name`: The run name for the FRAM model run you wish to export data for.  In double ("") or single ('') quotations. Can be found in FRAM table "RunID" under the column "RunName"
*`run.year`: The numeric year corresponding to the preseason run you are ecporting data for.  Found in FRAM table "RunID" under "RunYear".
*` pre.season.tamm`: preseason TAMM .xlsx file corresponding to the correct preseaon model run. Defaults to NA, which prompts a selection screen allowing the user to select the file.  Can be manually set by specifing the file location relative to your current working directory (using "\\" or "/" in the path) 
*` pre.season.tamm.fishery.ref, pre.season.tamm.esc.ref, template.dir, report.dir,  data.dir`: Files and directories that area needed for the function to run.  Should be included in this package and default to standard values, but can be set manually if folder structure has been changed.  Note that the "./report/" directory will have to be created prior to running the function if it does not exist
*`combine.GS`: Logical indicating if the Georgia Strait MUs should be combined  


Exports a .xlsx file to the report.dir containing 4 tabs:
* Fishery_ERs_year-year: detailed FRAM fishery ERs similar to the export from the exportFisheryERs() function
* Fishery_mortality: Fisherys ERs by PSC fisheries.  Similar to annual report table 3, but in a long format with more details
* stock_summary: similar to annual report table 2
* Run_info: meta data about the FRAM model run

