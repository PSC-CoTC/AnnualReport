# Annual Report Readme

This document describes how to configure and run the Coho Technical Committee Annual Reporting Tool.  Remember to first import the updated post-season template .csv files

The write the annual report run the R function:

	writeAnnualReport(run.year, post.season.run.name, pre.season.run.name,
                              post.season.fram.db = NA,
                              post.season.tamm = NA,
                              post.season.tamm.fishery.ref = "./csv/TammFisheryQueetsRef.csv",
                              post.season.tamm.esc.ref = "./csv/TammEscQueetsRef.csv",
                              pre.season.fram.db = NA,
                              pre.season.tamm = NA,
                              pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                              pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                              template.dir = "./templates/",
                              report.dir = "./report/",
                              data.dir = "./csv/",
                              combine.GS = NA)
						

As you can see it requires post season versions of the FRAM database and the TAMM files.  The run.names need to be set.  If others are left as NA then a selection dialog will be prompted, otherwise set the path manually. Be sure the use the same FRAM db that the updated imported templates were imported to

In addition *combine.GS* decides if the Georgia Strait MUs should be a single MU or split into VI and Mainland.  If it is left has NA it decides based on the run.year (with 2020 or greater combining the MUs).  

For example:

	writeAnnualReport(run.year = 2018,
                  post.season.run.name = "bc-BKCoho2018_A_2_a",
                  pre.season.run.name = "bc-Coho1830",
                  post.season.fram.db = "./fram db/Final/devloping BK2018 (8).mdb",
                  post.season.tamm = "./fram db/TAMM_Files_Postseason/BKCoho2018 final ver 8 postseason run.xlsx",
                  post.season.tamm.fishery.ref = "./csv/TammFisheryQueetsRef.csv",
                  post.season.tamm.esc.ref = "./csv/TammEscQueetsRef.csv",
                  pre.season.fram.db= "./fram db/Final/FramVS2-PSC-Coho-PreSeason-thru2019.mdb",
                  pre.season.tamm = "./fram db/TAMM_Files_Preseason/Coho1830_Final.xlsx",
                  pre.season.tamm.fishery.ref = "./csv/TammFisheryFullRef.csv",
                  pre.season.tamm.esc.ref = "./csv/TammEscFullRef.csv",
                  template.dir = "./templates/",
                  report.dir = "./report/",
                  data.dir = "./csv/",
                  combine.GS = TRUE)




