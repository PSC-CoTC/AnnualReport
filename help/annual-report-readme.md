# Annual Report Readme

This document describes how to configure and run the Coho Technical Committee Annual Reporting Tool.

## Setup

To run the annual report, you need to setup 3 files and have the databases with the FRAM model runs available to the script.

For example, if you wanted to setup an annual report for 1974, the year of the first "-gate" scandal ([Watergate](https://en.wikipedia.org/wiki/Watergate_scandal)).  You would need to setup these three files:

1. A `"1974_report_config.r"` in the `"config"` folder
2. A run script called `"1974_Create_Annual_Report.bat"` in the main `"PSC-FRAM-Admin"` folder
3. Put the database(s) with your pre-season and post-season model runs in the `"fram db"` folder

### Configuring `"1974_report_config.r"`

To configure the "1974_report_config.r" file you need to modify following lines.  The first line is the run year for the annual report, set using the following line.

```
run.year <- 1974
```

Next, provide the file name of where the database containing the pre-season FRAM model run is saved.  The same database can hold both the pre and post season model runs, just use the same file name for both the pre and post season database file name.
```
pre.season.fram.db <- "./fram db/1974_Fram_postseason.mdb"
```

Based on the post-season database file, you provide the name of the post-season model run.  Just replace `"1974 pre-season run name"` with your model run name.
```
pre.season.run.name <- "1974 pre-season run name"
```

After configuring the pre-season model run, you then need to configure the post-season model.  The file name of where the database containing the post-season FRAM model run is saved.  The same database can hold both the pre- and post-season model runs, just use the same file name for both database file names.
```
post.season.fram.db <- "./fram db/1974_Fram_postseason.mdb"
```

Once you configure the post-season database file name, you must set the post-season model run name.  Just replace `"1974 post-season run name"` with your model run name.

```
post.season.run.name <- "1974 post-season run name"
```


### Configuring "1974_Create_Annual_Report.bat"

With the configuration file finished, you need to configure a windows batch file to generate the annual report.  Modify the following lines by replacing the year (`1974`) with the year you want to run.

```
set ANNUAL_REPORT_LOG=".\log\1974_Annual_Report.log"
set ANNUAL_REPORT_CONFIG=".\config\1974_report_config.r"
```


## Running

Just double-click the `1974_Create_Annual_Report.bat` to generate the 1974 annual report.

Once the script runs, you will have two output files:

The log of R script, showing possible errors and warnings is provided at:

`log\1974_Annual_Report.log`

The actual 1974 report is provided in the following file

`report\2014_AnnualReport.html`

Once the report is generated, you can convert the report from HTML to PDF using Chrome.

Voila! You should now (hopefully) have an annual report and not a -gate scandal :-)
