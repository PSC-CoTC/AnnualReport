# Import File Readme

This document describes how to create import file templates and import data into a FRAM model run.  The main purpose of the import template function is to clearly split up responsibilities around updating catch values within a FRAM model run.

## Creating the Import templates

Import files can initially be created from an existing FRAM model run, typically the post-season run.  As the FRAM database name, run name, and run ID are within the import file template, the files must be created from the same run database and model run that the catch values are to be updated.  You can use the template file to update values within another model run, but the header information within the import template file must be modified

Use the R function **createImportFiles(fram.db.name = NA, fram.run.name, report.dir = "./import_files/",  data.dir = "./csv/")**\
*`fram.db.name`: defaults to NA which prompts a selection screen allowing you to select the FRAM database from which you would like to create the import files.  You can also set the path manually relative to your current working directory (using "\\" or "/" in the path name) 
* `fram.run.name`: character string of the FRAM run name from the fram database that you will use.  
*`report.dir`: the directory where the import templates will be saved
*`data.dir`: the directory where the person 'PersonFramFisheries.csv' and 'PersonFramStocks.csv' are saved.  This tell the code who is respsonsible for which stock/ fishery

## Modify Import Files

Once the import files are created, they can be distributed to individual scientists.  Generally speaking, the fisheries and time steps identified in the individual import files should not be modified because of two possible outcomes:

* Catch values may override values provided by other import files
* Catch values may not be used by FRAM because they don't align with the base period


### Fishery Flag Values

Fishery Flag | Description 
------------ | -------------
 1 | Non-Selective fishery based on base period scalar (not for use with Post Season catch) 
 2 | Non-Selective fishery based on quota (**use this to set final fishery catch**) 
 7 | Mark-Selective fishery based on base period scalar (not for use with Post Season catch) 
 8 | Mark-Selective fishery based on quota (**use this to set final fishery catch**) 
 17 | Non-Selective and Mark-Selective fishery based on base period scalar (not for use with Post Season catch) 
 18 | Non-Selective base period scalar and Mark-Selective quota fishery (not for use with Post Season catch) 
 27 | Non-Selective quota and Mark-Selective base period scalar fishery (not for use with Post Season catch) 
 28 | Non-Selective and Mark-Selective fishery based on quota (**use this to set final fishery catch**) 

### Escapement Flag Values
Escapement Flag | Description
--------------- | -------------
0 | Use pre-season target escapement
1 | Use entered exact target escapement
2 | Split into unmarked/marked target escapement using pre-season ratio. Enter flag 2 and corresponding escapement into either the marked (M) stock row.  Set the unmarked to 0. The associated component (UM or M, respectively) without an escapement value needs to receive a flag of 0.
9 | Updates the recruit scalar from the template but sets the flag in the FRAM database to 0 (to use preseason escapement).


## Update FRAM Post-Season Model Run with Import File

Once the import files have been updated, they can be imported back into the model run through the R function:

importFramTemplates(template_file = NA, validate.catch = TRUE, validate.fisheries = TRUE, validate.mark.info = TRUE, validate.stocks = TRUE, validate.escapment.flags = TRUE, data.dir = "./csv/")\

*`template_file`: defaults to NA allowing the user to select the file through a selection screen
*`validate.`: Usually keep as TRUE.  makes sure flags make sense (see below).
*`data.dir`: the directory where the person 'PersonFramFisheries.csv' and 'PersonFramStocks.csv' are saved.  This tell the code who is respsonsible for which stock/ fishery

Note: The import file is validated for use as a post-season catch data set for backward FRAM.  This mainly consists of checking fishery flags against non-selective and mark selective catch.

## Validation of Post-Season Catch Values

As part of importing post-season catch value files, several validation steps are run against the file.  The validation steps are all configurable using a TRUE/FALSE flag within the `"config/import_post_season_config.r"` file.  The following validation steps are carried out.

Validation Config Variable | Description 
------------ | -------------
 `validate.catch` | Checks that the flags are appropriately set for the catches provided (e.g. nonselective/mark-selective fisheries)
 `validate.fisheries` | Checks that all the fisheries are represented in the import file for the person responsible
 `validate.mark.info` |  Checks that mark rate information is provided if a mark-selective fishery is identified

:+1:
