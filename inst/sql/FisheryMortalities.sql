SELECT RunID.RunID as "fram_run_id",
  		 RunID.RunYear as "run_year",
	   Mortality.FisheryID as "fram_fishery_id",
	   Mortality.TimeStep as "fram_time_steps",
       Mortality.StockID as "fram_stock_id",
	   SUM (LandedCatch + NonRetention + Shaker + DropOff + MSFLandedCatch + MSFNonRetention + MSFShaker + MSFDropOff) AS "fishery_mortality"
	FROM Mortality
		INNER JOIN RunID ON Mortality.RunID = RunID.RunID
WHERE RunID.RunName = '${RUNNAME}'
	GROUP BY RunID.RunID, RunID.RunYear, Mortality.FisheryID, Mortality.TimeStep, Mortality.StockID
