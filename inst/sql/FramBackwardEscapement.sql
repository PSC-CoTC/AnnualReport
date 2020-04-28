SELECT RunID.RunID as "fram_run_id",
  RunID.RunYear as "run_year",
  BackwardsFRAM.StockID as "fram_stock_id",
  BackwardsFRAM.TargetFlag as "escapement_flag",
  BackwardsFRAM.TargetEscAge3 AS "target_escapement",
  BackwardsFRAM.Comment as "comment"
FROM (BackwardsFRAM INNER JOIN RunID ON BackwardsFRAM.RunID = RunID.RunID)
WHERE RunID.RunName = '${RUNNAME}';
