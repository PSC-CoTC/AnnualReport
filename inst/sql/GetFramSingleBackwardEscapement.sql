SELECT
  BackwardsFRAM.TargetFlag as "escapement_flag",
  BackwardsFRAM.TargetEscAge3 AS "target_escapement"
FROM BackwardsFRAM
WHERE BackwardsFRAM.RunID = ${RUNID} AND BackwardsFRAM.StockID = ${STOCKID};
