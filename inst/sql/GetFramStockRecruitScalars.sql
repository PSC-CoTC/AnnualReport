SELECT RunID.RunID as "fram_run_id",
  StockRecruit.StockID as "fram_stock_id",
  StockRecruit.RecruitScaleFactor as "recruit_scalar"
FROM (StockRecruit INNER JOIN RunID ON StockRecruit.RunID = RunID.RunID)
WHERE RunID.RunName = '${RUNNAME}';
