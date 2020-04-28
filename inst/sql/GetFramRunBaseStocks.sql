SELECT DISTINCT RunID.RunID as fram_run_id,
                Stock.StockLongName as fram_stock_name,
                BaseCohort.StockID as fram_stock_id
FROM RunID
      INNER JOIN (BaseCohort INNER JOIN Stock
        ON BaseCohort.StockID = Stock.StockID )
      ON RunID.BasePeriodID = BaseCohort.BasePeriodID
WHERE (Stock.Species='COHO' AND RunID.SpeciesName='COHO' AND RunID.RunName = '${RUNNAME}');
