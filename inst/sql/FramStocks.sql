SELECT  
  StockLongName as "fram_stock_name",
  StockID as "fram_stock_id"
FROM Stock
WHERE UCASE(species) = 'COHO'
