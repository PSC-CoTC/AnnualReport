SELECT StockRecruit.RecruitScaleFactor as "recruit_scalar"
FROM StockRecruit
WHERE StockRecruit.RunID = ${RUNID} AND StockRecruit.StockID = ${STOCKID};
