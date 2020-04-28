UPDATE StockRecruit SET StockRecruit.RecruitScaleFactor = ${RECRUITSCALAR}
WHERE StockRecruit.RunID = ${RUNID} AND StockRecruit.StockID = ${STOCKID};
