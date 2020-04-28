UPDATE BackwardsFRAM SET BackwardsFRAM.TargetFlag = ${ESCAPEMENTFLAG},
       BackwardsFRAM.TargetEscAge3 = ${TARGETESCAPEMENT},
       BackwardsFRAM.Comment = ${COMMENT}
WHERE RunID = ${RUNID} AND StockI D= ${STOCKID};
