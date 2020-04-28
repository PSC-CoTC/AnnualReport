UPDATE NonRetention SET NonRetentionFlag=1,
CNRInput1 = ${CNRMORTALITIES},
comment = ${COMMENT}
WHERE RunID = ${RUNID} AND FisheryID = ${FISHERYID} AND TimeStep = ${TIMESTEP};
