SELECT NonRetention.NonRetentionFlag,
	   NonRetention.CNRInput1 AS "cnr_mortalities"
FROM NonRetention
WHERE (NonRetention.RunID = ${RUNID} and NonRetention.TimeStep = ${TIMESTEP} and NonRetention.FisheryID = ${FISHERYID});
