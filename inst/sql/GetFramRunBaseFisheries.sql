SELECT DISTINCT RunID.RunID as fram_run_id,
       Fishery.FisheryTitle as fram_fishery_name,
	     BaseExploitationRate.FisheryID as fram_fishery_id,
       BaseExploitationRate.TimeStep as fram_time_step
FROM RunID
      INNER JOIN (BaseExploitationRate INNER JOIN Fishery
        ON BaseExploitationRate.FisheryID = Fishery.FisheryID )
      ON RunID.BasePeriodID = BaseExploitationRate.BasePeriodID
WHERE (Fishery.Species='COHO' AND RunID.SpeciesName='COHO' AND RunID.RunName = '${RUNNAME}');
