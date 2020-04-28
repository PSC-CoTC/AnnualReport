SELECT RunID.RunName,
  Fishery.FisheryName as "fishery.name",
  FisheryScalers.TimeStep as "time.step"
FROM ((FisheryScalers INNER JOIN RunID ON FisheryScalers.RunID = RunID.RunID)
INNER JOIN Fishery ON FisheryScalers.FisheryID = Fishery.FisheryID)
WHERE RunID.SpeciesName = 'COHO' AND FisheryScalers.FisheryFlag in (7,8) AND Nz(MSFFisheryScaleFactor, 0) = 0 AND Nz(MSFQuota, 0) = 0;
