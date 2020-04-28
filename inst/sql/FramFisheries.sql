SELECT FisheryID as "fram_fishery_id", 
  FisheryName as "fram_fishery_name",
  FisheryTitle as "fram_fishery_long_name"
FROM Fishery
WHERE UCASE(species) = 'COHO'
