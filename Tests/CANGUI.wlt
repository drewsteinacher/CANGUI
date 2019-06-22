BeginTestSection["CANGUI"];

VerificationTest[
	Get["CANGUI`"]
	,
	Null
	,
	TestID -> "a9a06ee2-536b-48f7-8d8a-abfc33264322"
];

VerificationTest[
	getDuration["DataFiles\\17040211.05B"]
	,
	Quantity[44, "Seconds"]
	,
	TestID -> "f9d246a8-d529-4833-a655-84a208eaeb82"
];

VerificationTest[
	getCANMetadata["DataFiles\\17040211.05B"]
	,
	<|
		"Date" -> DateObject[{2017, 4, 2}, "Day", "Gregorian", _],
		"StartTime" -> DateObject[{2017, 4, 2, 11, 5, 0.}, "Instant", "Gregorian",
			"America/Chicago"],
		"EndTime" -> DateObject[{2017, 4, 2, 11, 6, 0.}, "Instant", "Gregorian",
			"America/Chicago"],
		"Duration" -> Quantity[44, "Seconds"]
	|>,
	SameTest -> MatchQ,
	TestID -> "c12f07f7-dd10-4df8-b825-98e1277c4fd8"
];

VerificationTest[
	populateCANFileMetadata["DataFiles_Smaller"]
	,
	<|
		"DataFiles_Smaller\\19031716.30B" -> <|
			"Date" -> DateObject[{2019, 3, 17}, "Day", "Gregorian", -4.],
			"StartTime" -> DateObject[{2019, 3, 17, 16, 30, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime" -> DateObject[{2019, 3, 17, 16, 39, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[522, "Seconds"]
		|>,
		"DataFiles_Smaller\\19031715.33B" -> <|
			"Date" -> DateObject[{2019, 3, 17}, "Day", "Gregorian", -4.],
			"StartTime" -> DateObject[{2019, 3, 17, 15, 33, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime" -> DateObject[{2019, 3, 17, 15, 47, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[854, "Seconds"]
		|>,
		"DataFiles_Smaller\\19031419.12B" -> <|
			"Date" -> DateObject[{2019, 3, 14}, "Day", "Gregorian", -4.],
			"StartTime" -> DateObject[{2019, 3, 14, 19, 12, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2019, 3, 14, 19, 24, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[725, "Seconds"]
		|>,
		"DataFiles_Smaller\\19031409.45B" -> <|
			"Date" -> DateObject[{2019, 3, 14}, "Day", "Gregorian", -4.],
			"StartTime" -> DateObject[{2019, 3, 14, 9, 45, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime" -> DateObject[{2019, 3, 14, 9, 54, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[557, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120917.06B" -> <|
			"Date" -> DateObject[{2018, 12, 9}, "Day", "Gregorian", -4.],
			"StartTime" -> DateObject[{2018, 12, 9, 16, 6, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime" -> DateObject[{2018, 12, 9, 16, 21, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[896, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120916.53B" -> <|
			"Date" -> DateObject[{2018, 12, 9}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 9, 15, 53, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 9, 15, 55, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[129, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120916.21B" -> <|
			"Date" -> DateObject[{2018, 12, 9}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 9, 15, 21, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 9, 15, 25, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[234, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120915.39B" -> <|
			"Date" -> DateObject[{2018, 12, 9}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 9, 14, 39, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 9, 14, 43, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[242, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120914.11B" -> <|
			"Date" -> DateObject[{2018, 12, 9}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 9, 13, 11, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 9, 13, 34, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1405, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120817.12B" -> <|
			"Date" -> DateObject[{2018, 12, 8}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 8, 16, 12, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 8, 16, 21, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[544, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120816.37B" -> <|
			"Date" -> DateObject[{2018, 12, 8}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 8, 15, 37, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 8, 15, 49, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[725, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120815.54B" -> <|
			"Date" -> DateObject[{2018, 12, 8}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 8, 14, 54, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 8, 15, 19, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1494, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120815.41B" -> <|
			"Date" -> DateObject[{2018, 12, 8}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 8, 14, 41, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 8, 14, 45, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[231, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120814.17B" -> <|
			"Date" -> DateObject[{2018, 12, 8}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 8, 13, 17, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 8, 13, 29, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[735, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120719.41B" -> <|
			"Date" -> DateObject[{2018, 12, 7}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 7, 18, 41, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 7, 19, 2, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1247, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120711.15B" -> <|
			"Date" -> DateObject[{2018, 12, 7}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 7, 10, 15, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 7, 10, 32, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1035, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120610.51B" -> <|
			"Date" -> DateObject[{2018, 12, 6}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 6, 9, 51, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 6, 10, 4, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[766, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120519.31B" -> <|
			"Date" -> DateObject[{2018, 12, 5}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 5, 18, 31, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 5, 18, 41, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[586, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120519.06B" -> <|
			"Date" -> DateObject[{2018, 12, 5}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 5, 18, 6, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 5, 18, 23, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1026, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120510.51B" -> <|
			"Date" -> DateObject[{2018, 12, 5}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 5, 9, 51, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 5, 10, 6, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[895, "Seconds"]
		|>,
		"DataFiles_Smaller\\18120419.10B" -> <|
			"Date" -> DateObject[{2018, 12, 4}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 12, 4, 18, 10, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 12, 4, 18, 28, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1075, "Seconds"]
		|>,
		"DataFiles_Smaller\\18083012.43B" -> <|
			"Date" -> DateObject[{2018, 8, 30}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 30, 12, 43, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 30, 12, 54, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[675, "Seconds"]
		|>,
		"DataFiles_Smaller\\18083009.36B" -> <|
			"Date" -> DateObject[{2018, 8, 30}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 30, 9, 36, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 30, 9, 46, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[613, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082920.56B" -> <|
			"Date" -> DateObject[{2018, 8, 29}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 29, 20, 56, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 29, 21, 5, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[518, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082920.30B" -> <|
			"Date" -> DateObject[{2018, 8, 29}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 29, 20, 30, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 29, 20, 49, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1167, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082920.01B" -> <|
			"Date" -> DateObject[{2018, 8, 29}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 29, 20, 1, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 29, 20, 17, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[974, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082918.38B" -> <|
			"Date" -> DateObject[{2018, 8, 29}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 29, 18, 38, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 29, 18, 51, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[754, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082909.56B" -> <|
			"Date" -> DateObject[{2018, 8, 29}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 29, 9, 56, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 29, 10, 9, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[785, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082821.17B" -> <|
			"Date" -> DateObject[{2018, 8, 28}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 28, 21, 17, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 28, 21, 31, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[820, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082820.13B" -> <|
			"Date" -> DateObject[{2018, 8, 28}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 28, 20, 13, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 28, 20, 18, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[276, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082819.17B" -> <|
			"Date" -> DateObject[{2018, 8, 28}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 28, 19, 17, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 28, 19, 35, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[1055, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082814.34B" -> <|
			"Date" -> DateObject[{2018, 8, 28}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 28, 14, 34, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 28, 14, 49, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[920, "Seconds"]
		|>,
		"DataFiles_Smaller\\18082718.10B" -> <|
			"Date" -> DateObject[{2018, 8, 27}, "Day", "Gregorian", -4.],
			"StartTime"-> DateObject[{2018, 8, 27, 18, 10, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"EndTime"-> DateObject[{2018, 8, 27, 19, 27, 0.}, "Instant", "Gregorian", "America/Chicago"],
			"Duration" -> Quantity[4644, "Seconds"]
		|>
	|>,
	TestID -> "b09784e4-9168-4092-bc58-1f3dfd017d08"
];

VerificationTest[
	parseRawCANData[
		{
			{0, 338, 225, 108, 0, 0, 0, 0, 24, 128},
			{0, 328, 14, 21, 169, 136, 65, 0, 255, 5},
			{0, 320, 0, 0, 103, 72, 0, 0, 26, 161},
			{1, 208, 248, 255, 0, 0, 0, 0, 3, 252},
			{1, 2, 0, 2, 112, 5, 119, 0, 0, 0},
			{1, 211, 0, 6, 224, 15, 0, 0, 114, 253},
			{1, 210, 0, 0, 255, 255, 0, 0, 0, 0},
			{1, 320, 0, 5, 237, 71, 0, 0, 27, 161},
			{1, 212, 0, 0, 0, 0, 0, 0, 0, 0},
			{1, 208, 248, 255, 1, 0, 0, 0, 2, 253},
			{1, 321, 87, 38, 11, 41, 135, 135, 32, 0},
			{1, 329, 126, 1, 63, 170, 170, 66, 0, 0},
			{1, 321, 88, 38, 38, 41, 93, 199, 32, 0},
			{1, 342, 1, 0, 0, 0, 0, 0, 0, 52},
			{1, 321, 90, 38, 65, 41, 35, 199, 32, 0},
			{1, 329, 126, 1, 63, 172, 172, 66, 0, 0},
			{1, 2, 0, 2, 112, 7, 121, 0, 0, 0},
			{1, 2, 0, 2, 112, 15, 129, 0, 0, 0},
			{1, 320, 0, 2, 207, 70, 0, 0, 29, 161},
			{1, 320, 0, 6, 201, 70, 0, 0, 29, 33},
			{1, 329, 126, 1, 63, 172, 172, 66, 0, 0},
			{1, 864, 112, 6, 52, 52, 26, 0, 12, 0},
			{1, 328, 14, 28, 186, 134, 65, 0, 255, 5},
			{1, 2, 0, 2, 112, 1, 115, 0, 0, 0},
			{1, 329, 126, 1, 63, 171, 171, 66, 0, 0},
			{1, 208, 248, 255, 1, 0, 0, 0, 2, 253},
			{1, 328, 14, 28, 192, 134, 65, 0, 255, 5},
			{2, 209, 0, 0, 2, 0, 0, 0, 1, 253},
			{2, 342, 1, 0, 0, 0, 0, 0, 0, 52},
			{2, 324, 192, 0, 86, 197, 106, 164, 40, 0},
			{2, 320, 0, 4, 232, 70, 0, 0, 30, 161},
			{2, 328, 14, 18, 223, 134, 65, 0, 255, 5},
			{2, 209, 0, 0, 1, 0, 0, 0, 2, 253},
			{2, 209, 0, 0, 1, 0, 0, 0, 1, 253},
			{2, 209, 0, 0, 1, 0, 0, 0, 3, 253},
			{2, 209, 0, 0, 1, 0, 0, 0, 1, 253},
			{2, 209, 0, 0, 1, 0, 0, 0, 3, 253},
			{2, 320, 0, 6, 14, 71, 0, 0, 31, 161},
			{2, 329, 126, 1, 63, 173, 171, 68, 0, 0},
			{2, 328, 14, 24, 17, 135, 65, 0, 255, 5},
			{2, 329, 126, 1, 63, 173, 171, 68, 0, 0},
			{2, 208, 248, 255, 1, 0, 0, 0, 3, 252},
			{2, 321, 91, 38, 31, 42, 10, 135, 32, 0},
			{2, 329, 126, 1, 63, 174, 171, 69, 0, 0},
			{2, 977, 56, 48, 0, 0, 253, 0, 0, 0},
			{2, 865, 0, 41, 0, 217, 15, 144, 143, 114},
			{2, 320, 0, 12, 22, 71, 0, 0, 32, 161},
			{2, 864, 99, 7, 52, 52, 28, 0, 12, 0},
			{2, 324, 192, 1, 87, 197, 106, 164, 40, 0},
			{3, 321, 90, 38, 53, 42, 22, 199, 32, 0},
			{3, 329, 126, 1, 63, 175, 171, 70, 0, 0},
			{3, 977, 58, 48, 0, 0, 253, 0, 0, 0},
			{3, 886, 125, 57, 0, 217, 5, 16, 145, 114},
			{3, 880, 0, 0, 0, 0, 253, 9, 0, 0},
			{3, 321, 90, 38, 68, 42, 24, 135, 32, 0},
			{3, 321, 90, 38, 70, 42, 26, 135, 32, 0},
			{3, 209, 0, 0, 0, 0, 0, 0, 1, 253},
			{3, 320, 0, 11, 36, 71, 0, 0, 32, 161},
			{3, 211, 0, 6, 192, 15, 0, 0, 227, 253},
			{3, 642, 101, 80, 0, 111, 111, 3, 34, 0},
			{3, 2, 0, 2, 112, 3, 117, 0, 0, 0},
			{3, 328, 14, 21, 31, 135, 65, 0, 255, 5},
			{3, 881, 0, 0, 0, 64, 253, 250, 255, 0}
		}
	]
	,
	_TemporalData
	,
	SameTest -> MatchQ,
	TestID -> "d6f25b21-d762-443b-9604-75d1f21a9a4f"
];

VerificationTest[
	nibbleGet[97, 0]
	,
	1
	,
	TestID -> "7a0d61d0-2d66-448d-a076-aa8fd1a4722d"
];

VerificationTest[
	nibbleGet[97, 1]
	,
	6
	,
	TestID -> "46abb517-4e7e-4f5b-b6b2-3be6a91bd589"
];

VerificationTest[
	And @@ (nibbleGet[#, 0] + 16 nibbleGet[#, 1] === # & /@ Range[1, 255])
	,
	True
	,
	TestID -> "5f1b1fca-fd37-4ffe-b6a0-aefa99fc8f72"
];

VerificationTest[
	With[{nibble = nibbleGet[97, 1, -1]},
		nibble === FromDigits[StringPadLeft[IntegerString[nibble, 2], 4, "0"], 2]
	]
	,
	True
	,
	TestID -> "5ef58d3e-972e-432a-a244-9df740c4c2bf"
];




