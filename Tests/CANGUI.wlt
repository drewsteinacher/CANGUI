BeginTestSection["CANGUI"];

VerificationTest[
	Get["CANGUI`"]
	,
	Null
	,
	TestID -> "a9a06ee2-536b-48f7-8d8a-abfc33264322"
];

VerificationTest[
	CANGUI`Private`getDuration["DataFiles\\04021105.DAT"]
	,
	Quantity[44, "Seconds"]
	,
	TestID -> "f9d246a8-d529-4833-a655-84a208eaeb82"
];

VerificationTest[
	CANGUI`Private`getCANMetadata["DataFiles\\04021105.DAT"]
	,
	<|
		"Date" -> DateObject[{2017, 4, 2}, "Day", "Gregorian", -6.],
		"StartTime" -> TimeObject[{11, 5}, TimeZone -> -6.],
		"EndTime" -> TimeObject[{11, 6, 0.}, TimeZone -> -6.],
		"Duration" -> Quantity[44, "Seconds"]
	|>,
	TestID -> "c12f07f7-dd10-4df8-b825-98e1277c4fd8"
];

VerificationTest[
	CANGUI`Private`populateCANFileMetadata["DataFiles_Smaller"]
	,
	<|
		"DataFiles_Smaller\\09011611.DAT" -> <|
			"Date" -> DateObject[{2017, 9, 1}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{16, 11}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{16, 20, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[567, "Seconds"]
		|>,
		"DataFiles_Smaller\\09011530.DAT" -> <|
			"Date" -> DateObject[{2017, 9, 1}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{15, 30}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{15, 34, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[256, "Seconds"]
		|>,
		"DataFiles_Smaller\\09011515.DAT" -> <|
			"Date" -> DateObject[{2017, 9, 1}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{15, 15}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{15, 23, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[486, "Seconds"]
		|>,
		"DataFiles_Smaller\\08311753.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 31}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{17, 53}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{18, 0, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[442, "Seconds"]
		|>,
		"DataFiles_Smaller\\08310927.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 31}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{9, 27}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{9, 35, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[456, "Seconds"]
		|>,
		"DataFiles_Smaller\\08301742.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 30}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{17, 42}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{17, 45, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[183, "Seconds"]
		|>,
		"DataFiles_Smaller\\08301729.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 30}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{17, 29}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{17, 36, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[431, "Seconds"]
		|>,
		"DataFiles_Smaller\\08300854.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 30}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{8, 54}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{9, 1, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[448, "Seconds"]
		|>,
		"DataFiles_Smaller\\08291919.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 29}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{19, 19}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{19, 29, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[611, "Seconds"]
		|>,
		"DataFiles_Smaller\\08291906.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 29}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{19, 6}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{19, 14, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[482, "Seconds"]
		|>,
		"DataFiles_Smaller\\08291826.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 29}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{18, 26}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{18, 32, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[385, "Seconds"]
		|>,
		"DataFiles_Smaller\\08290926.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 29}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{9, 26}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{9, 31, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[295, "Seconds"]
		|>,
		"DataFiles_Smaller\\08290925.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 29}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{9, 25}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{9, 26, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[61, "Seconds"]
		|>,
		"DataFiles_Smaller\\08281746.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 28}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{17, 46}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{17, 54, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[505, "Seconds"]
		|>,
		"DataFiles_Smaller\\08280932.DAT" -> <|
			"Date" -> DateObject[{2017, 8, 28}, "Day", "Gregorian", -6.],
			"StartTime" -> TimeObject[{9, 32}, TimeZone -> -6.],
			"EndTime" -> TimeObject[{9, 39, 0.}, TimeZone -> -6.],
			"Duration" -> Quantity[441, "Seconds"]
		|>
	|>,
	TestID -> "b09784e4-9168-4092-bc58-1f3dfd017d08"
];

VerificationTest[
	CANGUI`Private`parseRawCANData[
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




