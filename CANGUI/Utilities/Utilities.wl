ClearAll["CANGUI`Utilities`*"];
ClearAll["CANGUI`Utilities`*`*"];

BeginPackage["CANGUI`Utilities`"];

populateCANFileMetadata;
getCANMetadata;
fileNameToDateObject;
getDuration;
importDataFiles;
parseRawCANData;

Begin["`Private`"];


populateCANFileMetadata[directory_: Directory[]] := With[
	{
		files = FileNames[RegularExpression[StringRepeat["\\d", 8] <> "." <> StringRepeat["\\d", 2]<> "B"], directory],
		loadingTemplate = StringTemplate["Gathering metadata from `` files (`` Mb total)..."]
	},

	PrintTemporary[
		Internal`LoadingPanel[
			loadingTemplate[Length[files], Total[FileByteCount /@ files] / 1024.^2]
		]
	];

	AssociationMap[getCANMetadata, files] // SortBy[#, ({#Date, #StartTime}&)]& // Reverse
];

getCANMetadata[fileName_String] := With[
	{
		date = fileNameToDateObject[fileName]
	},
	With[
		{
			startTime = date,
			duration = getDuration[fileName]
		},
		<|
			"Date" -> CurrentDate[date, "Day"],
			"StartTime" -> startTime,
			"EndTime" -> startTime + Round[duration, Quantity["Minutes"]],
			"Duration" -> duration
		|>
	]
];

fileNameToDateObject[fileName_String] := With[
	{
		fileNameNumbers = FromDigits /@	StringCases[fileName, Repeated[DigitCharacter, 2]]
	},
	DateObject[
		{2000, 0, 0} + fileNameNumbers[[ ;; 3]],
		TimeObject[Append[fileNameNumbers[[-2 ;; ]], 0.]]
	]
];


(* See http://mathematica.stackexchange.com/questions/55536/get-the-last-line-from-each-of-a-large-number-of-files-transform-them-and-writ *)
getDuration[file_, maxLineBytes_: 14] := Module[{s, duration},

	s = OpenRead[file, BinaryFormat -> True];

	(* Move near the last line in the file *)
	Quiet[SetStreamPosition[s, -maxLineBytes], SetStreamPosition::stmrng];

	(* TODO: Better error handling if this information is bad? *)
	duration = Replace[
		BinaryRead[s, "UnsignedInteger16"],
		{
			{} -> $Failed,
			d_Integer :> Quantity[d, "Seconds"]
		}
	];
	Close[s];

	duration
];


(* This is the structure the Arduino writes to the SD card *)
binaryFileFormat = Flatten[
	{
		(* Time since file creation (seconds) *)
		"UnsignedInteger16",
		
		(* CAN ID (remember, it gets converted to decimal here) *)
		"UnsignedInteger32",
		
		(* Actual data *)
		ConstantArray["UnsignedInteger8", {8}]
	}
];

importDataFiles[files: {__File}] := importDataFiles[files[[All, 1]]];
importDataFiles[File[file_String]] := importDataFiles[file];

importDataFiles[files:{__String}] := Association[importDataFiles /@ files];
importDataFiles[file_String] := With[
	{
		rawBinaryData = BinaryReadList[file, binaryFileFormat],
		startTime = fileNameToDateObject[file]
	},

	Rule[
		{startTime, DatePlus[startTime, {rawBinaryData[[-1, 1]], "Second"}]},

	(* Group by CAN ID, create TemporalData objects for them, shifted to match the start time *)
		GroupBy[
			rawBinaryData,
			#[[2]]& -> (Drop[#, {2}]&),
			TimeSeriesShift[parseRawCANData[#], {{AbsoluteTime @ startTime}}]&
		] // KeySort
	]
];
importDataFiles[___] := $Failed;



parseRawCANData[data: {{__Integer}..}] := With[
	{
		(* Keep everything in order and unique *)
		times = Join @@ KeyValueMap[(N @ Most @ Subdivide[#1, #1 + 1, #2])&, Counts[data[[All, 1]]]],
		values = data[[All, 2 ;;]]
	},
	TemporalData[Transpose[values], {times}]
];
parseRawCANData[___] := $Failed;

End[];

EndPackage[];