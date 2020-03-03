ClearAll["CANGUI`Utilities`*"];
ClearAll["CANGUI`Utilities`*`*"];


BeginPackage["CANGUI`Utilities`", (Get[#]; #)& /@ {"CANGUI`Utilities`BinaryProcessing`"}];

populateCANFileMetadata;
getCANMetadata;
fileNameToDateObject;
getDuration;
importDataFiles;
parseRawCANData;
GetPlotChoiceTimeSeries;
GPSPlot;
TimeSeriesSliceData;
TimeSeriesAddUnits;
TimeSeriesSelect;

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
			startTime = date - Quantity[TimeZoneOffset[-5, If[date < DateObject[{2020, 1, 1, 0}], Entity["TimeZone", "America/Chicago"], Entity["TimeZone", "America/New_York"]], date], "Hours"],
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
		TimeObject[Append[fileNameNumbers[[-2 ;; ]], 0.]],
		TimeZone -> If[fileNameNumbers[[1]] < 20,
			Entity["TimeZone", "America/Chicago"],
			Entity["TimeZone", "America/New_York"]
		]
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

importDataFiles[files_] := importDataFiles[files, Automatic];
importDataFiles[files: {__File}, startTime_] := importDataFiles[files[[All, 1]], startTime];
importDataFiles[File[file_String], startTime_] := importDataFiles[file, startTime];

importDataFiles[files:{__String}, startTime_] := Association[importDataFiles[#, startTime]& /@ files];
importDataFiles[file_String, startTimeIn_] := With[
	{
		rawBinaryData = BinaryReadList[file, binaryFileFormat],
		startTime = Replace[startTimeIn, {d_DateObject :> d, _ :> fileNameToDateObject[file]}]
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
		times = Join @@ KeyValueMap[(N @ Most @ Subdivide[#1, #1 + 0.99, #2])&, Counts[data[[All, 1]]]],
		values = data[[All, 2 ;;]]
	},
	TemporalData[Transpose[values], {times}]
];
parseRawCANData[___] := $Failed;

GetPlotChoiceTimeSeries::args = "Cannot fill Function expecting `` slots with `` arguments.";
GetPlotChoiceTimeSeries[plotChoice_Association, relevantCANData_] := Module[
	{keys, combinedMessageData0, combinedMessageData, function, maxSlot, minArguments, units, resamplingRate},
	
	keys = StringTrim @ StringSplit[plotChoice["ID"], ","];
	keys = FromDigits[#, 16]& /@ keys;
	
	(* Verify that there is some data for the given keys *)
	If[Not @ MemberQ[Flatten[Keys @ relevantCANData], Alternatives @@ keys],
		Return[Missing["NotAvailable"]];
	];
	
	combinedMessageData0 = Select[KeyTake[relevantCANData, keys], FreeQ[_Missing]];
	combinedMessageData = Join @@@ (Map[#["Components"]&] /@ Values[combinedMessageData0]);
	
	function = plotChoice["Function"];
	maxSlot = Max @ Cases[function, Slot[n_Integer] :> n, Infinity];
	combinedMessageData = Replace[
		{
			l_List /; (Length[l] >= maxSlot) :> function @@ l,
			x_ :> (Message[GetPlotChoiceTimeSeries::args, maxSlot, Length[x]]; $Failed)
		}
	] /@ combinedMessageData;
	
	resamplingRate = plotChoice["ResamplingRate"];
	If[resamplingRate =!= None,
		combinedMessageData = TimeSeriesResample[#, resamplingRate]& /@ combinedMessageData;
	];
	
	units = plotChoice["Units"];
	If[units =!= None,
		combinedMessageData = TimeSeriesAddUnits[combinedMessageData, units];
	];
	
	First @ combinedMessageData
];

Options[GPSPlot] := Options[GeoGraphics];
GPSPlot[gpsTimeSeries_TemporalData, opts : OptionsPattern[]] := With[
	{
		sliceData = TimeSeriesSliceData[gpsTimeSeries]
	},
	GeoGraphics[
		{
			{Black, Opacity[0.25], GeoPath[sliceData[[All, 2]]]},
			Function[
				{time, position, speed, accuracy},
				{Red, GeoStyling[Opacity[1]], GeoDisk[position, accuracy]}
			] @@@ Select[sliceData, #[[4]] < Quantity[100, "Meters"] &],
			Function[
				{time, position, speed, accuracy},
				{
					EdgeForm[
						Directive[
							Blend[
								{Blue, Red},
								QuantityMagnitude[speed, "Miles" / "Hours"] // Clip[#, {0, 80}]& // Rescale[#, {0, 80}]&
							]
						]
					],
					GeoStyling[Opacity[0]],
					Tooltip[
						GeoDisk[position, Quantity[50, "Meters"]],
						Column[{time, position, speed, accuracy}, Frame -> All]
					]
				}
			] @@@ sliceData
		},
		opts
	]
];

TimeSeriesSliceData::usage = "TimeSeriesSliceData[ts_TemporalData] produces a List of values for each of the path values in the time series";
TimeSeriesSliceData[td_TemporalData] := With[
	{times = td["Times"]},
	Transpose[Prepend[td["SliceData", times], DateObject /@ times]]
];

TimeSeriesAddUnits::usage = "TimeSeriesAddUnits[ts, units] adds the given units to the values of the TimeSeries via QuantityArray";
TimeSeriesAddUnits[l_List, units_] := TimeSeriesAddUnits[#, units]& /@ l;
TimeSeriesAddUnits[td_TemporalData, units_] := TimeSeries[N @ QuantityArray[td["Values"], units], {td["Times"]}];

TimeSeriesSelect::usage = "TimeSeriesSelect[tseries, crit] returns a list of TimeSeries portions from tseries for which crit returns True for each value.";
TimeSeriesSelect[ts_TemporalData, criteria_] := Module[
	{boolTS, peaks, windows},
	boolTS = TimeSeriesMap[criteria /* Boole, ts];
	(* TODO: RegularlySampledQ is required for FindPeaks *)
	Quiet[
		Check[
			peaks = FindPeaks[boolTS, 0, $MinMachineNumber, 0.5];
			windows = If[boolTS["FirstValue"] === 0,
				peaks["Times"],
				Join[{ts["FirstTime"]}, peaks["Times"], {ts["LastTime"]}]
			];
			,
			windows = {ts["FirstTime"], ts["LastTime"]};
			,
			{FindPeaks::arg}
		],
		{FindPeaks::arg, Join::heads}
	];
	windows = Partition[windows, 2];
	TimeSeriesWindow[ts, #] & /@ windows
];

End[];

EndPackage[];