BeginPackage["CANGUI`"];
ClearAll["CANGUI`*"];

CANGUI::usage = "CANGUI[] makes a GUI designed to make analyzing CAN data files easy";
signedInt::usage = "signedInt[bytes: {__Integer} | {__TemporalData}] converts bytes into a signed integer";
unsignedInt::usage = "unsignedInt[bytes: {__Integer} | {__TemporalData}] converts bytes into an unsigned integer";
bitGet::usage = "bitGet[td_TemporalData, k_Integer] gets the kth bit from the given TemporalData object";
nibbleGet::usage = "nibbleGet[byte_, n_Integer, offset_Integer:0] gets the nth nibble from the right in byte (with an optional offset)";
bitPlot::usage = "bitPlot[byte] plots the bits in the given byte in one plot";

CANMessageSpacePlot::usage = "CANMessageSpacePlot plots out which parts of the message space correspond to which messages";

Begin["`Private`"];

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


emptyPlotChoice = <|
	"ID" -> "",
	"Name" -> "",
	"Function" -> (#1&),
	"Options" -> Hold[{}],
	"PlotRange" -> Full,
	"FrameLabel" -> "",
	"ReferenceLines" -> Hold[{}]
|>;


$plotChoiceDirectory = "PlotChoices";



(* Converts numbers from Two's Complement *)
ClearAll[convertFromTC];
convertFromTC[x_Integer, b_:16] /; (x<=(2^(b-1))) := x;
convertFromTC[x_Integer, b_:16] /; (x>(2^(b-1))) := x - 2^b;
convertFromTC[x:(_Integer | _TemporalData), b_:16] := TimeSeriesMap[convertFromTC, x];


(* To change endianness, Reverse the byte order *)
ClearAll[signedInt];
signedInt[b:(_Integer | _TemporalData)] := signedInt[{b}];
signedInt[b:({__Integer} | {__TemporalData})] := convertFromTC[unsignedInt[b], 8 * Length[b]];


(* To change endianness, Reverse the byte order *)
ClearAll[unsignedInt];
unsignedInt[b:(_Integer | _TemporalData)] := b;
unsignedInt[b:({__Integer} | {__TemporalData})] := b . Reverse[Table[2^((i-1)*8), {i, Length[b]}]];


ClearAll[bitGet];
bitGet[b_TemporalData, k_Integer] := TimeSeriesMap[BitGet[#, k]&, b];


ClearAll[nibbleGet];
nibbleGet[byte_Integer, n_Integer, offset_Integer: 0] := BitAnd[BitShiftRight[byte, 4 * n + offset], 15];
nibbleGet[b_TemporalData, n_Integer, offset_Integer: 0] := TimeSeriesMap[nibbleGet[#, n, offset]&, b];



ClearAll[bitPlot];
Options[bitPlot] = Join[{"ScaleFactor" -> 1}, Options[DateListPlot]];
bitPlot[td_TemporalData, opts: OptionsPattern[bitPlot]] := With[
	{
		factor = OptionValue["ScaleFactor"],
		byteRange = Range[0, 7],
		spacing = 0.33,
		dateListPlotOptions = FilterRules[{opts}, Options[DateListPlot]]
	},
	DateListPlot[
		factor * ((2 * spacing * bitGet[td, #] + # - spacing) & /@ byteRange),
		ImageSize -> Large,
		PlotTheme -> "Detailed",
		Filling -> (# -> factor * (# - (1 + spacing)) & /@ byteRange),
		FrameTicks -> {{{factor * #, #} & /@ byteRange, Automatic}, Automatic},
		GridLines -> {Automatic, factor * byteRange},
		PlotRange -> {Automatic, factor * (MinMax[byteRange] + {-2 * spacing, 2 * spacing})},
		Sequence @@ dateListPlotOptions
	]
];



populateCANFileMetadata[directory_: Directory[]] := With[
	{
		files = FileNames[RegularExpression[StringRepeat["\\d", 8] <> ".DAT"], directory],
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
		fileNameNumbers = ToExpression[ StringJoin @@@ Partition[Characters @ FileBaseName[fileName], 2]]
	},
	With[
		{
			date = DateObject @ Prepend[fileNameNumbers[[ ;; 2]], 2017(*DateValue["Year"]*)],
			startTime = TimeObject @ fileNameNumbers[[-2 ;; ]],
			duration = getDuration[fileName]
		},
		<|
			"Date" -> date,
			"StartTime" -> startTime,
			"EndTime" -> startTime + Round[duration, Quantity["Minutes"]],
			"Duration" -> duration
		|>
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



importDataFiles[files:{__String}] := Association[importDataFiles /@ files];
importDataFiles[file_String] := With[
	{
		rawBinaryData = BinaryReadList[file, binaryFileFormat],
		startTime = TimeObject @ ToExpression[StringJoin @@@ Partition[Characters @ StringTake[FileBaseName[file], {5, -1}], 2]]
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



CANGUI[] := DynamicModule[
	{
		metadata = CANGUI`Private`populateCANFileMetadata["DataFiles"],
		dateChoice,
		startTime, endTime,
		(* TODO: Read this from a configuration file, add a saveConfiguration[] button? *)
		(* Also need Units, PlotOptions (Joined, Dashed, PlotStyle?, others lumped together), description? *)
		plotChoices = plotChoiceImport[],
		plotChoice = emptyPlotChoice,
		plotChoicePosition = -1,
		canData = <||>
	},
	plotChoicePosition = Length[plotChoices];
	Row[
		{
			dateTimeSelectionMenu[
				Dynamic[canData],
				Dynamic[metadata],
				Dynamic[dateChoice],
				Dynamic[startTime],
				Dynamic[endTime]
			],
			plotChoiceMenu[
				Dynamic[plotChoice],
				Dynamic[plotChoices],
				Dynamic[plotChoicePosition]
			],
			plottingAndStatusArea[
				Dynamic[canData],
				Dynamic[metadata],
				Dynamic[startTime],
				Dynamic[endTime],
				Dynamic[plotChoice],
				Dynamic[plotChoicePosition],
				Dynamic[dateChoice]
			]
		}
	],
	SynchronousUpdating -> False
];


dateTimeSelectionMenu[Dynamic[canData_], Dynamic[metadata_], Dynamic[dateChoice_], Dynamic[startTime_], Dynamic[endTime_]] := DynamicModule[
	{
		working = False,
		loadedFileDate = None
	},
	Column[
		{
			refreshMetadataButton[Dynamic[metadata]],
			Grid[{{
				datePopupMenu[
					Dynamic[metadata],
					Dynamic[dateChoice],
					Dynamic[startTime],
					Dynamic[endTime]
				],
				
				(* TODO: Modularize this? *)
				Dynamic[
					If[TrueQ[working],
						ProgressIndicator[Appearance -> "Percolate"],
						If[loadedFileDate === dateChoice,
							"Ready!",
							""
						]
					]
				]
			}}],
			tripSelectionMenu[
				Dynamic[metadata],
				Dynamic[startTime],
				Dynamic[endTime],
				Dynamic[dateChoice],
				Dynamic[canData],
				Dynamic[working],
				Dynamic[loadedFileDate]
			]
		},
		Frame -> All,
		Alignment -> Center
	]
];


refreshMetadataButton[Dynamic[metadata_]] := Button[
	"Refresh files",
	metadata = CANGUI`Private`populateCANFileMetadata["TestFiles"]
];



datePopupMenu[Dynamic[metadata_], Dynamic[dateChoice_], Dynamic[startTime_], Dynamic[endTime_]] := PopupMenu[
	Dynamic[
			dateChoice,
			(
				If[dateChoice=!=#,
					startTime = endTime = None;
				];
				dateChoice = #;
			)&
		],
	Reverse[Union[Values[metadata[[All, "Date"]]]]]
]



tripSelectionMenu[Dynamic[metadata_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[dateChoice_], Dynamic[canData_], Dynamic[working_], Dynamic[loadedFileDate_]] := DynamicWrapper[
	Dynamic[
		Grid[
			List @ {
				SetterBar[
					Dynamic[startTime],
					Reverse @ Values[Select[metadata, #Date == dateChoice &][[All, "StartTime"]]],
					Appearance -> "Vertical"
				],
				SetterBar[
					Dynamic[endTime],
					Reverse @ Values[Select[metadata, #Date == dateChoice &][[All, "EndTime"]]],
					Appearance -> "Vertical"
				]
			},
			Frame -> All
		]
	],
	If[Xor @@ (MatchQ[#, _TimeObject]& /@ {startTime, endTime}) && loadedFileDate =!= dateChoice,
		working = True;
		canData = importDataFiles[Keys[Select[metadata, #Date === dateChoice &]]];
		loadedFileDate = dateChoice;
		working = False;
	],
	SynchronousUpdating -> False,
	TrackedSymbols :> {startTime, endTime}
];


plotChoiceMenu[Dynamic[plotChoice_], Dynamic[plotChoices_], Dynamic[plotChoicePosition_]] := Dynamic[
	Column[
		{
			plotChooser[Dynamic[plotChoice], Dynamic[plotChoices], Dynamic[plotChoicePosition]],
			OpenerView[
				{
					"Details",
					Column[
						{
							plotChoiceEditor[Dynamic[plotChoice]],
							Row[
								{
									plotChoiceImportButton[
										Dynamic[plotChoice],
										Dynamic[plotChoices],
										Dynamic[plotChoicePosition]
									],
									plotChoiceRemoveButton[
										Dynamic[plotChoice],
										Dynamic[plotChoices],
										Dynamic[plotChoicePosition]
									],
									plotChoiceAddButton[
										Dynamic[plotChoice],
										Dynamic[plotChoices],
										Dynamic[plotChoicePosition]
									]
								}
							]
						},
						Frame -> All,
						Alignment -> Center
					]
				},
				Method -> "Active"
			]
		},
		Frame -> All,
		Alignment -> Center
	]
];


plotChooser[Dynamic[plotChoice_], Dynamic[plotChoices_], Dynamic[plotChoicePosition_]] := SetterBar[
	Dynamic[
		plotChoicePosition,
		(
			plotChoice = plotChoices[[#]];
			plotChoicePosition = First @ Flatten @ Position[plotChoices, plotChoice];
		)&
	],
	Thread[Range @ Length[plotChoices] -> ReplaceAll[
			StringTemplate["`Name` (ID: `ID`)"] /@ plotChoices[[All, {"Name", "ID"}]],
			" (ID: )" -> "New Choice"
		]
	],
	Appearance -> "Vertical" -> {Automatic, 2}
];


plotChoiceEditor[Dynamic[plotChoice_]] := Grid[
	{
		{"IDs:", InputField[Dynamic[plotChoice["ID"]], String]},
		{"Name:", InputField[Dynamic[plotChoice["Name"]], String]},
		{"Function:", InputField[Dynamic[plotChoice["Function"]], Expression]},
		{"Range:", InputField[Dynamic[plotChoice["PlotRange"]], Expression]},
		{"Label:", InputField[Dynamic[plotChoice["FrameLabel"]], String]},
		{"ReferenceLines:", InputField[Dynamic[plotChoice["ReferenceLines"]], Hold[Expression]]},
		{"Options:", InputField[Dynamic[plotChoice["Options"]], Hold[Expression]]}
	},
	Frame -> All
];



plotChoiceRemoveButton[Dynamic[plotChoice_], Dynamic[plotChoices_], Dynamic[plotChoicePosition_]] := Button[
	"Remove",
	If[ChoiceDialog["Are you sure you want to remove this?"],
		With[{newPlotChoices = SortBy[Complement[plotChoices, {plotChoice}], {#Name === "", #ID, #Name}&]},
			plotChoice = Last[newPlotChoices];
			plotChoices = newPlotChoices;
		]
	],
	Enabled -> Dynamic[Not @ newChoiceQ[plotChoicePosition, plotChoices]],
	Method -> "Queued"
];



plotChoiceAddButton[Dynamic[plotChoice_], Dynamic[plotChoices_], Dynamic[plotChoicePosition_]] := Button[
	Dynamic[
		If[newChoiceQ[plotChoicePosition, plotChoices], "Add", "Update"]
	],
	If[Not @ newChoiceQ[plotChoicePosition, plotChoices],
		plotChoices = Drop[plotChoices, {plotChoicePosition}];
	];
	AppendTo[plotChoices, plotChoice];
	plotChoices = validatePlotChoices[plotChoices];
	plotChoicePosition = First @ Flatten @ Position[plotChoices, plotChoice];
	plotChoiceExport[plotChoices];
	,
	Enabled -> Dynamic[
		And[
			TrueQ[validPlotChoiceQ[plotChoice]],
			plotChoices[[plotChoicePosition]] =!= plotChoice
		]
	]
];



newChoiceQ[plotChoicePosition_Integer, plotChoices:{___Association}]:= MatchQ[plotChoicePosition, -1 | Length[plotChoices]];


plotChoiceExport[plotChoices:{__Association}] := With[{dir = $plotChoiceDirectory},
	If[Not @ DirectoryQ[dir],
		CreateDirectory[dir]
	];
	SetDirectory[dir];
	(* TODO: Export these in a way so that they are formatted nicely for easy editing outside of the GUI *)
	Export[dir <> "_" <> StringReplace[DateString["ISODateTime"], ":" -> "-"] <> ".m", plotChoices];
	ResetDirectory[];
];


plotChoiceImport[] := Module[{dir = $plotChoiceDirectory, choices, file},
	If[Not @ DirectoryQ[dir],
		choices = {}
		,
		
		SetDirectory[dir];
		file = TakeLargestBy[FileNames[dir ~~ "_" ~~ __ ~~ ".m"], FileDate, 1];
		If[MatchQ[file, {_String}],
			choices = Import @ First @ file;,
			choices = {};
		];
		ResetDirectory[];
	];
	validatePlotChoices[choices]
];


plotChoiceImport[file_String] := validatePlotChoices @ Import[file];


validatePlotChoices[plotChoices:{___Association}] := Module[{newPlotChoices = plotChoices},
	newPlotChoices = Select[newPlotChoices, #Name =!= ""&];
	If[FreeQ[newPlotChoices, emptyPlotChoice],
		AppendTo[newPlotChoices, emptyPlotChoice];
	];
	newPlotChoices = Association[emptyPlotChoice, #]& /@ newPlotChoices;
	newPlotChoices = SortBy[newPlotChoices, {#Name === "", #ID, #Name}&];
	newPlotChoices
];


plotChoiceImportButton[Dynamic[plotChoice_], Dynamic[plotChoices_], Dynamic[plotChoicePosition_]] := Button[
	"Import...",
	If[ChoiceDialog["Are you sure you want to discard any unsaved changes?"],
		With[
			{
				file = SystemDialogInput[
					"FileOpen",
					{
						FileNameJoin[{Directory[], $plotChoiceDirectory}],
						{"Choices" -> {$plotChoiceDirectory <> "_" <> "*.m"}}
					}
				]
			},
			If[MatchQ[file, _String],
				plotChoices = plotChoiceImport[file];
				plotChoice = emptyPlotChoice;
				plotChoicePosition = Length[plotChoices];
			];
		];
	],
	Method -> "Queued"
];


plottingAndStatusArea[Dynamic[canData_], Dynamic[metadata_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[plotChoice_], Dynamic[plotChoicePosition_], Dynamic[dateChoice_]] := DynamicModule[
	{combinedMessageData, relevantCANData = {}},
	Row[
		{
			canInformationArea[
				Dynamic[metadata],
				Dynamic[startTime],
				Dynamic[endTime],
				Dynamic[plotChoice],
				Dynamic[plotChoicePosition],
				Dynamic[dateChoice],
				Dynamic[relevantCANData]
			],
			
			Column[
				{
					statusArea[
						Dynamic[canData],
						Dynamic[startTime],
						Dynamic[endTime],
						Dynamic[dateChoice],
						Dynamic[plotChoicePosition],
						Dynamic[combinedMessageData]
					],
					plottingArea[
						Dynamic[canData],
						Dynamic[metadata],
						Dynamic[startTime],
						Dynamic[endTime],
						Dynamic[dateChoice],
						Dynamic[plotChoice],
						Dynamic[combinedMessageData],
						Dynamic[relevantCANData]
					]
				}
			]
		}
	]
];


statusArea[Dynamic[canData_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[dateChoice_], Dynamic[plotChoicePosition_], Dynamic[combinedMessageData_]] := Dynamic[
	Row[
		{
			dateChoice,
			Row[Framed/@{startTime, endTime}],
			If[MatchQ[{startTime, endTime}, {__TimeObject}] && endTime > startTime,
				"Ready to plot!",
				"Nope, not ready to plot"
			],
			Framed @ plotChoicePosition,
			Framed @ Length[canData],
			copyToCliboardButton[Dynamic[combinedMessageData]]
		},
		Frame-> True
	]
];


copyToCliboardButton[Dynamic[combinedMessageData_]] := Button[
	"Copy",
	CopyToClipboard[combinedMessageData],
	Method -> "Queued",
	Enabled -> True
];


plottingArea[Dynamic[canData_], Dynamic[metadata_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[dateChoice_], Dynamic[plotChoice_], Dynamic[combinedMessageData_], Dynamic[relevantCANData_]] := Dynamic[
	
	If[MatchQ[{startTime, endTime}, {__TimeObject}] && endTime > startTime,
		relevantCANData = KeySelect[canData, (First[#] <= endTime && Last[#] >= startTime)&];
		
		If[validPlotChoiceQ[plotChoice],
			
			(* TODO: Modularize this? *)
			Module[{keys, combinedMessageData0},
				
				keys = StringTrim @ StringSplit[plotChoice["ID"], ","];
				keys = FromDigits[#, 16]& /@ keys;
				
				(* Verify that there is some data for the given keys *)
				If[Not @ MemberQ[Union[Flatten[Values[Keys /@ canData]]], Alternatives @@ keys],
					Return[StringTemplate["CAN IDs `ID` were not found in the selected data"][plotChoice], Module];
				];
				
				(* TODO: Find a better way than doubly-nested Values? *)
				combinedMessageData0 = Select[relevantCANData[[All, Key /@ keys]], FreeQ[_Missing]];
				combinedMessageData = Join @@@ (Map[#["Components"]&] /@ Values[Values[combinedMessageData0]]);
				combinedMessageData = plotChoice["Function"] @@@ combinedMessageData;
				
				DateListPlot[
					combinedMessageData,
					Sequence @@ ReleaseHold[plotChoice["Options"]],
					FrameLabel -> {None, plotChoice["FrameLabel"]},
					ImageSize -> Large,
					PlotRange -> {{startTime, endTime}, plotChoice["PlotRange"]},
					PlotRangePadding -> Scaled[0.05],
					Joined -> True,
					PlotLabel -> plotChoice["Name"],
					PlotTheme -> "Detailed"
					(* TODO: Hook up "ReferenceLines" *)
				]
			]
			,
			"Not ready to plot"
		],
		"Selected times are invalid"
	],
	TrackedSymbols :> {plotChoice, startTime, endTime},
	SynchronousUpdating -> False
];


canInformationArea[Dynamic[metadata_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[plotChoice_], Dynamic[plotChoicePosition_], Dynamic[dateChoice_], Dynamic[relevantCANData_]] := Column[
		{
			Dynamic[
				Grid[
					Join[
						{{"ID", "Hz"}},
						KeyValueMap[
							{ToUpperCase[IntegerString[#, 16]], #2} &,
							NumberForm[#, 2]& /@ Reverse @ Sort @ Merge[Map[getAverageMessageRate, Values[relevantCANData], {2}], Total]
						]
					],
					Frame -> All,
					Background -> {None, {LightGray}}
				]
			]
		}
	];


getAverageMessageRate[td_TemporalData] := Quiet[Replace[td["PathLength"]/ (Subtract @@ td /@ {"LastTime", "FirstTime"}), Except[_?NumberQ] -> \[Infinity]]];


validPlotChoiceQ[plotChoice_Association] := With[{ids = StringTrim @ StringSplit[plotChoice["ID"], ","]},
	And[
		StringLength[plotChoice["Name"]] > 0,
		Quiet[MatchQ[FromDigits[#, 16]& /@ ids, {__Integer}]],
		MatchQ[plotChoice["Function"], f_Function /; ContainsOnly[Cases[f ,_Slot, Infinity], Slot /@ Range[8 * Length[ids]]]],
		MatchQ[plotChoice["PlotRange"], Automatic | Full | All | {Repeated[Automatic | _?NumberQ, {2}]}],
		MatchQ[plotChoice["Options"], Hold[{___Rule}]]
	]
];


$MaxByteCount = 8;
$MaxBitCount = 8 * $MaxByteCount;

rowCount = 1;
row := rowCount++;

colCount = 1;
col := colCount++;

Attributes[byte] = {HoldAllComplete};
Attributes[bit] = {HoldAllComplete};

CANMessageSpacePlot[] := Module[{sa},
	colCount = rowCount = 1;
	sa = SparseArray[
		processMessages @ {
			topScale[],
			plotMessages[]
		},
		{rowCount - 1, ($MaxByteCount * 8) + 1},
		SpanFromLeft
	];
	
	Grid[
		Normal[sa],
		Frame -> All,
		Alignment -> {Center, Center}
	]

];

(* Avoids subsequent messages getting overridden while maintaining order *)
processMessages[x_] := Reverse @ DeleteDuplicatesBy[Reverse @ Flatten[x], First];

topScale[] := Module[
	{r},
	Flatten @ {
	(* Convenient scale at the top (done at bottom for simplicity for now) *)
		r = row;
		{r, 1} -> "ID",
		{r, (# - 1) * 8 + 2} -> messageSpaceButton[StringTemplate["Byte #``"][#], "Appearance" -> "Frameless"] & /@ (Range[$MaxByteCount]),
	
	
	(* Equivalent, but one might be faster? *)
		r = row;
		{r, 1} -> SpanFromAbove,
		{r, # + 1} -> messageSpaceButton[Replace[Mod[#, 8], 0 -> 8], "Appearance" -> "Frameless"] & /@ Range[$MaxByteCount * 8],
		
		r = row;
		{r, 1} -> SpanFromAbove,
		{r, # + 1} -> messageSpaceButton[#, "Appearance" -> "Frameless"] & /@ Range[$MaxByteCount * 8]
	}
];


plotMessages[] := Module[{r},
	Flatten @ {
		addMessageRow["0x002",
			bit[1, "AAA", Print["a"];, "AAAA"],
			bit[4, "B", Print["b"];, "BBB"]
		],
		
		addMessageRow["0x152",
			byte[2, 0, "CCC"],
			byte[1, 0, "DDD"]
		],
		
		addMessageRow["0x140",
			byte[3, 0, "short", Print["ZZZ"];, "tooltip"],
			bit[3, "B", Print["QQQ"];, "tooltip #2"]
		]
	}
];

addMessageRow[id_String, bitsAndBytes__] := Module[
	{r = row},
	Join[
		{addBit[r, 0, id]},
		Cases[{bitsAndBytes}, byte[args__] :> addByte[r, args]],
		Cases[{bitsAndBytes}, bit[args__] :> addBit[r, args]]
	]
];
addMessageRow[___] := {};

Attributes[addBit] = {HoldRest};
addBit[r_Integer?Positive, colPosition_Integer, buttonArguments__] := Block[
	{colCount = colPosition + 1, extraOptions},
	extraOptions = If[colPosition === 0,
		{"Appearance" -> "Frameless"},
		{}
	];
	{
		{r, col} -> messageSpaceButton[buttonArguments, Evaluate[Sequence @@ extraOptions]],
		If[colCount < 33,
			{r, col} -> "",
			{}
		]
	}
];
addBit[___] := {};

Attributes[addByte] = {HoldRest};
addByte[r_Integer?Positive, bytePosition_Integer, byteOffset_Integer, buttonArguments__, opts: OptionsPattern[]] := Block[
	{colCount = (8 * (bytePosition - 1)) + byteOffset + 1 + 1},
	{
		{r, colCount} -> messageSpaceButton[buttonArguments, opts],
		If[colCount + 8 < ($MaxBitCount + 1),
			{r, colCount + 8} -> "",
			{}
		]
	}
];

Attributes[messageSpaceButton] = {HoldRest};
Options[messageSpaceButton] = {
	"Appearance" -> Automatic
};
messageSpaceButton[label_, action_: Null, opts: OptionsPattern[]] := Button[label, action, "Appearance" -> OptionValue["Appearance"], FrameMargins -> None, opts];
messageSpaceButton[label_, action_: Null, tooltip: Except[_Rule], opts:OptionsPattern[]] := Tooltip[messageSpaceButton[label, action, opts], tooltip];


End[];
EndPackage[];
