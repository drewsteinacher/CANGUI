ClearAll["CANGUI`*"];
ClearAll["CANGUI`*`*"];

BeginPackage["CANGUI`",
	(Get[#]; #)& /@ {
		"CANGUI`Utilities`BinaryProcessing`",
		"CANGUI`Utilities`"
	}
];


CANGUI::usage = "CANGUI[] makes a GUI designed to make analyzing CAN data files easy";
CANMessageSpacePlot::usage = "CANMessageSpacePlot plots out which parts of the message space correspond to which messages";

Begin["`Private`"];

emptyPlotChoice = <|
	"ID" -> "",
	"Name" -> "",
	"Function" -> (#1&),
	"Units" -> None,
	"ResamplingRate" -> None,
	"Options" -> Hold[{}],
	"PlotRange" -> Full,
	"FrameLabel" -> "",
	"ReferenceLines" -> Hold[{}]
|>;


$plotChoiceDirectory = "PlotChoices";

CANGUI[] := DynamicModule[
	{
		metadata = populateCANFileMetadata["DataFiles"],
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
	metadata = populateCANFileMetadata["TestFiles"]
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
					(# -> TimeObject[#])& /@ Reverse @ Values[Select[metadata, #Date === dateChoice &][[All, "StartTime"]]],
					Appearance -> "Vertical"
				],
				SetterBar[
					Dynamic[endTime],
					(# -> TimeObject[#])& /@ Reverse @ Values[Select[metadata, #Date === dateChoice &][[All, "EndTime"]]],
					Appearance -> "Vertical"
				]
			},
			Frame -> All
		]
	],
	If[Xor @@ (MatchQ[#, _DateObject]& /@ {startTime, endTime}) && loadedFileDate =!= dateChoice,
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
		{"Units:", InputField[Dynamic[plotChoice["Units"]], Expression]},
		{"Resampling:", InputField[Dynamic[plotChoice["ResamplingRate"]], Expression]},
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
			If[MatchQ[{startTime, endTime}, {__DateObject}] && endTime > startTime,
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
	CopyToClipboard[Replace[combinedMessageData, {x_} :> x]],
	Method -> "Queued",
	Enabled -> True
];


plottingArea[Dynamic[canData_], Dynamic[metadata_], Dynamic[startTime_], Dynamic[endTime_], Dynamic[dateChoice_], Dynamic[plotChoice_], Dynamic[combinedMessageData_], Dynamic[relevantCANData_]] := Dynamic[
	
	If[MatchQ[{startTime, endTime}, {__DateObject}] && endTime > startTime,
		relevantCANData = KeySelect[canData, (First[#] <= endTime && Last[#] >= startTime)&];
		
		If[validPlotChoiceQ[plotChoice],
			
			(* TODO: Modularize this? *)
			Module[{keys, combinedMessageData0, frameLabel, units, resamplingRate},
				
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
				
				frameLabel = plotChoice["FrameLabel"];
				
				units = plotChoice["Units"];
				If[units =!= None,
					combinedMessageData = TimeSeriesAddUnits[combinedMessageData, units];
					frameLabel = Replace[
						{frameLabel, Quantity[None, units]},
						{
							{None | "", u_} -> u,
							{x_, u_} :> Row[{x, Row[{"(", u, ")"}]}, Spacer[1]]
						}
					];
				];
				
				resamplingRate = plotChoice["ResamplingRate"];
				If[resamplingRate =!= None,
					combinedMessageData = TimeSeriesResample[#, resamplingRate]& /@ combinedMessageData;
				];
				
				
				
				DateListPlot[
					combinedMessageData,
					Sequence @@ ReleaseHold[plotChoice["Options"]],
					FrameLabel -> {None, frameLabel},
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
		MatchQ[plotChoice["Units"], None | _?KnownUnitQ],
		MatchQ[plotChoice["ResamplingRate"], None | _?Positive],
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

Attributes[byteSequence] = {HoldAllComplete};
Attributes[nibble] = {HoldAllComplete};
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
		Alignment -> {Center, Center},
		ItemSize -> Full
	]

];

(* Avoids subsequent messages getting overridden while maintaining order *)
processMessages[x_] := Module[
	{flattened, ends, rest},
	flattened = Flatten[x];
	ends = Select[flattened, Last[#] === ""&];
	rest = Select[flattened, Last[#] =!= ""&];
	Join[rest, ends]
];

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


plotMessages[] := Flatten[getMessageRows[plotChoiceImport[]]];

getMessageRows[plotChoices_] := Module[
	{groupedMessageData},
	
	groupedMessageData = GroupBy[
		plotChoices,
		#ID&,
		Map[Join[#, <|"Bytes" -> getBytes[#Function], "Nibbles" -> getNibbles[#Function], "Bits" -> getBits[#Function]|>] &]
	];
	groupedMessageData = KeyMap[StringTrim[StringSplit[#, ","]] &, groupedMessageData];
	groupedMessageData = KeyValueMap[
		Function[{IDs, messages},
			Table[id -> message, {id, IDs}, {message, messages}]
		],
		groupedMessageData
	];
	groupedMessageData = Flatten @ groupedMessageData;
	groupedMessageData = GroupBy[groupedMessageData, First -> Last];
	groupedMessageData = KeySortBy[groupedMessageData, FromDigits[#, 16]&];
	KeyValueMap[processMessage, groupedMessageData]
];

getBytes[foo_] := Join[
	MinMax /@ Split[
		Sort[
			Complement[
				Cases[foo, Slot[byteNumber_Integer] :> fixByteCount[byteNumber], Infinity],
				Cases[foo, (bitGet | nibbleGet)[Slot[byteNumber_Integer], _] :> fixByteCount[byteNumber], Infinity]
			]
		],
		Less
	]
	(* TODO: Handle SlotSequence *)
	(*,
	Cases[foo, SlotSequence[byteNumber_Integer] :> fixByteCount /@ Range[byteNumber, Ceiling[byteNumber, 8]], Infinity]
	*)
];
fixByteCount = Function[# //. x_Integer /; x > $MaxByteCount :> (x - $MaxByteCount)];

getNibbles[foo_] := Cases[foo, nibbleGet[Slot[byteNumber_Integer], nibbleNumber_] :> {(byteNumber - 1) * 8 + nibbleNumber * 4}, Infinity];

getBits[foo_] := Cases[foo, bitGet[Slot[byteNumber_Integer], bitNumber_] :> {(byteNumber - 1)*8 + bitNumber}, Infinity];

processMessage[id_, plotChoices_] := addMessageRow[id, Sequence @@ Flatten[processPlotChoice /@ plotChoices]];

processPlotChoice = {
	Function[{start, end},
		byteSequence[start, end, #Name, Print[#Name];, Dataset[#]]
	] @@@ #Bytes,
	
	Function[{x}, nibble[x, #Name, Print[#Name];, Dataset[#]]] @@@ #Nibbles,
	
	(* TODO: Use a 1-3 character "ShortName" instead of "..." *)
	Function[{x}, bit[x, "...", Print[#Name];, Dataset[#]]] @@@ #Bits
	
}&;

addMessageRow[id_String, bitsAndBytes__] := Module[
	{r = row},
	Join[
		{addBit[r, -1, id]},
		Cases[{bitsAndBytes}, byteSequence[args__] :> addByteSequence[r, args]],
		Cases[{bitsAndBytes}, nibble[args__] :> addNibble[r, args]],
		Cases[{bitsAndBytes}, bit[args__] :> addBit[r, args]]
	]
];
addMessageRow[___] := {};

Attributes[addBit] = {HoldRest};
addBit[r_Integer?Positive, colPosition_Integer, buttonArguments__] := Block[
	{colCount = colPosition + 2, extraOptions},
	extraOptions = If[colPosition === -1,
		{"Appearance" -> "Frameless"},
		{}
	];
	{
		{r, colCount} -> messageSpaceButton[buttonArguments, Evaluate[Sequence @@ extraOptions]],
		If[colCount < ($MaxBitCount + 1),
			{r, colCount + 1} -> "",
			{}
		]
	}
];
addBit[args___] := {};

Attributes[addNibble] = {HoldRest};
addNibble[r_Integer?Positive, colPosition_Integer, buttonArguments__] := Block[
	{colCount = colPosition + 2, extraOptions},
	extraOptions = If[colPosition === -1,
		{"Appearance" -> "Frameless"},
		{}
	];
	{
		{r, colCount} -> messageSpaceButton[buttonArguments, Evaluate[Sequence @@ extraOptions]],
		If[colCount + 4 < ($MaxBitCount + 1),
			{r, colCount + 4} -> "",
			{}
		]
	}
];
addNibble[args___] := {};

Attributes[addByteSequence] = {HoldRest};
addByteSequence[r_Integer?Positive, byteStart_Integer, byteEnd_Integer, buttonArguments__, opts: OptionsPattern[]] := Block[
	{
		colCountStart = (8 * (byteStart - 1)) + 1 + 1,
		colCountEnd = (8 * (byteEnd - 1)) + 1 + 1
	},
	{
		{r, colCountStart} -> messageSpaceButton[buttonArguments, opts],
		If[colCountEnd + 8 < ($MaxBitCount + 1),
			{r, colCountEnd + 8} -> "",
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
