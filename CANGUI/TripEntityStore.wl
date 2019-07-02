ClearAll["CANGUI`TripEntityStore`*"];
ClearAll["CANGUI`TripEntityStore`*`*"];

BeginPackage["CANGUI`TripEntityStore`",
	(Get[#]; #)& /@ {
		"CANGUI`",
		"CANGUI`Utilities`BinaryProcessing`",
		"CANGUI`Utilities`"
	}
];

CreateTripEntityStore;
$StopSpeedThreshold = Quantity[15., "Miles" / "Hours"];

Begin["`Private`"];

CreateTripEntityStore::usage = "CreateTripEntityStore[directory] creates an EntityStore from the trip files in the given directory";

CreateTripEntityStore[dataDirectory_String ? DirectoryQ, plotChoiceDirectory : _String ? DirectoryQ : None] := With[
	{
		tripEntityData = getTripData[dataDirectory],
		tripPropertyData = getPropertiesFromPlotChoices[plotChoiceDirectory]
	},
	EntityStore[
		{
			"Trip" -> <|
				"Entities" -> tripEntityData,
				"Properties" -> <|
					"Label" -> <|
						"Label" -> "label",
						"DefaultFunction" -> EntityFramework`BatchApplied[
							RightComposition[
								CanonicalName,
								StringSplit[#, ":"]&,
								(StringTemplate["Trip #`2` on `1`"] @@@ #)&
							]
						]
					|>,
					"File" -> <|
						"Label" -> "file"
					|>,
					"Date" -> <|
						"Label" -> "date"
					|>,
					"StartTime" -> <|
						"Label" -> "start time"
					|>,
					"EndTime" -> <|
						"Label" -> "end time"
					|>,
					"Duration" -> <|
						"Label" -> "duration",
						"FormattingFunction" -> Function[
							amount,
							N @ Which[
								amount < Quantity[1, "Minutes"],
								UnitConvert[amount, "Seconds"],
								
								amount < Quantity[1, "Hour"],
								UnitConvert[amount, "Minutes"],
								
								True,
								UnitConvert[amount, "Hours"]
							]
						]
					|>,
					"TripNumber" -> <|
						"Label" -> "trip number"
					|>,
					"RawData" -> <|
						"Label" -> "raw data",
						"DefaultFunction" -> EntityFramework`CachedEntityFunction[
							EntityFramework`BatchApplied @ Function[
								entities,
								entities // RightComposition[
									EntityValue[#, {"File", "StartTime"}]&,
									importDataFiles @@@ # &,
									Values,
									Function[
										listOfAssociations,
										MapThread[
											Function[
												{entity, idToTimeSeries},
												entity["EndTime"] = First @ TakeLargest[#["LastDate"]& /@ Values[idToTimeSeries], 1];
												entity["Duration"] = Subtract @@ entity[{"EndTime", "StartTime"}];
												idToTimeSeries
											],
											{entities, listOfAssociations}
										]
									]
								]
							],
							"UseFileCache" -> False,
							"UpdateInterval" -> Quantity[5, "Minutes"]
						]
					|>,
					"GPSTimeSeries" -> <|
						"Label" -> "GPS time series",
						"DefaultFunction" -> EntityProperty["Trip", "GPSTimeSeries", {"AdditionalProperties" -> {}}]
					|>,
					{"GPSTimeSeries", {"AdditionalProperties" -> _List}} -> <|
						"Label" -> "GPS time series",
						"DefaultFunction" -> EntityFramework`BatchApplied[
							Function[
								{entities, qualifiers},
								Module[
									{gpsTimeSeries, properties, propertyLabels},
									
									(* Get GPS information, update start/end times *)
									gpsTimeSeries = entities // RightComposition[
										(* TODO: Pass databin ID in via an option? *)
										getGPSTimeSeries["ilkxZTEP"],
										{entities, #1}&,
										MapThread[
											Function[
												{entity, timeSeries},
												If[MatchQ[timeSeries, _TemporalData],
													
													entity["StartTime"] = timeSeries["FirstDate"];
													entity["EndTime"] = timeSeries["LastDate"];
													
													entity["StartPosition"] = timeSeries["FirstValue"];
													entity["EndPosition"] = timeSeries["LastValue"];
												];
												timeSeries
											]
										]
									];
									
									(* Gather other properties, integrate into one TemporalData object *)
									properties = qualifiers["AdditionalProperties"];
									If[Length[properties] > 0,
										propertyLabels = Replace[p_EntityProperty :> StringTrim[p["Label"], " time series"]] /@ properties;
										gpsTimeSeries = MapThread[
											Function[
												{gpsTS, newPropertyTSs},
												Module[
													{combinedTSList, validTSList, labels},
													
													combinedTSList = Join[gpsTS["Components"], newPropertyTSs];
													
													validTSList = TemporalData`TemporalDataQ /@ combinedTSList;
													
													combinedTSList = Pick[combinedTSList, validTSList];
													labels = Pick[Join[gpsTS["MetaInformation"]["Labels"], propertyLabels], validTSList];
													
													TemporalData[combinedTSList, MetaInformation -> <|"Labels" -> labels|>]
												]
											],
											{gpsTimeSeries, EntityValue[entities, properties]}
										]
									];
									
									gpsTimeSeries
								]
							],
							BatchSize -> 100
						]
					|>,
					"StartPosition" -> <|
						"Label" -> "start position",
						"DefaultFunction" -> EntityFramework`BatchApplied[
							RightComposition[
								EntityProperty["Trip", "GPSTimeSeries"],
								Map[Replace[td_TemporalData :> td["FirstValue"]]]
							]
						]
					|>,
					"EndPosition" -> <|
						"Label" -> "end position",
						"DefaultFunction" -> EntityFramework`BatchApplied[
							RightComposition[
								EntityProperty["Trip", "GPSTimeSeries"],
								Map[Replace[td_TemporalData :> td["LastValue"]]]
							]
						]
					|>,
					"StoppedTimes" -> <|
						"Label" -> "stopped times",
						"DefaultFunction" -> Function[
							entity,
							{#["FirstTime"], #["LastTime"]}& /@ TimeSeriesSelect[entity["SpeedometerTimeSeries"], LessThan[$StopSpeedThreshold]]
						]
					|>,
					tripPropertyData
				|>
			|>
		}
	]
];

CreateTripEntityStore::invalid = "Invalid directory ``";
CreateTripEntityStore::duplicates = "The following properties are duplicates: ``";
CreateTripEntityStore[directory_, ___] := (Message[CreateTripEntityStore::invalid, directory]; $Failed);

getTripData[dataDirectory_String] := Module[
	{fileToMetadata, entityData},

	fileToMetadata = populateCANFileMetadata[dataDirectory];

	entityData = KeyValueMap[Prepend[#2, "File" -> File[AbsoluteFileName[#1]]]&, fileToMetadata];
	entityData = GroupBy[entityData, Lookup["Date"], SortBy[Lookup["StartTime"]]];

	Values[entityData] // RightComposition[
		Map[
			MapIndexed[
				Rule[
					StringTemplate["``:``"][DateString[Lookup[#1, "Date"], "ISODate"], First @ #2],
					Append[#1, "TripNumber" -> First @ #2]
				]&
			]
		],
		Flatten,
		Association
	]
];

getPropertiesFromPlotChoices[plotChoicesDirectory_String] := Module[
	{choices, properties, canonicalNameToMultipleChoices},
	
	choices = importLatestPlotChoiceFile[plotChoicesDirectory];
	choices = Select[choices, StringLength[#Name] > 0&];
	
	canonicalNameToMultipleChoices = choices // RightComposition[
		GroupBy[getPropertyCanonicalNameFromPlotChoiceName[#Name]&],
		Select[Length[#] > 1&],
		KeyValueMap[
			Message[CreateTripEntityStore::duplicates, KeyTake[#2, {"ID", "Name"}]]&
		]
	];
	
	properties = getPropertyEntryFromPlotChoice /@ choices;
	Flatten[properties]
];
getPropertiesFromPlotChoices[___] := <||>;

importLatestPlotChoiceFile[plotChoicesDirectory_String] := Module[
	{file, choices},
	
	SetDirectory[plotChoicesDirectory];
	file = TakeLargestBy[FileNames[plotChoicesDirectory ~~ "_" ~~ __ ~~ ".m"], FileDate, 1];
	If[MatchQ[file, {_String}],
		choices = Import @ First @ file;,
		choices = {};
	];
	ResetDirectory[];
	
	choices
];

getPropertyEntryFromPlotChoice = Function[
	plotChoice,
    Module[
		{longerName, propertyCanonicalName},
	    longerName = plotChoice["Name"] <> " time series";
	    propertyCanonicalName = getPropertyCanonicalNameFromPlotChoiceName[longerName];
	    {
		    Rule[
				propertyCanonicalName,
				<|
					plotChoice,
					"Label" -> getPropertyLabelFromPlotChoiceName[longerName],
					"DefaultFunction" -> Function[entity,
						entity // RightComposition[
							#["RawData"]&,
							GetPlotChoiceTimeSeries[plotChoice, {#}]&
						]
					],
					"PlotChoice" -> plotChoice
				|>
			],
		    Rule[
			    {propertyCanonicalName, "AggregationFunction" -> _},
			    <|
				    "Label" -> getPropertyLabelFromPlotChoiceName[plotChoice["Name"]],
				    "DefaultFunction" -> Function[
					    {entity, qualifiers},
					    With[
						    {
								agg = Lookup[qualifiers, "AggregationFunction"],
								timeSeries = entity[propertyCanonicalName]
							},
					        Replace[
								timeSeries,
								{
									td_TemporalData ? TemporalData`TimeSeriesQ :> (
										entity[EntityProperty["Trip", propertyCanonicalName, {"AggregationFunction" -> agg}]] = agg[td]
									),
									_ -> $Failed
								}
							]
					    ]
				    ]
			    |>
		    ]
	    }
	]
];

getPropertyCanonicalNameFromPlotChoiceName = RightComposition[
	StringReplace[{"%" -> "Percent", "+" -> "Plus"}],
	StringReplace[Except[LetterCharacter | DigitCharacter] -> " "],
	StringReplace[Whitespace -> " "],
	StringSplit /* Capitalize /* StringJoin
];

(* TODO: Improve the labels to be mostly lowercase to match other properties? *)
getPropertyLabelFromPlotChoiceName = RightComposition[
	StringReplace[lc: LetterCharacter ~~ "?" :> lc <> " (?) "],
	StringReplace[Whitespace -> " "]
];



getGPSTimeSeries[databinID_String][entities : {__Entity}] := Module[
	{dateRange, data, splitTrips},
	
	dateRange = getDateRange[entities];
	
	data = Normal @ Dataset @ Databin[databinID, dateRange, {"type", "t", "loc", "speed", "accuracy"}];
	data = DeleteDuplicatesBy[data, KeyTake[{"type", "t"}]];
	
	(* TODO: Actually figure out time zone nonsense instead of resorting to using the "Timestamp" *)
	data = Append[#, "t" -> #Timestamp] & /@ data;
	
	data = data // Select[
		RightComposition[
			Lookup[{"type", "t", "loc", "speed", "accuracy"}],
			MatchQ[{_String, _DateObject, _GeoPosition, _Quantity, _Quantity}]
		]
	];
	splitTrips = Split[data, #1["type"] =!= "end" &];
	splitTrips = TemporalData[
		Transpose[Lookup[#, {"loc", "speed", "accuracy"}]],
		{Lookup[#, "t"]},
		ResamplingMethod -> None,
		MetaInformation -> <|
			"Labels" -> {"Position", "Speed", "Accuracy"}
		|>
	]& /@ splitTrips;
	
	findMatchingTrips[splitTrips] /@ entities
	
];

getDateRange = RightComposition[
	EntityValue[#, {"StartTime", "EndTime"}]&,
	Flatten /* MinMax,
	Map[DayRound],
	Apply[{#1, #2 + Quantity[1, "Days"]} &],
	ReplaceAll[DateObject[{y_, m_, d_}, ___] :> DateObject[{y, m, d, 0}]]
];

findMatchingTrips[splitTrips_, tolerance : _ : Quantity[16, "Minutes"]] := Function[
	entity,
	With[
		{
			tripStart =	entity["StartTime"] - tolerance,
			tripEnd = entity["EndTime"] + tolerance
		},
		splitTrips // RightComposition[
			Select[
				And[
					Between[{tripStart, tripEnd}][#["FirstDate"]],
					Between[{tripStart, tripEnd}][#["LastDate"]]
				]&
			],
			Replace[
				{
					td: {__TemporalData} :> Fold[TimeSeriesInsert, td],
					_ -> Missing["NotAvailable"]
				}
			]
		]
	]
];

End[];

EndPackage[];