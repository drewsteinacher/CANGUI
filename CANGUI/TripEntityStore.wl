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
						"Label" -> "start time"
					|>,
					"Duration" -> <|
						"Label" -> "duration"
					|>,
					"TripNumber" -> <|
						"Label" -> "trip number"
					|>,
					"RawData" -> <|
						"Label" -> "raw data",
						"DefaultFunction" -> EntityFramework`BatchApplied[
							Function[
								entities,
								entities // RightComposition[
									EntityProperty["Trip", "File"],
									importDataFiles,
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
							]
						]
					|>,
					tripPropertyData
				|>
			|>
		}
	]
];

CreateTripEntityStore::invalid = "Invalid directory ``";
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
	{choices, properties},
	
	choices = importLatestPlotChoiceFile[plotChoicesDirectory];
	
	properties = getPropertyEntryFromPlotChoice /@ Select[choices, StringLength[#Name] > 0&];
	
	(* TODO: Deal with duplicate properties by numbering them? *)
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
		{longerName, propertyCanonicalName, aggregationFunctions},
	    longerName = plotChoice["Name"] <> " time series";
	    propertyCanonicalName = getPropertyCanonicalNameFromPlotChoiceName[longerName];
	    aggregationFunctions = {Min, Mean, Median, StandardDeviation, Max};
	    {
		    Rule[
				propertyCanonicalName,
				<|
					plotChoice,
					"Label" -> getPropertyLabelFromPlotChoiceName[longerName],
					"DefaultFunction" -> Function[entity,
						entity // RightComposition[
							#["RawData"]&,
							GetPlotChoiceTimeSeries[plotChoice, {#}]&,
							
							Function[
								timeSeries,
								(entity[EntityProperty["Trip", propertyCanonicalName, {"AggregationFunction" -> #}]] = #[timeSeries])& /@ aggregationFunctions;
								timeSeries
							]
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
						    {agg = Lookup[qualifiers, "AggregationFunction"]},
					        entity[EntityProperty["Trip", propertyCanonicalName, {"AggregationFunction" -> agg}]] = agg[entity[propertyCanonicalName]]
					    ]
				    ]
			    |>
		    ]& /@ aggregationFunctions
	    }
	]
];

getPropertyCanonicalNameFromPlotChoiceName = RightComposition[
	StringReplace["%" -> "Percent"],
	StringDelete["(" | ")" | "\"" | "?"],
	StringSplit /* Capitalize /* StringJoin
];

(* TODO: Improve the labels to be mostly lowercase to match other properties? *)
getPropertyLabelFromPlotChoiceName = RightComposition[
	StringReplace[lc: LetterCharacter ~~ "?" :> lc <> " (?) "],
	StringReplace[Whitespace -> " "]
];



End[];

EndPackage[];