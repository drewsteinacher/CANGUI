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

CreateTripEntityStore[directory_String] /; DirectoryQ[directory] := With[
	{
		tripEntityData = getTripData[directory]
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
							RightComposition[
								EntityProperty["Trip", "File"],
								importDataFiles,
								Values
							]
						]
					|>
				|>
			|>
		}
	]
];

CreateTripEntityStore::invalid = "Invalid directory ``";
CreateTripEntityStore[directory_, ___] := (Message[CreateTripEntityStore::invalid, directory]; $Failed);

getTripData[directory_String] := Module[
	{fileToMetadata, entityData},

	fileToMetadata = populateCANFileMetadata[directory];

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

End[];

EndPackage[];