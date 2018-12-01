ClearAll["CANGUI`TripEntityStore`*"];
ClearAll["CANGUI`TripEntityStore`*`*"];

BeginPackage["CANGUI`TripEntityStore`",
	{
		"CANGUI`",
		"CANGUI`Utilities`BinaryProcessing`",
		"CANGUI`Utilities`"
	}
];

CreateTripEntityStore;

Begin["`Private`"];

CreateTripEntityStore::usage = "CreateTripEntityStore[directory] creates an EntityStore from the trip files in the given directory";

CreateTripEntityStore[directory_String] /; DirectoryQ[directory] := Module[
	{},
	populateCANFileMetadata[directory]
];

CreateTripEntityStore::invalid = "Invalid directory ``";
CreateTripEntityStore[directory_, ___] := (Message[CreateTripEntityStore::invalid, directory]; $Failed);

End[];

EndPackage[];