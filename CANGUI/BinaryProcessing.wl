ClearAll["CANGUI`BinaryProcessing`*"];
ClearAll["CANGUI`BinaryProcessing`*`*"];

BeginPackage["CANGUI`BinaryProcessing`"];

signedInt::usage = "signedInt[bytes: {__Integer} | {__TemporalData}] converts bytes into a signed integer";
unsignedInt::usage = "unsignedInt[bytes: {__Integer} | {__TemporalData}] converts bytes into an unsigned integer";
bitGet::usage = "bitGet[td_TemporalData, k_Integer] gets the kth bit from the given TemporalData object";
nibbleGet::usage = "nibbleGet[byte_, n_Integer, offset_Integer:0] gets the nth nibble from the right in byte (with an optional offset)";
bitPlot::usage = "bitPlot[byte] plots the bits in the given byte in one plot";

Begin["`Private`"];

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


End[];

EndPackage[];