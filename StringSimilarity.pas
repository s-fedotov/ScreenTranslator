unit StringSimilarity;
//got from
//http://www.delphiarea.com/articles/how-to-match-two-strings-approximately/

interface

function DamerauLevenshteinDistance(const Str1, Str2: string): Integer;
function StringSimilarityRatio(const Str1, Str2: string; IgnoreCase: Boolean): Double;

implementation
uses StrUtils, SysUtils;

function StringSimilarityRatio(const Str1, Str2: string; IgnoreCase: Boolean): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
    begin
      if IgnoreCase then
        Distance := DamerauLevenshteinDistance(LowerCase(Str1), LowerCase(Str2))
      else
        Distance := DamerauLevenshteinDistance(Str1, Str2);
      Result := Result - (Distance / MaxLen);
    end;
end;

function DamerauLevenshteinDistance(const Str1, Str2: string): Integer;
  function Min(const A, B, C: Integer): Integer;
  begin
    Result := A;
    if B < Result then
      Result := B;
    if C < Result then
      Result := C;
  end;

var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // save a bit memory by making the second index points to the shorter string
  if LenStr1 < LenStr2 then
    begin
      T := LenStr1;
      LenStr1 := LenStr2;
      LenStr2 := T;
      pStr1 := PChar(Str2);
      pStr2 := PChar(Str1);
    end
  else
    begin
      pStr1 := PChar(Str1);
      pStr2 := PChar(Str2);
    end;

  // bypass leading identical characters
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
    begin
      Inc(pStr1);
      Inc(pStr2);
      Dec(LenStr1);
      Dec(LenStr2);
    end;

  // bypass trailing identical characters
  while (LenStr2 <> 0) and ((pStr1 + LenStr1 - 1)^ = (pStr2 + LenStr2 - 1)^) do
    begin
      Dec(LenStr1);
      Dec(LenStr2);
    end;

  // is the shorter string empty? so, the edit distance is length of the longer one
  if LenStr2 = 0 then
    begin
      Result := LenStr1;
      Exit;
    end;

  // calculate the edit distance
  GetMem(D, (LenStr2 + 1) * SizeOf(Integer));

  for I := 0 to LenStr2 do
    D[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
    begin
      PrevCost := I - 1;
      Cost := I;
      S2 := pStr2;
      for J := 1 to LenStr2 do
        begin
          if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
            Cost := PrevCost
          else
            Cost := 1 + Min(Cost, PrevCost, D[J]);
          PrevCost := D[J];
          D[J] := Cost;
          Inc(S2);
        end;
      Inc(S1);
    end;
  Result := D[LenStr2];
  FreeMem(D);
end;
end.

