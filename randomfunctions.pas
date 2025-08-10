unit RandomFunctions;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Special random functions for evolutionary algorithms }

{ Version 1.1.0 (Desarrollo) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode ObjFPC}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math;

type
  TIntArray = array of integer;

const
  kError101 = 'Runtime error: Negative parameters';
  kError102 = 'Runtime error: Parameter out of range';
  kError103 = 'Runtime error: min > max';

function Sample(const theArray: TIntArray; const theLength: integer): TIntArray;
function Sample(const theString: string; const theLength: integer): string;
function runif(const min, max: real): real;

implementation

function contains(const theArray: array of integer; theNumber: integer): boolean;
var
  i: integer;
begin
  Result := False;
  i := 0;
  repeat
    if theArray[i] = theNumber then
      Result := True;
    Inc(i)
  until (Result = True) or (i >= length(theArray) - 1);
end;

function Sample(const theArray: TIntArray; const theLength: integer): TIntArray;
var
  i, r: integer;
begin
  assert(theLength <= length(theArray), kError102);
  setLength(Result, theLength);
  for i := 0 to theLength - 1 do
  begin
    repeat
      r := random(length(theArray)) + 1;
    until not contains(Result, r);
    Result[i] := theArray[r - 1];
  end;
end;

function Sample(const theString: string; const theLength: integer): string;
var
  i, origLength: integer;
  indexArray, posArray: array of integer;
begin
  origLength := length(theString);
  setLength(indexArray, origLength);
  Result := '';
  for i := 0 to origLength - 1 do
    indexArray[i] := i + 1;
  posArray := Sample(indexArray, theLength);
  for i := 0 to theLength - 1 do
    Result := Result + theString[posArray[i]];
end;

function runif(const min, max: real): real;
begin
  assert(max > min, kError103);
  result := min + random * (max - min);
end;

end.
