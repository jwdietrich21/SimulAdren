unit SignalAnalysis;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Standard signal processing tools }

{ FFT functions }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math, ucomplex;

const

  kError300 = 'Runtime error: Vector length not a power of two.';

type

  table = array of complex;

function IsPowerOfTwo(num: integer): boolean;
function NextPowerOfTwo(num: integer): integer;
function fft(DataVector: table): table;

implementation

var
  MaxPower2: integer;

procedure split(t: table; evens: table; odds: table);
var
  k: integer;
begin
  for k := 0 to length(t) - 1 do
    if odd(k) then
      odds[k div 2] := t[k]
    else
      evens[k div 2] := t[k];
end;

function IsPowerOfTwo(num: integer): boolean;
var
  y: integer;
begin
  Result := False;
  y := 2;
  repeat
    y := y * 2;
    if num = y then Result := True;
  until (num = y) or (y >= MaxPower2);
end;

function NextPowerOfTwo(num: integer): integer;
var
  y: integer;
begin
  y := 2;
  if num > 2 then
    repeat
      if num <> y then y := y * 2;
    until (y >= num) or (y >= MaxPower2);
  Result := y;
end;

function fft(DataVector: table): table;
{ Modified from implementations published by "JDP" at Rosetta Code
  (https://rosettacode.org/wiki/Fast_Fourier_transform),
  by Bob Schor, Don Cross and Jean Debord }
var
  k: integer;
  n: integer;
  halfN: integer;
  EvenTable: table;
  Even: table;
  OddTable: table;
  Odds: table;
  T: complex;
begin
  n := length(DataVector);

  if n >= 2 then
  begin
    halfN := (n div 2);
    SetLength(EvenTable, halfN);
    SetLength(OddTable, halfN);
    Split(DataVector, EvenTable, OddTable);
    SetLength(DataVector, 0);
    SetLength(Even, halfN);
    Even := FFT(EvenTable);
    SetLength(EvenTable, 0);
    SetLength(Odds, halfN);
    Odds := FFT(OddTable);
    SetLength(OddTable, 0);
    SetLength(DataVector, n);
    for k := 0 to halfN - 1 do
    begin
      T := Cexp(-2 * i * pi * k / n) * Odds[k];
      DataVector[k] := Even[k] + T;
      DataVector[k + halfN] := Even[k] - T;
    end;
    SetLength(Even, 0);
    SetLength(Odds, 0);
  end;
  Result := DataVector;
end;

initialization

  MaxPower2 := Trunc(Log2(MaxFloat));

end.
