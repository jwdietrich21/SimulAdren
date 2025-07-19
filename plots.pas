unit plots;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.0.0 (Escorpión) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Unit for plotting }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TAGraph, TASeries, Math, Bricks;

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended; resolution: integer;
  var omega, M, phi: TVector;
  var inputSignal, outputSignal, time: TMatrix);

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; resolution: integer; var omega, M, phi: TVector);

implementation

function FirstMinimum(timeSeries: tVector): Integer;
var
  t: Integer;
  tMin, lasty: extended;
begin
  t := 0;
  tMin := 0;
  repeat
  { search for sinking values first }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] < lasty) or (t = length(timeSeries));
  if t < length(timeSeries) then
  repeat
   { and then for rising values }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] > lasty) or (t = length(timeSeries));
  Result := t - 1;
end;

function FirstMaximum(timeSeries: tVector): Integer;
var
  t: Integer;
  tMax, lasty: extended;
begin
  t := 0;
  tMax := 0;
  repeat
  { search for rising values first }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] > lasty) or (t = length(timeSeries));
  if t < length(timeSeries) then
  repeat
   { and then for sinking values }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] < lasty) or (t = length(timeSeries));
  Result := t - 1;
end;

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended; resolution: integer;
  var omega, M, phi: TVector; var inputSignal, outputSignal, time: TMatrix);
{ Draws extimated bode plot via simulation }
var
  diff, startTime: extended;
  x, y, t, x2, y2: TVector;
  i, j, tpeak, tmin, tmax: Integer;
  minI, maxI, testLength, initLength: integer;
  model: TModel;
  testSignal: TTHarmonic;
begin
  model := TModel.Create;
  testSignal := TTHarmonic.Create;
  model.firstBlock := testSignal;
  testSignal.model := model;
  testSignal.delta := 1 / (maxFreq * 2); // Nyquist frequency
  testSignal.phi := pi / 2; // begin with maximum as in cosine function
  testSignal.G := aBrick.amplitude;
  testSignal.updateTime := true;
  if aBrick.ClassType = TPT0 then
  begin
    TPT0(aBrick).delta := testSignal.delta;
  end
  else if aBrick.ClassType = TPT1 then
  begin
    TPT1(aBrick).delta := testSignal.delta;
  end
  else if aBrick.ClassType = TPT2 then
  begin
    TPT2(aBrick).delta := testSignal.delta;
  end;
  testLength := 1 + trunc(2 * pi / (minFreq * testSignal.delta));
  initLength := testLength * 10;
  SetLength(omega, resolution + 1);
  SetLength(M, resolution + 1);
  SetLength(phi, resolution + 1);
  SetLength(y, testLength + 1);
  SetLength(t, testLength + 1);
  diff := maxFreq - minFreq;
  minI := 1;
  maxI := resolution;
  SetLength(inputSignal, maxI - minI + 2, testLength + 1);
  SetLength(outputSignal, maxI - minI + 2, testLength + 1);
  SetLength(time, maxI - minI + 2, testLength + 1);
  for i := 0 to maxI do
  begin
    SetLength(x, initLength + 1);
    model.Reset;
    omega[i] := minFreq + (i) * diff / resolution;
    testSignal.omega := omega[i];
    for j := 0 to initLength do
    { initial runs for allowing the system to settle to a new equilibrium }
    begin
      x[j] := testSignal.simOutput;
      if aBrick.ClassType = TPT0 then
      begin
        TPT0(aBrick).input := x[j];
        TPT0(aBrick).simulate;
      end
      else if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[j];
        TPT1(aBrick).simulate;
      end
      else if aBrick.ClassType = TPT2 then
      begin
        TPT2(aBrick).input := x[j];
        TPT2(aBrick).simulate;
      end;
    end;
    SetLength(x, testLength + 1);
    startTime := model.time;
    for j := 0 to testLength do
    { simulation to deliver time series }
    begin
      x[j] := testSignal.simOutput;
      if aBrick.ClassType = TPT0 then
      begin
        TPT0(aBrick).input := x[j];
        y[j] := TPT0(aBrick).simOutput;
        t[j] := model.time - startTime;
      end
      else if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[j];
        y[j] := TPT1(aBrick).simOutput;
        t[j] := model.time - startTime;
      end
      else if aBrick.ClassType = TPT2 then
      begin
        TPT2(aBrick).input := x[j];
        y[j] := TPT2(aBrick).simOutput;
        t[j] := model.time - startTime;
      end;
    end;
    tpeak := FirstMaximum(x);
    if tPeak >= j - 10 then
      tPeak := 0;
    tmin := FirstMinimum(y);
    tmax := FirstMaximum(y);
    M[i] := y[tmax] - y[tmin];
    if tmax < tpeak then  // compensate for phase cut situation
    begin
      SetLength(x2, length(x) - tmax);
      SetLength(y2, length(y) - tmax);
      x2 := copy(x, tmax, length(x) - tmax);
      y2 := copy(y, tmax, length(y) - tmax);
      tPeak := FirstMinimum(x2);
      tmin := FirstMinimum(y2);
      tmax := FirstMaximum(y2);
    end;
    phi[i] := tmax - tpeak;
    AmpSeries.AddXY(omega[i], M[i]);
    PhaseSeries.AddXY(omega[i], phi[i]);
    inputSignal[i] := copy(x, 0, length(x));  // simple assignements of open
    outputSignal[i] := copy(y, 0, length(y)); // arrays wouldn't copy
    time[i] := copy(t, 0, length(t));
  end;
  testSignal.Destroy;
  model.Destroy;
end;

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; resolution: integer; var omega, M, phi: TVector);
{ Draws exact bode plot from calculated frequency response }
var
  diff: extended;
  i: Integer;
  minI, maxI: integer;
begin
  SetLength(omega, resolution + 1);
  SetLength(M, resolution + 1);
  SetLength(phi, resolution + 1);
  diff := maxFreq - minFreq;
  minI := 1;
  maxI := resolution;
  for i := 0 to maxI do
  begin
    omega[i] := MINFREQ + (i) * diff / resolution;
    if aBrick.ClassType = TPT0 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT0(aBrick).fr.M;
      phi[i] := TPT0(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TPT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT1(aBrick).fr.M;
      phi[i] := TPT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TPT2 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT2(aBrick).fr.M;
      phi[i] := TPT2(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TDT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TDT1(aBrick).fr.M;
      phi[i] := TDT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TInt then
    begin
      aBrick.omega := omega[i];
      M[i] := TInt(aBrick).fr.M;
      phi[i] := TInt(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TIT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TIT1(aBrick).fr.M;
      phi[i] := TIT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TIT2 then
    begin
      aBrick.omega := omega[i];
      M[i] := TIT2(aBrick).fr.M;
      phi[i] := TIT2(aBrick).fr.phi;
    end;
   AmpSeries.AddXY(omega[i], m[i]);
   PhaseSeries.AddXY(omega[i], phi[i]);
  end;
end;

end.

