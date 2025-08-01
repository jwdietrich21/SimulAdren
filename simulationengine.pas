unit SimulationEngine;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Simulation Engine }

{ Version 1.1.0 (Desarrollo) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Standard blocks for systems modelling and simulation }

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
  Classes, SysUtils, Math, Forms, Bricks, Lifeblocks, Solver;

const
  GAFactor = 1e-9;
  DAFactor = 1e-12;
  DRFactor = 1e-9;
  eFactor = 1e-15; // fmol/s
  CRHFactor = 1e-15; // fmol/L
  ACTHFactor = 1e-12; // pmol/L
  PRFFactor = 1e-12; // pmol/s
  CortisolFactor = 1e-9; // nmol/L
  yRFactor = 1e-3; // AU

type

  { TSequence }

  TSequence = class
  protected
    function GetSize: integer;
    procedure SetSize(aValue: integer);
  public
    CRH, e, ACTH, PRF, F, v, yR: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer read GetSize write SetSize;
  end;

  TParams = record
    G1, G3, GA, GR, GE, DA, DR: extended;
  end;

  TBlocks = record
    G1, G3, GE: TP;
    MiMeA, MimeR: TMiMe;
    NoCoDI: TNoCoDI;
  end;

  TPrediction = record
    CRH, e, ACTH, PRF, F, v, yR: extended;
  end;

var
  gSequence: TSequence;
  gBlocks: TBlocks;
  gPrediction1, gPrediction2: TPrediction;

procedure RunSimulation(CRH: extended; params: TParams; nmax: integer);
procedure PredictSteadyState(CRH: extended; params: TParams);

implementation

function PituitaryResponse(CRH, yR: extended): extended;
begin
  gBlocks.NoCoDI.input1 := CRH;
  gBlocks.NoCoDI.input2 := yR;
  Result := gBlocks.NoCoDI.simOutput;
end;

function AdrenalResponse(ACTH: extended): extended;
begin
  gBlocks.MiMeA.input := ACTH;
  Result := gBlocks.MiMeA.simOutput;
end;

procedure PredictSteadyState(CRH: extended; params: TParams);
var
  e, ACTH, PRF: extended;
  a, b, c, K1, K2: extended;
  i: integer;
  predictions: TQRoots;
begin
  gPrediction1.CRH := CRH;
  gPrediction2.CRH := CRH;

  { Solving for F: }
  with params do
  begin
    K1 := GR * G3 * GA / (DR + G3 * GA);
    K2 := DR * DA / (DR + G3 * GA);
    a := GE * K1 + 1;
    b := K2 - G1 * gPrediction1.CRH;
    c := -G1 * K2 * gPrediction1.CRH;
    predictions := Solve(a, b, c);
    gPrediction1.ACTH := max(predictions[0], predictions[1]);
    gPrediction1.PRF := GA * gPrediction1.ACTH / (DA + gPrediction1.ACTH);
    gPrediction1.F := G3 * gPrediction1.PRF;
    gPrediction1.v := GR * gPrediction1.F / (DR + gPrediction1.F);
    gPrediction1.yR := GE * gPrediction1.v;
    gPrediction1.e := gPrediction1.CRH / (1 + gPrediction1.yR);

    gPrediction2.ACTH := min(predictions[0], predictions[1]);
    gPrediction2.PRF := GA * gPrediction2.ACTH / (DA + gPrediction2.ACTH);
    gPrediction2.F := G3 * gPrediction2.PRF;
    gPrediction2.v := GR * gPrediction2.F / (DR + gPrediction2.F);
    gPrediction2.yR := GE * gPrediction2.v;
    gPrediction2.e := gPrediction2.CRH / (1 + gPrediction2.yR);
  end;
end;

procedure RunSimulation(CRH: extended; params: TParams; nmax: integer);
var
  e, ACTH, PRF, F, v, yr: extended;
  a, b, c, K1, K2: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
    PredictSteadyState(CRH, params);

    gSequence.size := 0; // delete content
    gSequence.size := nmax;
    gBlocks.G1 := TP.Create;
    gBlocks.G3 := TP.Create;
    gBlocks.GE := TP.Create;
    gBlocks.MiMeA := TMiMe.Create;
    gBlocks.MimeR := TMime.Create;
    gBlocks.NoCoDI := TNoCoDI.Create;
    gBlocks.G1.G := params.G1;
    gBlocks.G3.G := params.G3;
    gBlocks.GE.G := params.GE;
    gBlocks.MiMeA.G := params.GA;
    gBlocks.MiMeA.D := params.DA;
    gBlocks.MimeR.G := params.GR;
    gBlocks.MimeR.D := params.DR;

    yr := 20;
    for i := 0 to nmax - 1 do
    begin
      gBlocks.NoCoDI.input1 := CRH;
      gBlocks.NoCoDI.input2 := yR;
      e := PituitaryResponse(CRH, yR);
      gBlocks.G1.input := e;
      ACTH := gBlocks.G1.simOutput;
      PRF := AdrenalResponse(ACTH);
      F := params.G3 * PRF;
      gBlocks.MimeR.input := F;
      v := gBlocks.MimeR.simOutput;
      gBlocks.GE.input := v;
      yR := gBlocks.GE.simOutput;

      gSequence.CRH[i] := CRH;
      gSequence.e[i] := e;
      gSequence.ACTH[i] := ACTH;
      gSequence.PRF[i] := PRF;
      gSequence.F[i] := F;
      gSequence.v[i] := v;
      gSequence.yr[i] := yr;
      application.ProcessMessages;
    end;
    gBlocks.G1.Destroy;
    gBlocks.G3.Destroy;
    gBlocks.MiMeA.Destroy;
    gBlocks.MimeR.Destroy;
    gBlocks.GE.Destroy;
    gBlocks.NoCoDI.Destroy;
  end;
end;

{ TSequence }

function TSequence.GetSize: integer;
begin
  Result := Length(CRH);
end;

procedure TSequence.SetSize(aValue: integer);
begin
  SetLength(CRH, aValue);
  SetLength(e, aValue);
  SetLength(ACTH, aValue);
  SetLength(PRF, aValue);
  SetLength(F, aValue);
  SetLength(v, aValue);
  SetLength(yr, aValue);
end;

constructor TSequence.Create;
begin
  inherited Create;
end;

destructor TSequence.Destroy;
begin
  inherited Destroy;
end;

end.
