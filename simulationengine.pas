unit SimulationEngine;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Simulation Engine }

{ Version 2.0.0 (Rubycon) }

{ (c) Johannes W. Dietrich, 1994 - 2026 }
{ (c) Nina Siegmar, 2020 - 2026 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2026 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://simuladren.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Bricks, Lifeblocks, Solver, SimuladrenTypes,
  ScenarioHandler;

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

  Delta = 1;

  kCRH = 5e-15;

  VD_ACTH = 2.5;
  kalpha1 = 1 / VD_ACTH;
  kbeta1 = 0.0002;
  VD_F_1 = 20;
  VD_F_1_1 = 21;
  t12_F_1 = 96;
  t12_F_1_1 = 90;
  kalpha3_1 = 1 / VD_F_1;
  kalpha3_1_1 = 1 / VD_F_1_1;
  kbeta3_1 = ln(2) / (t12_F_1 * SecsPerMin);
  kbeta3_1_1 = ln(2) / (t12_F_1_1 * SecsPerMin);
  mPR_F = 5.2e-10;

  kStrucPars_1: TStrucPars =
    ( // default values for model 1
    G1: kalpha1 / kbeta1;
    G3: kalpha3_1 / kbeta3_1;
    GA: 1.2e-9;
    DA: 1e-11;
    GR: 1;
    DR: 2e-7;
    GE: 1;
    alpha1: kalpha1;
    beta1: kbeta1;
    alpha3: kalpha3_1;
    beta3: kbeta3_1;
    );

  kStrucPars_1_1: TStrucPars =
    ( // default values for model 1.1
    G1: kalpha1 / kbeta1;
    G3: kalpha3_1_1 / kbeta3_1_1;
    GA: 2.25 * mPR_F;
    DA: 1e-11;
    GR: 1;
    DR: 2e-7;
    GE: 1;
    alpha1: kalpha1;
    beta1: kbeta1;
    alpha3: kalpha3_1_1;
    beta3: kbeta3_1_1;
    );

  kStrucPars_1_2: TStrucPars =
    ( // default values for model 1.2
    G1: kalpha1 / kbeta1;
    G3: kalpha3_1 / kbeta3_1;
    GA: 1.2e-9;
    DA: 1e-11;
    GR: 1;
    DR: 2e-7;
    GE: 1;
    alpha1: kalpha1;
    beta1: kbeta1;
    alpha3: kalpha3_1;
    beta3: kbeta3_1;
    );

  kStrucPars_1_3: TStrucPars =
    ( // default values for model 1.3
    G1: kalpha1 / kbeta1;
    G3: kalpha3_1_1 / kbeta3_1_1;
    GA: 2.25 * mPR_F;
    DA: 1e-11;
    GR: 1;
    DR: 2e-7;
    GE: 1;
    alpha1: kalpha1;
    beta1: kbeta1;
    alpha3: kalpha3_1_1;
    beta3: kbeta3_1_1;
    );

  kUoMs: array [1..6] of string =
    ( // CRH, e, ACTH, PRF, F, yR
    'fmol/l', 'fmol/s', 'pmol/L', 'pmol/s', 'nmol/L', 'mAU');

  kEvoTargets: TEvoTargets =
    (
    ACTH: 6.81;
    F: 175.88;
    );

type

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

  TBlocks = record
    G1, G3, GE: TP;
    MiMeA, MimeR: TMiMe;
    NoCoDI: TNoCoDI;
    ASIA1, ASIA3: TASIA;
  end;

  TPrediction = record
    CRH, e, ACTH, PRF, F, v, yR: extended;
  end;

  TPredictionArray = array[0..1] of TPrediction;

var
  gSequence: TSequence;
  gBlocks: TBlocks;
  gPrediction: TPredictionArray;

procedure RunSimulation(CRH: extended; model: tActiveModel);
function PredictSteadyState(CRH: extended; model: tActiveModel): TPredictionArray;

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

function PredictSteadyState(CRH: extended; model: tActiveModel): TPredictionArray;
var
  a, b, c, K1, K2: extended;
  predictions: TQRoots;
  params: TStrucPars;
begin
  params := model.StrucPars;
  if (model.Version <> '1') and (model.Version <> '1.1') then
  begin
    { G1 and G3 provide the gain of a P element that is equivalent to the gain
      of the ASIA element in steady state }
    params.G1 := params.alpha1 / params.beta1;
    params.G3 := params.alpha3 / params.beta3;
  end;
  Result[0].CRH := CRH;
  Result[1].CRH := CRH;

  { Solving for F (Cortisol): }
  with params do
  begin
    K1 := GR * G3 * GA / (DR + G3 * GA);
    K2 := DR * DA / (DR + G3 * GA);
    a := GE * K1 + 1;
    b := K2 - G1 * Result[0].CRH;
    c := -G1 * K2 * Result[0].CRH;
    predictions := Solve(a, b, c);
    Result[0].ACTH := max(predictions[0], predictions[1]);
    Result[0].PRF := GA * Result[0].ACTH / (DA + Result[0].ACTH);
    Result[0].F := G3 * Result[0].PRF;
    Result[0].v := GR * Result[0].F / (DR + Result[0].F);
    Result[0].yR := GE * Result[0].v;
    Result[0].e := Result[0].CRH / (1 + Result[0].yR);

    Result[1].ACTH := min(predictions[0], predictions[1]);
    Result[1].PRF := GA * Result[1].ACTH / (DA + Result[1].ACTH);
    Result[1].F := G3 * Result[1].PRF;
    Result[1].v := GR * Result[1].F / (DR + Result[1].F);
    Result[1].yR := GE * Result[1].v;
    Result[1].e := Result[1].CRH / (1 + Result[1].yR);
  end;
end;

procedure RunSimulation(CRH: extended; model: tActiveModel);
var
  e, ACTH, PRF, F, v, yr: extended;
  i: integer;
  params: TStrucPars;
begin
  params := model.StrucPars;
  if model.Iterations > 0 then
  begin
    gPrediction := PredictSteadyState(CRH, model);

    gSequence.size := 0; // delete content
    gSequence.size := model.Iterations;;
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
    if (model.Version <> '1') and (model.Version <> '1.1') then
    begin
      gBlocks.ASIA1 := TASIA.Create;
      gBlocks.ASIA3 := TASIA.Create;
      gBlocks.ASIA1.alpha := params.alpha1;
      gBlocks.ASIA1.beta := params.beta1;
      gBlocks.ASIA1.delta := Delta;
      gBlocks.ASIA3.alpha := params.alpha3;
      gBlocks.ASIA3.beta := params.beta3;
      gBlocks.ASIA3.delta := Delta;
    end;

    yr := 20;
    for i := 0 to model.Iterations - 1 do
    begin
      gBlocks.NoCoDI.input1 := CRH;
      gBlocks.NoCoDI.input2 := yR;
      e := PituitaryResponse(CRH, yR);
      if (model.Version = '1') or (model.Version = '1.1') then
      begin
        gBlocks.G1.input := e;
        ACTH := gBlocks.G1.simOutput;
      end
      else
      begin
        gBlocks.ASIA1.input := e;
        ACTH := gBlocks.ASIA1.simOutput;
      end;
      PRF := AdrenalResponse(ACTH);
      if (model.Version = '1') or (model.Version = '1.1') then
      begin
        gBlocks.G3.input := PRF;
        F := gBlocks.G3.simOutput;
      end
      else
      begin
        gBlocks.ASIA3.input := PRF;
        F := gBlocks.ASIA3.simOutput;
      end;
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
    if assigned(gBlocks.ASIA1) then
      FreeAndNil(gBlocks.ASIA1);
    if assigned(gBlocks.ASIA3) then
      FreeAndNil(gBlocks.ASIA3);
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

initialization

  gActiveModel := NewScenario;

end.
