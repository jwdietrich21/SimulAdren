unit HandleInitialConditions;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Form for setting initial conditions }

{ Version 2.0.1 (Rubycon) }

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


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  SimulAdrenTypes, SimulationEngine;

type

  { TInitialConditionsForm }

  TInitialConditionsForm = class(TForm)
    CurStateButton: TButton;
    CancelButton: TButton;
    PredictionButton: TButton;
    OKButton: TButton;
    ICList: TStringGrid;
    procedure CancelButtonClick(Sender: TObject);
    procedure CurStateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PredictionButtonClick(Sender: TObject);
  private

  public
    response: TModalResult;
  end;

var
  InitialConditionsForm: TInitialConditionsForm;

implementation

{$R *.lfm}

{ TInitialConditionsForm }

procedure TInitialConditionsForm.CancelButtonClick(Sender: TObject);
begin
  response := mrCancel;
  Close;
end;

procedure TInitialConditionsForm.CurStateButtonClick(Sender: TObject);
var
  i: integer;
begin
  if assigned(gSequence) then
  begin
    i := gSequence.size - 1;
    ICList.Cells[1, 1] := FloatToStrF(gSequence.CRH[i] / CRHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 2] := FloatToStrF(gSequence.e[i] / eFactor, ffFixed, 0, 4);
    ICList.Cells[1, 3] := FloatToStrF(gSequence.ACTH[i] / ACTHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 4] := FloatToStrF(gSequence.PRF[i] / PRFFactor, ffFixed, 0, 4);
    ICList.Cells[1, 5] := FloatToStrF(gSequence.F[i] / CortisolFactor, ffFixed, 0, 4);
    ICList.Cells[1, 6] := FloatToStrF(gSequence.yR[i] / yRFactor, ffFixed, 0, 4);
  end;
end;

procedure TInitialConditionsForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ICList.Cells[0, 1] := 'CRH';
  ICList.Cells[0, 2] := 'e';
  ICList.Cells[0, 3] := 'ACTH';
  ICList.Cells[0, 4] := 'PRF';
  ICList.Cells[0, 5] := 'F';
  ICList.Cells[0, 6] := 'yr';
  ICList.Cells[1, 1] := FloatToStrF(gInitialconditions.CRH / CRHFactor, ffFixed, 0, 4);
  for i := 2 to ICList.RowCount - 1 do
  begin
    ICList.Cells[1, i] := '0';
  end;
  for i := 1 to ICList.RowCount - 1 do
  begin
    ICList.Cells[2, i] := kUoMs[i];
  end;
end;

procedure TInitialConditionsForm.OKButtonClick(Sender: TObject);
begin
  gInitialConditions.CRH := StrToFloatDef(ICList.Cells[1, 1], 0) * CRHFactor;
  gInitialConditions.e := StrToFloatDef(ICList.Cells[1, 2], 0) * eFactor;
  gInitialConditions.ACTH := StrToFloatDef(ICList.Cells[1, 3], 0) * ACTHFactor;
  gInitialConditions.PRF := StrToFloatDef(ICList.Cells[1, 4], 0) * PRFFactor;
  gInitialConditions.F := StrToFloatDef(ICList.Cells[1, 5], 0) * CortisolFactor;
  gInitialConditions.yR := StrToFloatDef(ICList.Cells[1, 6], 0) * yRFactor;
  response := mrOk;
  Close;
end;

procedure TInitialConditionsForm.PredictionButtonClick(Sender: TObject);
begin
  if gPrediction[0].F > 0 then
  begin
    ICList.Cells[1, 1] := FloatToStrF(gPrediction[0].CRH / CRHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 2] := FloatToStrF(gPrediction[0].e / eFactor, ffFixed, 0, 4);
    ICList.Cells[1, 3] := FloatToStrF(gPrediction[0].ACTH / ACTHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 4] := FloatToStrF(gPrediction[0].PRF / PRFFactor, ffFixed, 0, 4);
    ICList.Cells[1, 5] := FloatToStrF(gPrediction[0].F / CortisolFactor, ffFixed, 0, 4);
    ICList.Cells[1, 6] := FloatToStrF(gPrediction[0].yR / yRFactor, ffFixed, 0, 4);
  end
  else
  begin
    ICList.Cells[1, 1] := FloatToStrF(gPrediction[1].CRH / CRHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 2] := FloatToStrF(gPrediction[1].e / eFactor, ffFixed, 0, 4);
    ICList.Cells[1, 3] := FloatToStrF(gPrediction[1].ACTH / ACTHFactor, ffFixed, 0, 4);
    ICList.Cells[1, 4] := FloatToStrF(gPrediction[1].PRF / PRFFactor, ffFixed, 0, 4);
    ICList.Cells[1, 5] := FloatToStrF(gPrediction[1].F / CortisolFactor, ffFixed, 0, 4);
    ICList.Cells[1, 6] := FloatToStrF(gPrediction[1].yR / yRFactor, ffFixed, 0, 4);
  end;
end;

initialization

  gInitialconditions.CRH := kCRH;

end.
