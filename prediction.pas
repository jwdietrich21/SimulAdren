unit Prediction;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Predictor }

{ Version 1.1.0 (Desarrollo) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, SimuladrenTypes, SimulationEngine;

type

  { TPredictionForm }

  TPredictionForm = class(TForm)
    PredictionList: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DisplayPrediction(Parameters1, Parameters2: TPrediction);
  end;

var
  PredictionForm: TPredictionForm;

implementation

{$R *.lfm}

procedure TPredictionForm.FormPaint(Sender: TObject);
begin
  PredictionList.Cells[0, 1] := 'CRH';
  PredictionList.Cells[0, 2] := 'e';
  PredictionList.Cells[0, 3] := 'ACTH';
  PredictionList.Cells[0, 4] := 'PRF';
  PredictionList.Cells[0, 5] := 'F';
  PredictionList.Cells[0, 6] := 'yr';
end;

procedure TPredictionForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  left := screen.Width - Width - 26;
  for i := 1 to PredictionList.RowCount - 1 do
    PredictionList.Cells[3, i] := kUoMs[i];
end;

procedure TPredictionForm.DisplayPrediction(Parameters1, Parameters2: TPrediction);
begin
  PredictionList.Cells[1, 1] := FloatToStrF(Parameters1.CRH / CRHFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 2] := FloatToStrF(Parameters1.e / eFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 3] := FloatToStrF(Parameters1.ACTH / ACTHFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 4] := FloatToStrF(Parameters1.PRF / PRFFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 5] := FloatToStrF(Parameters1.F / CortisolFactor, ffFixed, 0, 4);
  PredictionList.Cells[1, 6] := FloatToStrF(Parameters1.yr / yRFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 1] := FloatToStrF(Parameters2.CRH / CRHFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 2] := FloatToStrF(Parameters2.e / eFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 3] := FloatToStrF(Parameters2.ACTH / ACTHFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 4] := FloatToStrF(Parameters2.PRF / PRFFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 5] := FloatToStrF(Parameters2.F / CortisolFactor, ffFixed, 0, 4);
  PredictionList.Cells[2, 6] := FloatToStrF(Parameters2.yr / yRFactor, ffFixed, 0, 4);
end;

end.

