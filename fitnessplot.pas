unit FitnessPlot;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Plot for fitness over generations }

{ Version 1.2.0 (Emerald) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Nina Siegmar, 2020 - 2025 }
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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  EvoEngine;

type

  { TFitnessPlotForm }

  TFitnessPlotForm = class(TForm)
    FitnessChart: TChart;
    FitnessChartLineSeries1: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure DrawFitness(theFittest: TFittest);
  end;

var
  FitnessPlotForm: TFitnessPlotForm;

implementation

{$R *.lfm}

{ TFitnessPlotForm }

procedure TFitnessPlotForm.FormCreate(Sender: TObject);
begin
  left := screen.Width - width - 39;
end;

procedure TFitnessPlotForm.DrawFitness(theFittest: TFittest);
var
  i: integer;
begin
  FitnessChartLineSeries1.Clear;
  for i := 0 to length(theFittest) - 1 do
    FitnessChartLineSeries1.AddXY(i, theFittest[i].fitness);
end;

end.

