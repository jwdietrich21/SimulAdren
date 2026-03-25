unit FitnessPlot;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Plot for fitness over generations }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  TADrawUtils, TADrawerSVG, TADrawerCanvas, SimulAdrenTypes, EvoEngine;

type

  { TFitnessPlotForm }

  TFitnessPlotForm = class(TForm)
    FitnessChart: TChart;
    FitnessChartLineSeries1: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure DrawFitness(theFittest: TFittest);
    procedure SaveChart(fileName: string; imageType: TImageType);
  end;

var
  FitnessPlotForm: TFitnessPlotForm;

implementation

{$R *.lfm}

{ TFitnessPlotForm }

procedure TFitnessPlotForm.FormCreate(Sender: TObject);
begin
  Scaled := true;
  left := screen.Width - width - trunc(39 * gScalingFactor);
end;

procedure TFitnessPlotForm.DrawFitness(theFittest: TFittest);
var
  i: integer;
begin
  FitnessChartLineSeries1.Clear;
  for i := 0 to length(theFittest) - 1 do
    FitnessChartLineSeries1.AddXY(i, theFittest[i].fitness);
end;

procedure TFitnessPlotForm.SaveChart(fileName: string; imageType: TImageType);
var
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  theStream := nil;
  try
    case imageType of
      BMP: FitnessChart.SaveToBitmapFile(fileName);
      XPM: FitnessChart.SaveToFile(TPixmap, fileName);
      PNG: FitnessChart.SaveToFile(TPortableNetworkGraphic, fileName);
      PBM: FitnessChart.SaveToFile(TPortableAnyMapGraphic, fileName);
      JPG: FitnessChart.SaveToFile(TJPEGImage, fileName);
      TIFF: FitnessChart.SaveToFile(TTIFFImage, fileName);
      SVG: begin
        theStream := TFileStream.Create(fileName, fmCreate);
        theDrawer := TSVGDrawer.Create(theStream, True);
        theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
        with FitnessChart do
          Draw(theDrawer, Rect(0, 0, Width, Height));
      end;
    end;
  finally
    if assigned(theStream) then
      theStream.Free;
  end;
end;

end.

