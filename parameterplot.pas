unit ParameterPlot;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Plot for evoluation of parameters over generations }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  TADrawUtils, TADrawerSVG, TADrawerCanvas, SimulAdrenTypes, EvoEngine;

type

  { TParameterForm }

  TParameterForm = class(TForm)
    ParameterChart: TChart;
    GELineSeries: TLineSeries;
    GRLineSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure DrawParameters(theFittest: TFittest);
    procedure SaveChart(fileName: string; imageType: TImageType);
  end;

var
  ParameterForm: TParameterForm;

implementation

{$R *.lfm}

{ TParameterForm }

procedure TParameterForm.FormCreate(Sender: TObject);
begin
  Scaled := true;
  left := 26;
  top := screen.Height - height - trunc(39 * gScalingFactor);
end;

procedure TParameterForm.DrawParameters(theFittest: TFittest);
var
  i: integer;
begin
  GELineSeries.Clear;
  GRLineSeries.Clear;
  for i := 0 to length(theFittest) - 1 do
  begin
    GELineSeries.AddXY(i, theFittest[i].GE);
    GRLineSeries.AddXY(i, theFittest[i].GR);
  end;
end;

procedure TParameterForm.SaveChart(fileName: string; imageType: TImageType);
var
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  theStream := nil;
  try
    case imageType of
      BMP: ParameterChart.SaveToBitmapFile(fileName);
      XPM: ParameterChart.SaveToFile(TPixmap, fileName);
      PNG: ParameterChart.SaveToFile(TPortableNetworkGraphic, fileName);
      PBM: ParameterChart.SaveToFile(TPortableAnyMapGraphic, fileName);
      JPG: ParameterChart.SaveToFile(TJPEGImage, fileName);
      TIFF: ParameterChart.SaveToFile(TTIFFImage, fileName);
      SVG: begin
        theStream := TFileStream.Create(fileName, fmCreate);
        theDrawer := TSVGDrawer.Create(theStream, True);
        theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
        with ParameterChart do
          Draw(theDrawer, Rect(0, 0, Width, Height));
      end;
    end;
  finally
    if assigned(theStream) then
      theStream.Free;
  end;
end;

end.

