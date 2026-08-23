unit Plot;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Plot unit }

{ Version 2.1.0 (Chronos) }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel, TADrawUtils,
  TADrawerSVG, TADrawerCanvas, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ColorBox, SimuladrenTypes, SimulationEngine, GUIServices;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    ChartLegendPanel1: TChartLegendPanel;
    TimeSeriesChart: TChart;
    PRFSeries: TLineSeries;
    FSeries: TLineSeries;
    eSeries: TLineSeries;
    ACTHSeries: TLineSeries;
    VariablesCheckGroup: TCheckGroup;
    yrSeries: TLineSeries;
    CRHSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure VariablesCheckGroupClick(Sender: TObject);
    procedure VariablesCheckGroupItemClick(Sender: TObject; Index: integer);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowPlot;
    procedure SaveChart(fileName: string; imageType: TImageType);
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Scaled := True;
  top := screen.Height - Height - trunc(39 * gScalingFactor);
  left := screen.Width - Width - trunc(52 * gScalingFactor);
  CRHSeries.SeriesColor := clDarkOrange;
  eSeries.SeriesColor := clDarkOrange;
  ACTHSeries.SeriesColor := clGoldenRod;
  for i := 0 to VariablesCheckGroup.Items.Count - 2 do
    VariablesCheckGroup.Checked[i] := True;
end;

procedure TPlotForm.VariablesCheckGroupClick(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.VariablesCheckGroupItemClick(Sender: TObject; Index: integer);
begin
  ShowPlot;
end;

procedure TPlotForm.ShowPlot;
var
  i: integer;
begin
  TimeSeriesChart.AxisList.Axes[1].Range.Max := gSequence.size - 1;
  if DarkTheme then
    yrSeries.SeriesColor := clWhite;
  if VariablesCheckGroup.Checked[0] then
    CRHSeries.Active := True
  else
    CRHSeries.Active := False;
  if VariablesCheckGroup.Checked[1] then
    eSeries.Active := True
  else
    eSeries.Active := False;
  if VariablesCheckGroup.Checked[2] then
    ACTHSeries.Active := True
  else
    ACTHSeries.Active := False;
  if VariablesCheckGroup.Checked[3] then
    PRFSeries.Active := True
  else
    PRFSeries.Active := False;
  if VariablesCheckGroup.Checked[4] then
    FSeries.Active := True
  else
    FSeries.Active := False;
  if VariablesCheckGroup.Checked[5] then
    yrSeries.Active := True
  else
    yrSeries.Active := False;
  CRHSeries.Clear;
  PRFSeries.Clear;
  FSeries.Clear;
  eSeries.Clear;
  ACTHSeries.Clear;
  yrSeries.Clear;
  CRHSeries.BeginUpdate;
  PRFSeries.BeginUpdate;
  FSeries.BeginUpdate;
  eSeries.BeginUpdate;
  ACTHSeries.BeginUpdate;
  yrSeries.BeginUpdate;
  for i := 0 to gSequence.size - 1 do
  begin
    CRHSeries.AddXY(i, gSequence.CRH[i] / CRHFactor);
    PRFSeries.AddXY(i, gSequence.PRF[i] / PRFFactor);
    FSeries.AddXY(i, gSequence.F[i] / CortisolFactor);
    eSeries.AddXY(i, gSequence.e[i] / eFactor);
    ACTHSeries.AddXY(i, gSequence.ACTH[i] / ACTHFactor);
    yrSeries.AddXY(i, gSequence.yr[i] / yRFactor);
  end;
  CRHSeries.EndUpdate;
  PRFSeries.EndUpdate;
  FSeries.EndUpdate;
  eSeries.EndUpdate;
  ACTHSeries.EndUpdate;
  yrSeries.EndUpdate;
end;

procedure TPlotForm.SaveChart(fileName: string; imageType: TImageType);
var
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  theStream := nil;
  try
    case imageType of
      BMP: TimeSeriesChart.SaveToBitmapFile(fileName);
      XPM: TimeSeriesChart.SaveToFile(TPixmap, fileName);
      PNG: TimeSeriesChart.SaveToFile(TPortableNetworkGraphic, fileName);
      PBM: TimeSeriesChart.SaveToFile(TPortableAnyMapGraphic, fileName);
      JPG: TimeSeriesChart.SaveToFile(TJPEGImage, fileName);
      TIFF: TimeSeriesChart.SaveToFile(TTIFFImage, fileName);
      SVG: begin
        theStream := TFileStream.Create(fileName, fmCreate);
        theDrawer := TSVGDrawer.Create(theStream, True);
        theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
        with TimeSeriesChart do
          Draw(theDrawer, Rect(0, 0, Width, Height));
      end;
    end;
  finally
    if assigned(theStream) then
      theStream.Free;
  end;
end;

end.
