unit Plot;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Plot unit }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel, Forms,
  Controls, Graphics, Dialogs, SimuladrenTypes, SimulationEngine;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    PRFSeries: TLineSeries;
    FSeries: TLineSeries;
    ChartLegendPanel1: TChartLegendPanel;
    eSeries: TLineSeries;
    ACTHSeries: TLineSeries;
    yrSeries: TLineSeries;
    CRHSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowPlot;
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.FormCreate(Sender: TObject);
begin
  top := screen.Height - Height - 39;
  CRHSeries.SeriesColor := clDarkOrange;
  eSeries.SeriesColor := clDarkOrange;
  ACTHSeries.SeriesColor := clGoldenRod;
end;

procedure TPlotForm.ShowPlot;
var
  i: integer;
begin
  Chart1.AxisList.Axes[1].Range.Max := gValues.size - 1;
  for i := 0 to gValues.size - 1 do
  begin
    CRHSeries.AddXY(i, gValues.CRH[i] / CRHFactor);
    PRFSeries.AddXY(i, gValues.PRF[i] / PRFFactor);
    FSeries.AddXY(i, gValues.F[i] / CortisolFactor);
    eSeries.AddXY(i, gValues.e[i] / eFactor);
    ACTHSeries.AddXY(i, gValues.ACTH[i] / ACTHFactor);
    yrSeries.AddXY(i, gValues.yr[i] / yRFactor);
  end;
end;

end.

