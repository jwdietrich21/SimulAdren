program SimulAdren;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Main project file }

{ Version 1.1.0 (Desarrollo) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, GUI, SimulationEngine, IPS, bricks, lifeblocks,
  Plot, prediction, SimuladrenTypes, Solver, AboutBox, EnvironmentInfo,
  RandomFunctions, evoEngine, SetTargets
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TValuesForm, ValuesForm);
  Application.CreateForm(TIPSForm, IPSForm);
  Application.CreateForm(TPlotForm, PlotForm);
  Application.CreateForm(TPredictionForm, PredictionForm);
  Application.CreateForm(TAboutWindow, AboutWindow);
  Application.CreateForm(TTargetForm, TargetForm);
  Application.Run;
end.

