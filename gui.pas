unit GUI;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ GUI }

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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus, Math,
  SimuladrenTypes, SimulationEngine, Prediction, Plot, GUIServices, AboutBox,
  SetTargets, evoEngine;

type

  { TValuesForm }

  TValuesForm = class(TForm)
    AppleMenu: TMenuItem;
    EstimateGECheckbox: TCheckBox;
    EstimateGRCheckBox: TCheckBox;
    SteadyStateButton: TButton;
    EvolveButton: TButton;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DAUnitLabel: TLabel;
    DREdit: TFloatSpinEdit;
    DRLabel: TLabel;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    DRUnitLabel: TLabel;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    DAEdit: TFloatSpinEdit;
    DALabel: TLabel;
    GAUnitLabel: TLabel;
    GEEdit: TFloatSpinEdit;
    G3Edit: TFloatSpinEdit;
    G3Label: TLabel;
    GREdit: TFloatSpinEdit;
    GRLabel: TLabel;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    IterationsSpinEdit: TSpinEdit;
    G1Label: TLabel;
    G1Edit: TFloatSpinEdit;
    G1UnitLabel: TLabel;
    G3Unitlabel: TLabel;
    GAFactorLabel: TLabel;
    DAFactorLabel: TLabel;
    GRUnitLabel: TLabel;
    DRFactorLabel: TLabel;
    GELabel: TLabel;
    CRHUnitLabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    CRHLabel: TLabel;
    CRHSpinEdit: TFloatSpinEdit;
    StartButton: TButton;
    ValuesGrid: TStringGrid;
    ToolBar1: TToolBar;
    IterationsLabel: TLabel;
    GALabel: TLabel;
    GAEdit: TFloatSpinEdit;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure EstimateGECheckboxChange(Sender: TObject);
    procedure EstimateGRCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure EvolveButtonClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure SteadyStateButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    AllPopulations: TAllPopulations;
    FittestIndividuals: TFittest;
    procedure ShowAboutWindow(Sender: TObject);
    procedure CopyCells(Sender: TObject);
    procedure ReadParams(Sender: TObject; var params: TParams);
    procedure SetParams(Sender: TObject; const params: TParams);
  end;

var
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

{ TValuesForm }

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  i, j, iterations: integer;
  params: TParams;
begin
  ReadParams(Sender, params);
  iterations := IterationsSpinEdit.Value;
  gSequence := TSequence.Create;
  ValuesGrid.RowCount := iterations + 2;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 2 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
  PlotForm.CRHSeries.Clear;
  PlotForm.PRFSeries.Clear;
  PlotForm.FSeries.Clear;
  PlotForm.eSeries.Clear;
  PlotForm.ACTHSeries.Clear;
  PlotForm.yrSeries.Clear;
  RunSimulation(CRHSpinEdit.Value * CRHFactor, params, iterations);
  PredictionForm.DisplayPrediction(gPrediction[0], gPrediction[1]);
  if iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := iterations + 1;
  for i := 0 to iterations - 1 do
  begin
    ValuesGrid.Cells[0, i + 2] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 2] :=
      FloatToStrF(gSequence.CRH[i] / CRHFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 2] := FloatToStrF(gSequence.e[i] / eFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 2] :=
      FloatToStrF(gSequence.ACTH[i] / ACTHFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 2] :=
      FloatToStrF(gSequence.PRF[i] / PRFFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 2] :=
      FloatToStrF(gSequence.F[i] / CortisolFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 2] := FloatToStrF(gSequence.yr[i] / yRFactor, ffFixed, 0, 4);
  end;
  PlotForm.ShowPlot;
  gSequence.Destroy;
end;

procedure TValuesForm.SteadyStateButtonClick(Sender: TObject);
var
  params: TParams;
begin
  ReadParams(Sender, params);
  gPrediction := PredictSteadyState(CRHSpinEdit.Value * CRHFactor, params);
  PredictionForm.DisplayPrediction(gPrediction[0], gPrediction[1]);
end;

procedure TValuesForm.EvolveButtonClick(Sender: TObject);
var
  params: TParams;
  EvoTargets: TEvoTargets;
begin
  if EstimateGRCheckbox.Checked or EstimateGECheckbox.Checked then
  begin
    params.G1 := G1Edit.Value;
    params.G3 := G3Edit.Value;
    params.GA := GAEdit.Value * GAFactor;
    if EstimateGRCheckbox.Checked then
      params.GR := Math.NaN
    else
      params.GR := GREdit.Value;
    if EstimateGECheckbox.Checked then
      params.GE := Math.NaN
    else
      params.GE := GEEdit.Value;
    params.DA := DAEdit.Value * DAFactor;
    params.DR := DREdit.Value * DRFactor;
    TargetForm.Show;
    EvoTargets.ACTH := TargetForm.targetA;
    EvoTargets.F := TargetForm.targetF;
    GeneticAlgorithm(PopulationSize, CRHSpinEdit.Value * CRHFactor, params,
      LowerBound, UpperBound, EvoTargets, Generations, MutationRate,
      AllPopulations, FittestIndividuals);
    SetParams(Sender, params);
  end;
end;

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  ValuesForm.WinAboutItem.Visible := False;
  ValuesForm.AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  ValuesForm.WinAboutItem.Visible := False;
  ValuesForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  ValuesForm.WinAboutItem.Visible := True;
  ValuesForm.AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  ValuesForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  ValuesForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  ValuesForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  ValuesForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  ValuesForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  ValuesForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  ValuesForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  ValuesForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  ValuesForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  ValuesForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TValuesForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.ShowAboutWindow(Sender: TObject);
begin
  AboutWindow.ShowModal;
end;

procedure TValuesForm.CopyCells(Sender: TObject);
begin
  CutorCopyfromGrid(ValuesGrid, False);
end;

procedure TValuesForm.ReadParams(Sender: TObject; var params: TParams);
begin
  params.G1 := G1Edit.Value;
  params.G3 := G3Edit.Value;
  params.GA := GAEdit.Value * GAFactor;
  params.GR := GREdit.Value;
  params.GE := GEEdit.Value;
  params.DA := DAEdit.Value * DAFactor;
  params.DR := DREdit.Value * DRFactor;
end;

procedure TValuesForm.SetParams(Sender: TObject; const params: TParams);
begin
  G1Edit.Value := params.G1;
  G3Edit.Value := params.G3;
  GAEdit.Value := params.GA / GAFactor;
  GREdit.Value := params.GR;
  GEEdit.Value := params.GE;
  DAEdit.Value := params.DA / DAFactor;
  DREdit.Value := params.DR / DRFactor;
end;

procedure TValuesForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TValuesForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Left := 13;
  AdaptMenus;
  CRHSpinEdit.Value := kCRH / CRHFactor;
  for i := 1 to ValuesGrid.ColCount - 1 do
    ValuesGrid.Cells[i, 1] := kUoMs[i];
  ValuesGrid.Columns[0].Font.Color := clDarkOrange;
  ValuesGrid.Columns[1].Font.Color := clDarkOrange;
  ValuesGrid.Columns[2].Font.Color := clGoldenRod;
end;

procedure TValuesForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TValuesForm.CopyMenuItemClick(Sender: TObject);
begin
  CopyCells(Sender);
end;

procedure TValuesForm.EstimateGECheckboxChange(Sender: TObject);
begin
  if EstimateGECheckbox.Checked then
  begin
    GEEdit.Enabled := False;
    GEEdit.Value := 0;
  end
  else
    GEEdit.Enabled := True;
end;

procedure TValuesForm.EstimateGRCheckBoxChange(Sender: TObject);
begin
  if EstimateGRCheckbox.Checked then
  begin
    GREdit.Enabled := False;
    GREdit.Value := 0;
  end
  else
    GREdit.Enabled := True;
end;


end.
