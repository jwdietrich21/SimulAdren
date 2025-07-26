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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus,
  SimuladrenTypes, SimulationEngine, Prediction, Plot, GUIServices, AboutBox,
  evoEngine;

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
    procedure EvolveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure SteadyStateButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowAboutWindow(Sender: TObject);
    procedure CopyCells(Sender: TObject);
  end;

var
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

{ TValuesForm }

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  i, j, iterations: integer;
begin
  iterations := IterationsSpinEdit.Value;
  gValues := TValues.Create;
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
  RunSimulation(CRHSpinEdit.Value * CRHFactor, G1Edit.Value, G3Edit.Value,
    GAEdit.Value * GAFactor, GREdit.Value, GEEdit.Value, DAEdit.Value *
    DAFactor, DREdit.Value * DRFactor, iterations);
  PredictionForm.DisplayPrediction(gPrediction1, gPrediction2);
  if iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := iterations + 1;
  for i := 0 to iterations - 1 do
  begin
    ValuesGrid.Cells[0, i + 2] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 2] := FloatToStrF(gValues.CRH[i] / CRHFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 2] := FloatToStrF(gValues.e[i] / eFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 2] := FloatToStrF(gValues.ACTH[i] / ACTHFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 2] := FloatToStrF(gValues.PRF[i] / PRFFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 2] := FloatToStrF(gValues.F[i] / CortisolFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 2] := FloatToStrF(gValues.yr[i] / yRFactor, ffFixed, 0, 4);
  end;
  PlotForm.ShowPlot;
  gValues.Destroy;
end;

procedure TValuesForm.SteadyStateButtonClick(Sender: TObject);
begin
  PredictSteadyState(CRHSpinEdit.Value * CRHFactor, G1Edit.Value, G3Edit.Value,
      GAEdit.Value * GAFactor, GREdit.Value, GEEdit.Value, DAEdit.Value *
      DAFactor, DREdit.Value * DRFactor);
  PredictionForm.DisplayPrediction(gPrediction1, gPrediction2);
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
  CutorCopyfromGrid(ValuesGrid, false);
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

procedure TValuesForm.EvolveButtonClick(Sender: TObject);
begin

end;

end.
