unit GUI;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ GUI }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus, Math,
  SimuladrenTypes, SimuladrenResources, SimulationEngine, Prediction, Plot,
  GUIServices, AboutBox, SetTargets, evoEngine, FitnessPlot, ParameterPlot,
  DIFSupport, ScenarioHandler, IPS, HandleInitialConditions;

type

  { TValuesForm }

  TValuesForm = class(TForm)
    AcrophaseFloatSpinEdit: TFloatSpinEdit;
    AcrophaseLabel: TLabel;
    Alpha3Edit: TFloatSpinEdit;
    Alpha3Label: TLabel;
    Alpha3UnitLabel: TLabel;
    AmplitudeFloatSpinEdit: TFloatSpinEdit;
    AmplitudeLabel: TLabel;
    Beta1Edit: TFloatSpinEdit;
    Beta3Edit: TFloatSpinEdit;
    Beta1Label: TLabel;
    Beta3Label: TLabel;
    Beta1UnitLabel: TLabel;
    AppleMenu: TMenuItem;
    Alpha1Edit: TFloatSpinEdit;
    Alpha1Label: TLabel;
    Alpha1UnitLabel: TLabel;
    Beta3UnitLabel: TLabel;
    CustomRadioButton: TRadioButton;
    ChronoGroupBox: TGroupBox;
    MesorFloatSpinEdit: TFloatSpinEdit;
    MesorLabel: TLabel;
    StrucParGroupBox: TGroupBox;
    ICGroupBox: TGroupBox;
    SimulationControlGroupBox: TGroupBox;
    HoursRadioButton: TRadioButton;
    EvolvedParameterMenuitem: TMenuItem;
    FitnessMenuItem: TMenuItem;
    ICRadioButton: TRadioButton;
    ContinueRadioButton: TRadioButton;
    BeginLabel: TLabel;
    ResetButton: TButton;
    SimControlMenuItem: TMenuItem;
    PredictionMenuItem: TMenuItem;
    PlotMenuItem: TMenuItem;
    IPSMenuItem: TMenuItem;
    TauFloatSpinEdit: TFloatSpinEdit;
    TauLabel: TLabel;
    WindowMenu: TMenuItem;
    MinutesRadioButton: TRadioButton;
    ModelVersionLabel: TLabel;
    ModelVersionComboBox: TComboBox;
    EstimateGECheckbox: TCheckBox;
    EstimateGRCheckBox: TCheckBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Shape1: TShape;
    Shape2: TShape;
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
    DRFactorLabel: TLabel;
    GELabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    ToolButton1: TToolButton;
    OpenToolButton: TToolButton;
    SaveToolButton: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    StartButton: TButton;
    ValuesGrid: TStringGrid;
    ToolBar1: TToolBar;
    TimeLabel: TLabel;
    GALabel: TLabel;
    GAEdit: TFloatSpinEdit;
    procedure Alpha1EditChange(Sender: TObject);
    procedure Alpha3EditChange(Sender: TObject);
    procedure Beta1EditChange(Sender: TObject);
    procedure Beta3EditChange(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure ContinueRadioButtonChange(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure CustomRadioButtonChange(Sender: TObject);
    procedure CustomRadioButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EstimateGECheckboxChange(Sender: TObject);
    procedure EstimateGRCheckBoxChange(Sender: TObject);
    procedure EvolvedParameterMenuitemClick(Sender: TObject);
    procedure FitnessMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HoursRadioButtonChange(Sender: TObject);
    procedure ICRadioButtonChange(Sender: TObject);
    procedure ICRadioButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure IPSMenuItemClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MinutesRadioButtonChange(Sender: TObject);
    procedure ModelVersionComboBoxChange(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure PlotMenuItemClick(Sender: TObject);
    procedure PredictionMenuItemClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure SaveToolButtonClick(Sender: TObject);
    procedure SimControlMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure EvolveButtonClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure SteadyStateButtonClick(Sender: TObject);
    procedure SaveGrid(theFileName: string; theDelimiter: char);
  private
    { private declarations }
    procedure CheckEvolveEnabling(Sender: TObject);
    procedure ClearOutput(Sender: TObject);
  public
    { public declarations }
    AllPopulations: TAllPopulations;
    FittestIndividuals: TFittest;
    SimTimeUnit, TestTimeUnit: TTimeUnit;
    procedure ShowAboutWindow(Sender: TObject);
    procedure CopyCells(Sender: TObject);
    procedure ReadParams(Sender: TObject; var params: TStrucPars);
    procedure SetParams(Sender: TObject; var theModel: tActiveModel);
    procedure SetModel(Sender: TObject; default: boolean);
    procedure SetG1(Sender: TObject);
    procedure SetG3(Sender: TObject);
  end;

var
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

function VersionID(theString: string): string;
begin
  Result := StringReplace(theString, 'Model ', '', [rfReplaceAll, rfIgnoreCase]);
end;

procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colnames, rowNames, hasGridColumns: boolean;
  var ReturnCode: integer);
{saves the contents of a string grid}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theString: string;
  r, c: integer;
  startC: integer;
  theContents: TStringList;
  doc: TDIFDocument;
  theCode: integer;
begin
  if rowNames then
    startC := 0
  else
    startC := 1;
  if theDelimiter = 'd' then
  begin {DIF file handling}
    theCode := 0;
    try
      doc := TDIFDocument.Create;
      doc.SetHead(SimulAdrenID);

      if colNames then
      begin
        doc.NewTuple;
        theString := '';
        if hasGridColumns then
        begin
          theString := 'i';
          Doc.AppendCell(theString);
          for c := startC to theTable.ColCount - 2 do
          begin
            theString := theTable.Columns[c].Title.Caption;
            Doc.AppendCell(theString);
          end;
        end
        else
          for c := startC to theTable.ColCount - 1 do
          begin
            theString := theTable.Cells[c, 0];
            Doc.AppendCell(theString);
          end;
      end;
      for r := 1 to theTable.RowCount - 1 do
      begin
        doc.NewTuple;
        theString := '';
        for c := startC to theTable.ColCount - 1 do
        begin
          theString := theTable.Cells[c, r];
          Doc.AppendCell(theString);
        end;
      end;

      WriteDIFFile(doc, theFileName, theCode);
      if theCode <> 0 then
        ShowSaveError;
    finally
      doc.Free;
      ReturnCode := theCode;
    end;
  end
  else if theDelimiter <> ' ' then {tab delimited and CSV files}
  begin
    if theDelimiter = 't' then
      theDelimiter := kTAB;
    if theDelimiter = 'c' then
      theDelimiter := kSEMICOLON;
    ReturnCode := 0;
    theContents := TStringList.Create;
    theString := '';
    if colNames then
    begin
      if hasGridColumns then
      begin
        theString := 'i' + theDelimiter;
        for c := startC to theTable.ColCount - 2 do
          theString := theString + theTable.Columns[c].Title.Caption + theDelimiter;
      end
      else
        for c := startC to theTable.ColCount - 1 do
          theString := theString + theTable.Cells[c, 0] + theDelimiter;
      theContents.Add(theString);
    end;
    for r := 1 to theTable.RowCount - 1 do
    begin
      theString := '';
      for c := startC to theTable.ColCount - 1 do
        theString := theString + theTable.Cells[c, r] + theDelimiter;
      theContents.Add(theString);
    end;
    try
      try
        theContents.SaveToFile(theFileName);
      except
        on Ex: EFCreateError do
        begin
          ShowMessage(SAVE_ERROR_MESSAGE);
          ReturnCode := -2;
        end;
      end;
    finally
      theContents.Free;
    end;
  end
  else
  begin
    ShowSaveError;
    ReturnCode := -1;
  end;
end;


{ TValuesForm }

procedure TValuesForm.ClearOutput(Sender: TObject);
begin
  ValuesGrid.RowCount := gActiveModel.iterations + 2;
  PlotForm.CRHSeries.Clear;
  PlotForm.PRFSeries.Clear;
  PlotForm.FSeries.Clear;
  PlotForm.eSeries.Clear;
  PlotForm.ACTHSeries.Clear;
  PlotForm.yrSeries.Clear;
end;

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  i, j, nmin: integer;
  params: TStrucPars;
begin
  //ValuesForm.cursor := crHourGlass;
  ReadParams(Sender, params);
  if ContinueRadioButton.Checked or CustomRadioButton.Checked then
    nmin := gActiveModel.iterations
  else
    nmin := 0;
  if SimTimeUnit = minutes then
    gActiveModel.iterations := nmin + IterationsSpinEdit.Value * SecsPerMin
  else if SimTimeUnit = hours then
    gActiveModel.iterations :=
      nmin + IterationsSpinEdit.Value * MinsPerHour * SecsPerMin;
  if nmin = 0 then
    ClearOutput(Sender)
  else
  begin
    ValuesGrid.RowCount := gActiveModel.iterations + 2;
    for i := 0 to ValuesGrid.ColCount - 1 do
      for j := nmin + 2 to ValuesGrid.RowCount - 1 do
        ValuesGrid.Cells[i, j] := '';
  end;
  RunSimulation(gInitialConditions, gActiveModel, nmin);
  PredictionForm.DisplayPrediction(gPrediction[0], gPrediction[1]);
  if gActiveModel.iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := gActiveModel.iterations + 1;
  for i := 0 to gActiveModel.iterations - 1 do
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
  //ValuesForm.cursor := crDefault;
end;

procedure TValuesForm.SteadyStateButtonClick(Sender: TObject);
var
  params: TStrucPars;
begin
  ReadParams(Sender, params);
  gPrediction := PredictSteadyState(gInitialconditions.CRH, gActiveModel);
  PredictionForm.DisplayPrediction(gPrediction[0], gPrediction[1]);
end;

procedure TValuesForm.SaveGrid(theFileName: string; theDelimiter: char);
{saves the contents of the log window}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  theCode := 0;
  SaveGridToFile(ValuesGrid, theFileName, theDelimiter, True, True, True, theCode);
  if theCode <> 0 then
    ShowSaveError;
end;

procedure TValuesForm.EvolveButtonClick(Sender: TObject);
var
  testModel: tActiveModel;
  EvoTargets: TEvoTargets;
begin
  if EstimateGRCheckbox.Checked or EstimateGECheckbox.Checked then
  begin
    TargetForm.ShowModal;
    if TargetForm.ModalResult = mrOk then
    begin
      testModel.StrucPars.G1 := G1Edit.Value;
      testModel.StrucPars.G3 := G3Edit.Value;
      testModel.StrucPars.GA := GAEdit.Value * GAFactor;
      if EstimateGRCheckbox.Checked then
        testModel.StrucPars.GR := Math.NaN
      else
        testModel.StrucPars.GR := GREdit.Value;
      if EstimateGECheckbox.Checked then
        testModel.StrucPars.GE := Math.NaN
      else
        testModel.StrucPars.GE := GEEdit.Value;
      testModel.StrucPars.DA := DAEdit.Value * DAFactor;
      testModel.StrucPars.DR := DREdit.Value * DRFactor;
      testModel.Version := VersionID(ModelVersionComboBox.Caption);
      if (testModel.Version <> '1') and (testModel.Version <> '1.1') then
      begin
        testModel.StrucPars.alpha1 := alpha1Edit.Value;
        testModel.StrucPars.beta1 := beta1Edit.Value;
        testModel.StrucPars.alpha3 := alpha3Edit.Value;
        testModel.StrucPars.beta3 := beta3Edit.Value;
      end;
      EvoTargets.ACTH := TargetForm.targetA;
      EvoTargets.F := TargetForm.targetF;
      EvoTargets.LowEdge := TargetForm.LowerBoundSpinEdit.Value;
      EvoTargets.HighEdge := TargetForm.UpperBoundSpinEdit.Value;
      EvoTargets.PopulationSize := TargetForm.PopSizeSpinEdit.Value;
      EvoTargets.Generations := TargetForm.GenerationsSpinEdit.Value;
      EvoTargets.MutationRate := TargetForm.MutationRateSpinEdit.Value;
      EvoTargets.TournamentSize := TargetForm.TournamentSizeSpinEdit.Value;
      FitnessPlotForm.Show;
      ParameterForm.Show;
      GeneticAlgorithm(gInitialconditions.CRH, testModel, EvoTargets,
        AllPopulations, FittestIndividuals);
      FitnessPlotForm.DrawFitness(FittestIndividuals);
      ParameterForm.DrawParameters(FittestIndividuals);
      SetParams(Sender, testModel);
      gActiveModel.StrucPars := testModel.StrucPars;
    end;
  end;
end;

procedure TValuesForm.SaveMenuItemClick(Sender: TObject);
var
  theForm: TForm;
  delimiter: char;
  imageType: TImageType;
  fileName: string;
  theFilterIndex: integer;
begin
  delimiter := kNull;
  theForm := Screen.ActiveForm;
  if theForm = ValuesForm then
  begin
    if SaveDialog1.Execute then
    begin
      fileName := SaveDialog1.FileName;
      theFilterIndex := SaveDialog1.FilterIndex;
      case theFilterIndex of
        1: delimiter := kTab; // Tab-delimited
        2: if DefaultFormatSettings.DecimalSeparator = ',' then
            delimiter := ';'  // CSV
          else
            delimiter := ','; // CSV
        3: delimiter := 'd';  // DIF
        4: delimiter := kNull;
        5: delimiter := ' ';
      end;
      if delimiter = kNULL then
      begin
        ReadParams(Sender, gActiveModel.StrucPars);
        SaveScenario(gActiveModel, fileName);
      end
      else
        SaveGrid(fileName, delimiter);
    end;
  end
  else if theForm = PlotForm then
  begin
    SaveDialog2.FilterIndex := 4;
    if SaveDialog2.Execute then
    begin
      fileName := SaveDialog2.FileName;
      theFilterIndex := SaveDialog2.FilterIndex;
      case theFilterIndex of
        1: ImageType := NULL;
        2: imageType := BMP;
        3: ImageType := XPM;
        4: ImageType := PNG;
        5: ImageType := PBM;
        6: ImageType := JPG;
        7: ImageType := TIFF;
        8: ImageType := SVG;
      end;
      PlotForm.SaveChart(fileName, imageType);
    end;
  end
  else if theForm = ParameterForm then
  begin
    SaveDialog2.FilterIndex := 4;
    if SaveDialog2.Execute then
    begin
      fileName := SaveDialog2.FileName;
      theFilterIndex := SaveDialog2.FilterIndex;
      case theFilterIndex of
        1: ImageType := NULL;
        2: imageType := BMP;
        3: ImageType := XPM;
        4: ImageType := PNG;
        5: ImageType := PBM;
        6: ImageType := JPG;
        7: ImageType := TIFF;
        8: ImageType := SVG;
      end;
      ParameterForm.SaveChart(fileName, imageType);
    end;
  end
  else if theForm = FitnessPlotForm then
  begin
    SaveDialog2.FilterIndex := 4;
    if SaveDialog2.Execute then
    begin
      fileName := SaveDialog2.FileName;
      theFilterIndex := SaveDialog2.FilterIndex;
      case theFilterIndex of
        1: ImageType := NULL;
        2: imageType := BMP;
        3: ImageType := XPM;
        4: ImageType := PNG;
        5: ImageType := PBM;
        6: ImageType := JPG;
        7: ImageType := TIFF;
        8: ImageType := SVG;
      end;
      FitnessPlotForm.SaveChart(fileName, imageType);
    end;
  end
  else if theForm = IPSForm then
  begin
    SaveDialog2.FilterIndex := 4;
    if SaveDialog2.Execute then
    begin
      fileName := SaveDialog2.FileName;
      theFilterIndex := SaveDialog2.FilterIndex;
      case theFilterIndex of
        1: ImageType := NULL;
        2: imageType := BMP;
        3: imageType := XPM;
        4: imageType := PNG;
        5: imageType := PBM;
        6: imageType := JPG;
        7: imageType := TIFF;
        8: imageType := SVG;
      end;
      IPSForm.SaveBlockDiagram(filename, imageType);
    end;
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

procedure TValuesForm.ReadParams(Sender: TObject; var params: TStrucPars);
begin
  params.G1 := G1Edit.Value;
  params.G3 := G3Edit.Value;
  params.GA := GAEdit.Value * GAFactor;
  params.GR := GREdit.Value;
  params.GE := GEEdit.Value;
  params.DA := DAEdit.Value * DAFactor;
  params.DR := DREdit.Value * DRFactor;
  params.alpha1 := Alpha1Edit.Value;
  params.beta1 := Beta1Edit.Value;
  params.alpha3 := Alpha3Edit.Value;
  params.beta3 := Beta3Edit.Value;
  GActiveModel.StrucPars := params;
  gReferenceInput.CRH.mesor := MesorFloatSpinEdit.Value * CRHFactor;
  gReferenceInput.CRH.amplitude := AmplitudeFloatSpinEdit.Value;
  gReferenceInput.CRH.acrophase := AcrophaseFloatSpinEdit.Value;
  gReferenceInput.CRH.tau := TauFloatSpinEdit.Value;
  gInitialconditions.CRH := gReferenceInput.CRH.mesor;
end;

procedure TValuesForm.SetParams(Sender: TObject; var theModel: tActiveModel);
// Set entry fields in GUI to the parameters of the model
begin
  G1Edit.Value := theModel.StrucPars.G1;
  G3Edit.Value := theModel.StrucPars.G3;
  GAEdit.Value := theModel.StrucPars.GA / GAFactor;
  GREdit.Value := theModel.StrucPars.GR;
  GEEdit.Value := theModel.StrucPars.GE;
  DAEdit.Value := theModel.StrucPars.DA / DAFactor;
  DREdit.Value := theModel.StrucPars.DR / DRFactor;
  if (theModel.Version <> '1') and (theModel.Version <> '1.1') then
  begin
    Alpha1Edit.Value := theModel.StrucPars.alpha1;
    Beta1Edit.Value := theModel.StrucPars.beta1;
    Alpha3Edit.Value := theModel.StrucPars.alpha3;
    Beta3Edit.Value := theModel.StrucPars.beta3;
  end;
  if theModel.Version = '1.0' then
    ModelVersionCombobox.Caption := 'Model 1'
  else
    ModelVersionCombobox.Caption := 'Model ' + theModel.Version;
  MesorFloatSpinEdit.Value := gInitialconditions.CRH / CRHFactor;
end;

procedure AssignParams(var theModel: TActiveModel; params: TStrucPars);
begin
  with theModel.StrucPars do
  begin
    alpha1 := params.alpha1;
    beta1 := params.beta1;
    alpha3 := params.alpha3;
    beta3 := params.beta3;
    if beta1 > 0 then
      G1 := alpha1 / beta1
    else
      G1 := params.G1;
    if beta3 > 0 then
      G3 := alpha3 / beta3
    else
      G3 := params.G3;
    GA := params.GA;
    DA := params.DA;
    GR := params.GR;
    DR := params.DR;
    GE := params.GE;
  end;
end;

procedure TValuesForm.SetModel(Sender: TObject; default: boolean);
begin
  gActiveModel.Version := VersionID(ModelVersionComboBox.Caption);
  if default then
    case gActiveModel.Version of
      '1', '1.0': AssignParams(gActiveModel, kStrucPars_1);
      '1.1': AssignParams(gActiveModel, kStrucPars_1_1);
      '1.2': AssignParams(gActiveModel, kStrucPars_1_2);
      '1.3': AssignParams(gActiveModel, kStrucPars_1_3);
      '1.4': AssignParams(gActiveModel, kStrucPars_1_4);
      '1.5': AssignParams(gActiveModel, kStrucPars_1_5);
    end;
  case gActiveModel.Version of
    '1', '1.0', '1.1': // Model versions 1 or 1.1?
    begin
      G1Edit.Enabled := True;
      Alpha1Edit.Enabled := False;
      Beta1Edit.Enabled := False;
      G3Edit.Enabled := True;
      Alpha3Edit.Enabled := False;
      Beta3Edit.Enabled := False;
      gInitialconditions.CRH := kCRH_old;
    end;
    '1.2', '1.3', '1.4': // Model versions 1.2 to 1.4?
    begin
      G1Edit.Enabled := False;
      Alpha1Edit.Enabled := True;
      Beta1Edit.Enabled := True;
      G3Edit.Enabled := False;
      Alpha3Edit.Enabled := True;
      Beta3Edit.Enabled := True;
      SetG1(Sender);
      SetG3(Sender);
     gInitialconditions.CRH := kCRH_old;
    end
    otherwise          // newer model versions?
    begin
      G1Edit.Enabled := False;
      Alpha1Edit.Enabled := True;
      Beta1Edit.Enabled := True;
      G3Edit.Enabled := False;
      Alpha3Edit.Enabled := True;
      Beta3Edit.Enabled := True;
      SetG1(Sender);
      SetG3(Sender);
      gInitialConditions.CRH := kCRH_new;
    end;
  end;
  SetParams(Sender, gActiveModel);
end;

procedure TValuesForm.SetG1(Sender: TObject);
begin
  G1Edit.Value := Alpha1Edit.Value / Beta1Edit.Value;
end;

procedure TValuesForm.SetG3(Sender: TObject);
begin
  G3Edit.Value := Alpha3Edit.Value / Beta3Edit.Value;
end;

procedure TValuesForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.HoursRadioButtonChange(Sender: TObject);
begin
  if HoursRadioButton.Checked then
  begin
    MinutesRadioButton.Checked := False;
    SimTimeUnit := hours;
  end
  else
  begin
    SimTimeUnit := minutes;
  end;
  TestTimeUnit := SimTimeUnit;
end;

procedure TValuesForm.ICRadioButtonChange(Sender: TObject);
begin
  if ContinueRadioButton.Checked then
  begin
    ContinueRadioButton.Checked := False;
    CustomRadioButton.Checked := False;
    ICRadioButton.Checked := True;
  end;
end;

procedure TValuesForm.ICRadioButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  InitialConditionsForm.Invalidate; // forces redrawing
  InitialConditionsForm.ShowModal;
  if InitialConditionsForm.response = mrOk then
    ICRadioButton.Checked := True;
end;

procedure TValuesForm.ContinueRadioButtonChange(Sender: TObject);
begin
  if ICRadioButton.Checked then
  begin
    ICRadioButton.Checked := False;
    CustomRadioButton.Checked := False;
    ContinueRadioButton.Checked := True;
  end;
end;

procedure TValuesForm.CustomRadioButtonChange(Sender: TObject);
begin
  if CustomRadioButton.Checked then
  begin
    ICRadioButton.Checked := False;
    CustomRadioButton.Checked := True;
    ContinueRadioButton.Checked := False;
  end;
end;

procedure TValuesForm.CustomRadioButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  InitialConditionsForm.Invalidate; // forces redrawing
  InitialConditionsForm.ShowModal;
  if (InitialConditionsForm.response = mrOk) and assigned(gSequence) then
  begin
    i := gSequence.size - 1;
    gSequence.CRH[i] := gInitialConditions.CRH;
    gSequence.e[i] := gInitialConditions.e;
    gSequence.ACTH[i] := gInitialConditions.ACTH;
    gSequence.PRF[i] := gInitialConditions.PRF;
    gSequence.F[i] := gInitialConditions.F;
    gSequence.yR[i] := gInitialConditions.yR;
    gBlocks.ASIA1.x1 := gInitialConditions.ACTH;
    gBlocks.ASIA3.x1 := gInitialConditions.F;
    CustomRadioButton.Checked := True;
  end;
end;

procedure TValuesForm.IPSMenuItemClick(Sender: TObject);
begin
  IPSForm.Show;
end;

procedure TValuesForm.MinutesRadioButtonChange(Sender: TObject);
begin
  if MinutesRadioButton.Checked then
  begin
    HoursRadioButton.Checked := False;
    SimTimeUnit := minutes;
  end
  else
  begin
    SimTimeUnit := hours;
  end;
  TestTimeUnit := SimTimeUnit;
end;

procedure TValuesForm.ModelVersionComboBoxChange(Sender: TObject);
begin
  SetModel(Sender, True);
  IPSForm.ModelChanged := True;
  IPSForm.Invalidate; // forces redrawing
end;

procedure TValuesForm.OpenMenuItemClick(Sender: TObject);
var
  theFileName: string;
  theFilterIndex: integer;
begin
  if OpenDialog1.Execute then
  begin
    theFileName := OpenDialog1.FileName;
    theFilterIndex := OpenDialog1.FilterIndex;
    case theFilterIndex of
      1:
      begin
        ReadScenario(theFileName, gActiveModel);  {XML file}
        SetParams(Sender, gActiveModel);
        SetModel(Sender, False);
      end;
    end;
    IPSForm.ModelChanged := True;
    IPSForm.Invalidate; // forces redrawing
  end;
end;

procedure TValuesForm.OpenToolButtonClick(Sender: TObject);
begin
  OpenMenuItemClick(Sender);
end;

procedure TValuesForm.PlotMenuItemClick(Sender: TObject);
begin
  PlotForm.Show;
end;

procedure TValuesForm.PredictionMenuItemClick(Sender: TObject);
begin
  PredictionForm.Show;
end;

procedure TValuesForm.ResetButtonClick(Sender: TObject);
begin
  ClearSimulation;
  gActiveModel.iterations := 0;
  ClearOutput(Sender);
  ICRadioButton.Checked := True;
  ContinueRadioButton.Checked := False;
  CustomRadioButton.Checked := False;
end;

procedure TValuesForm.SaveToolButtonClick(Sender: TObject);
begin
  SaveMenuItemClick(Sender);
end;

procedure TValuesForm.SimControlMenuItemClick(Sender: TObject);
begin
  ValuesForm.Show;
end;

procedure TValuesForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
  while not Application.Terminated do
    Application.ProcessMessages;
  Close;
end;

procedure TValuesForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Scaled := True;
  Left := 13;
  AdaptMenus;
  SimTimeUnit := minutes;
  for i := 1 to ValuesGrid.ColCount - 1 do
    ValuesGrid.Cells[i, 1] := kUoMs[i];
  ValuesGrid.Columns[0].Font.Color := clDarkOrange;
  ValuesGrid.Columns[1].Font.Color := clDarkOrange;
  ValuesGrid.Columns[2].Font.Color := clGoldenRod;
  ValuesGrid.Columns[0].Title.Font.Color := clDarkOrange;
  ValuesGrid.Columns[1].Title.Font.Color := clDarkOrange;
  ValuesGrid.Columns[2].Title.Font.Color := clGoldenRod;
  SetModel(Sender, True);
end;

procedure TValuesForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
  while not Application.Terminated do
    Application.ProcessMessages;
  Close;
end;

procedure TValuesForm.Alpha1EditChange(Sender: TObject);
begin
  SetG1(Sender);
end;

procedure TValuesForm.Alpha3EditChange(Sender: TObject);
begin
  SetG3(Sender);
end;

procedure TValuesForm.Beta1EditChange(Sender: TObject);
begin
  SetG1(Sender);
end;

procedure TValuesForm.Beta3EditChange(Sender: TObject);
begin
  SetG3(Sender);
end;

procedure TValuesForm.CopyMenuItemClick(Sender: TObject);
begin
  CopyCells(Sender);
end;

procedure TValuesForm.CheckEvolveEnabling(Sender: TObject);
begin
  if EstimateGECheckbox.Checked or EstimateGRCheckbox.Checked then
    EvolveButton.Enabled := True
  else
    EvolveButton.Enabled := False;
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
  CheckEvolveEnabling(Sender);
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
  CheckEvolveEnabling(Sender);
end;

procedure TValuesForm.EvolvedParameterMenuitemClick(Sender: TObject);
begin
  ParameterForm.Show;
end;

procedure TValuesForm.FitnessMenuItemClick(Sender: TObject);
begin
  FitnessPlotForm.Show;
end;


end.
