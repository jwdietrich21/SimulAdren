unit SetTargets;

{ SimulAdren}

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ GUI for targets of evoluationay algorithm }

{ Version 1.3.0 (Green Lizard) }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  SimulAdrenTypes, SimulationEngine, evoEngine;

type

  { TTargetForm }

  TTargetForm = class(TForm)
    BoundsLabel: TLabel;
    MutationRateSpinEdit: TFloatSpinEdit;
    TournamentSizeSpinEdit: TSpinEdit;
    TournamentSizeLabel: TLabel;
    HyphenLabel: TLabel;
    GenerationsLabel: TLabel;
    MutationRateLabel: TLabel;
    PopSizeLabel: TLabel;
    PopSizeSpinEdit: TSpinEdit;
    GenerationsSpinEdit: TSpinEdit;
    UpperBoundSpinEdit: TSpinEdit;
    ParametersGroupBox: TGroupBox;
    LowerBoundSpinEdit: TSpinEdit;
    TargetGroupBox: TGroupBox;
    UoM_A: TLabel;
    CancelButton: TButton;
    OKButton: TButton;
    FloatSpinEditA: TFloatSpinEdit;
    FloatSpinEditF: TFloatSpinEdit;
    ALabel: TLabel;
    FLabel: TLabel;
    UoM_F: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    targetA, targetF: extended;
  end;

var
  TargetForm: TTargetForm;

implementation

{$R *.lfm}

{ TTargetForm }

procedure TTargetForm.OKButtonClick(Sender: TObject);
begin
  TargetA := FloatSpinEditA.Value * ACTHFactor;
  TargetF := FloatSpinEditF.Value * CortisolFactor;
  Close;
  ModalResult := mrOK;
end;

procedure TTargetForm.CancelButtonClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TTargetForm.FormCreate(Sender: TObject);
begin
  targetA := kEvoTargets.ACTH;
  targetF := kEvoTargets.F;
  FloatSpinEditA.Value := targetA;
  FloatSpinEditF.Value := targetF;
  UoM_A.Caption := kUoMs[3]; // UoM for ACTH
  UoM_F.Caption := kUoMs[5]; // UoM for F (Cortisol)
  LowerBoundSpinEdit.Value := LowerBound;
  UpperBoundSpinEdit.Value := UpperBound;
  PopSizeSpinEdit.Value := PopulationSize;
  GenerationsSpinEdit.Value := Generations;
  MutationRateSpinEdit.Value := MutationRate;
  TournamentSizeSpinEdit.Value := TournamentSize;
end;

procedure TTargetForm.FormShow(Sender: TObject);
begin
  ActiveControl := OKButton;
  OKButton.SetFocus;
end;

end.

