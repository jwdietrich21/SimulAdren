unit SetTargets;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ GUI for targets of evoluationay algorithm }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  SimulAdrenTypes, SimulationEngine;

type

  { TTargetForm }

  TTargetForm = class(TForm)
    CancelButton: TButton;
    ExplanationLabel: TLabel;
    OKButton: TButton;
    FloatSpinEditA: TFloatSpinEdit;
    FloatSpinEditF: TFloatSpinEdit;
    ALabel: TLabel;
    FLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Close;
  TargetA := FloatSpinEditA.Value * ACTHFactor;
  TargetF := FloatSpinEditF.Value * CortisolFactor;
end;

procedure TTargetForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTargetForm.FormCreate(Sender: TObject);
begin
  targetA := kEvoTargets.ACTH;
  targetF := kEvoTargets.F;
  FloatSpinEditA.Value := targetA;
  FloatSpinEditF.Value := targetF;
end;



end.

