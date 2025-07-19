unit AboutBox;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ About Box }

{ Version 1.0.0 (Rudolphina) }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EnvironmentInfo;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
    BigLogo: TImage;
    CopyrightLabel1: TLabel;
    CopyrightLabel10: TLabel;
    CopyrightLabel11: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    CopyrightLabel8: TLabel;
    CopyrightLabel9: TLabel;
    SciCrunchLabel: TLabel;
    SimulaBetaLabel: TImage;
    URL1: TLabel;
    VersionLabel: TLabel;
    procedure BigLogoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutWindow: TAboutWindow;

implementation

{$R *.lfm}

{ TAboutWindow }

procedure TAboutWindow.BigLogoClick(Sender: TObject);
begin

end;

procedure TAboutWindow.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
end;

end.

