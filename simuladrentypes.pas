unit SimuladrenTypes;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Global types and constants }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TStrucPars = record
    G1, G3, GA, DA, GR, DR, GE: extended;
  end;

const
  kTAB = char(9);
  kCRLF = #13#10;

  clGoldenRod  = TColor($20A5DA);
  clDarkOrange = TColor($008CFF);

  kCRH = 5e-15;

  kStrucPars: TStrucPars =
    (
    G1: 0.4 / 0.0002;
    G3: 1 / 21 / (ln(2) / (90 * 60));
    GA: 2.25 * 5.2e-10;
    DA: 1e-11;
    GR: 1;
    DR: 2e-7;
    GE: 1;
    );

  kUoMs: array [1..6] of string =
    ( // CRH, e, ACTH, PRF, F, yR
    'fmol/l', 'fmol/s', 'pmol/L', 'pmol/s', 'nmol/L', 'mAU'
    );

  SimulAdrenVersionString = 'Simuladren 1.0 Alpha';

implementation

end.

