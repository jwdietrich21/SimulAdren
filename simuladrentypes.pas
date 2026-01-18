unit SimuladrenTypes;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Global types and constants }

{ Version 1.2.0 (Emerald) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Nina Siegmar, 2020 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

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
  Classes, SysUtils, Graphics;

type
  TStrucPars = record
    G1, G3, GA, DA, GR, DR, GE: extended;
  end;

  TEvoTargets = record
    ACTH, F: extended;
    LowEdge, HighEdge: integer;
    PopulationSize, Generations: integer;
    MutationRate: real;
    TournamentSize: integer;
  end;

  tActiveModel = record
    Name: ansistring; // MIRIAM 1
    Reference: ansistring; // MIRIAM 2
    Species: ansistring;
    Creators: ansistring; // MIRIAM 3
    Created: TDateTime; // MIRIAM 4
    LastModified: TDateTime; // MIRIAM 4
    Terms: ansistring; // MIRIAM 5
    Code: ansistring; // MIASE
    Comments: ansistring; // MIASE
    StrucPars: TStrucPars;
    Iterations: integer;
    Imported: boolean;
  end;

  Str13 = string[13];

const
  kNULL = char(0);
  kTAB = char(9);
  kCRLF = #13#10;
  kSEMICOLON = ';';
  kPERIOD = '.';

  clGoldenRod = TColor($20A5DA);
  clDarkOrange = TColor($008CFF);

  kCRH = 5e-15;

  kStrucPars: TStrucPars =
    ( // default values
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
    'fmol/l', 'fmol/s', 'pmol/L', 'pmol/s', 'nmol/L', 'mAU');

  kEvoTargets: TEvoTargets =
    (
    ACTH: 6.81;
    F: 175.88;
    );

  SimulAdrenID = 'Simuladren';
  SimulAdrenVersionString = SimulAdrenID + ' 1.1 (Desarrollo)';

  ISO_8601_DATE_FORMAT = 'YYYY-MM-DD"T"hh:nn:ss'; {Date/time format in XML representation}
  STANDARD_NUM_FORMAT = '###,##0.0000';
  SHORT_NUM_FORMAT = '###,###.00';
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

var
  gNumberFormat, gDateTimeFormat: String;
  gFormatSettings, gUSFormatSettings: TFormatSettings;

implementation

initialization
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

end.
