unit SimuladrenTypes;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Global types and constants }

{ Version 1.1.0 (Desarrollo) }

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

const
  kTAB = char(9);
  kCRLF = #13#10;
  kSEMICOLON = ';';

  clGoldenRod = TColor($20A5DA);
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
    'fmol/l', 'fmol/s', 'pmol/L', 'pmol/s', 'nmol/L', 'mAU');

  kEvoTargets: TEvoTargets =
    (
    ACTH: 6.81;
    F: 175.88;
    );

  SimulAdrenID = 'Simuladren';
  SimulAdrenVersionString = SimulAdrenID + ' 1.1 (Desarrollo)';

  BASE_URL = 'http://simuladren.sf.net';
  SIMULADREN_GLOBAL_ID = 'net.sf.simuladren';
  HELP_URL = 'http://simuladren.sf.net';

  SCICRUNCH_URL = 'https://rrid.site/data/record/nlx_144509-1/SCR_027314/resolver?i=rrid:scr_027314';

  SAVE_ERROR_MESSAGE = 'Error saving the file';

implementation

end.
