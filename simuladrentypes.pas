unit SimuladrenTypes;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Global types and constants }

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

  TImageType = (NULL, BMP, XPM, PNG, PBM, JPG, TIFF, SVG);

  Str13 = string[13];

const
  kNULL = char(0);
  kTAB = char(9);
  kCRLF = #13#10;
  kSEMICOLON = ';';
  kPERIOD = '.';

  clGoldenRod = TColor($20A5DA);
  clDarkOrange = TColor($008CFF);

  MODEL_VERSION_1 = '1.1';

  kCRH = 5e-15;

  VD_ACTH = 2.5;
  alpha1 = 1 / VD_ACTH;
  beta1 = 0.0002;
  VD_F = 21;
  t12_F = 90;
  alpha3 = 1 / VD_F;
  beta3 = ln(2) / (t12_F * SecsPerMin);
  mPR_F = 5.2e-10;

  kStrucPars: TStrucPars =
    ( // default values
    G1: alpha1 / beta1;
    G3: alpha3 / beta3;
    GA: 2.25 * mPR_F;
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
  gScalingFactor: real;

implementation

initialization
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

end.

{ References:

Breuninger LM, Dempsey WL, Uhl J, Murasko DM. Hydrocortisone regulation of
interleukin-6 protein production by a purified population of human peripheral
blood monocytes. Clin Immunol Immunopathol. 1993 Nov;69(2):205-14.
doi: 10.1006/clin.1993.1171. PMID: 8403558.


DiBartolomeis MJ, Williams C, Jefcoate CR. Inhibition of ACTH action on
cultured bovine adrenal cortical cells by 2,3,7,8-tetrachlorodibenzo-p-dioxin
through a redistribution of cholesterol. J Biol Chem.
1986 Apr 5;261(10):4432-7. PMID: 3007456.

Kraan GP, Dullaart RP, Pratt JJ, Wolthers BG, Drayer NM, De Bruin R.
The daily cortisol production reinvestigated in healthy men. The serum and
urinary cortisol production rates are not significantly different.
J Clin Endocrinol Metab. 1998 Apr;83(4):1247-52. doi: 10.1210/jcem.83.4.4694.
PMID: 9543150.

Dietrich JW, Boehm BO. Equilibrium behaviour of feedback-coupled physiological
saturation kinetics. In Trappl R (Editor): Cybernetics and Systems 2006.
Austrian Society for Cybernetic Studies. doi: 10.13140/2.1.2400.2568.

Dietrich, JW, Boehm, BO. Die MiMe-NoCoDI-Plattform: Ein Ansatz für  die
Modellierung biologischer Regelkreise. German Med. Sci. Abstr. 284.
doi: 10.3205/15gmd s058 (2015).

Dietrich JW. A Methodology for Vertical Translation Between Molecular and
Organismal Level in Biological Feedback Loops. bioRxiv 2021.09.20.461028;
doi: 10.1101/2021.09.20.461028.

Dietrich JW, Siegmar N, Hojjati JR, Gardt O, Boehm BO. CyberUnits Bricks – An
Implementation Study of a Class Library for Simulating Nonlinear Biological
Feedback Loops. Advances in Distributed Computing and Artificial Intelligence
Journal. 2024 August; 13. doi: 10.14201/adcaij.31762.

}

