unit SimuladrenResources;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ This unit provides URLs and global strings for other SimulAdren units }

{ Version 2.0.0 (Rubycon) }

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
  Classes, SysUtils, SimuladrenTypes;

const

  BASE_URL = 'http://simuladren.sf.net';
  SIMULADREN_GLOBAL_ID = 'net.sf.simuladren';
  HELP_URL = 'http://simuladren.sf.net';

  SCICRUNCH_URL =
    'https://rrid.site/data/record/nlx_144509-1/SCR_027314/resolver?i=rrid:scr_027314';

  FILE_VERSION_MESSAGE = 'This scenario file has a file version that is not supported by this version of SimulAdren.';
  FILE_FORMAT_ERROR_MESSAGE = 'This is not a valid XML file that can be used by SimulAdren.';
  SAVE_ERROR_MESSAGE = 'Error saving the file';
  SVG_UNSUPPORTED_MESSAGE = 'SVG export of processing structurs is not supported in this version of SimulAdren';

  MIASE_URL = 'http://co.mbine.org/standards/miase';
  MIASE_SIMULADREN_STANDARD_CODE = 'Model of HPA feedback control for use with SimulAdren, as available from ' +  BASE_URL;
  MIRIAM_URL = 'http://www.ebi.ac.uk/miriam/main/';
  MIBBI_URL = 'http://biosharing.org/collection/MIBBI';

  kSTANDARD_MODEL_NAME = 'Model 1.1';
  kSTANDARD_MODEL_REFERENCE =
    'Dietrich JW, Boehm BO: Equilibrium behaviour of feedback-coupled physiological saturation kinetics. In: Cybernetics and Systems 2006.Volume 1, edn. Edited by Trappl R. Vienna: Austrian Society forCybernetic Studies; 2006: 269-274.';
  kSTANDARD_MODEL_SPECIES = 'Homo sapiens (NCBI Taxonomy ID 9606)';
  kSTANDARD_MODEL_CREATORS = 'Dietrich, Boehm and Siegmar';
  kSTANDARD_MODEL_CREATED_Y = 2025;
  kSTANDARD_MODEL_CREATED_M = 06;
  kSTANDARD_MODEL_CREATED_D = 28;
  kSTANDARD_MODEL_CREATED_H = 13;
  kSTANDARD_MODEL_CREATED_N = 00;
  kSTANDARD_MODEL_CREATED_S = 00;
  kSTANDARD_MODEL_MODIFIED_Y = 2026;
  kSTANDARD_MODEL_MODIFIED_M = 01;
  kSTANDARD_MODEL_MODIFIED_D = 18;
  kSTANDARD_MODEL_MODIFIED_H = 20;
  kSTANDARD_MODEL_MODIFIED_N = 28;
  kSTANDARD_MODEL_MODIFIED_S = 00;
  kSTANDARD_MODEL_TERMS = 'Creative Commons Attributions License 4.0 (CC BY 4.0)';

implementation

initialization

gFormatSettings := DefaultFormatSettings;
gUSFormatSettings := gFormatSettings;
gUSFormatSettings.DecimalSeparator := '.';
gUSFormatSettings.ThousandSeparator := ',';

end.
