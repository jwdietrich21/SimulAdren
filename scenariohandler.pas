unit ScenarioHandler;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ This unit reads and writes scenarios as XML files }

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
  Classes, SysUtils, DateUtils, DOM, XMLRead, XMLWrite,
  URIParser, SimuladrenTypes, SimuladrenResources, BaseServices;

var
  gActiveModel: tActiveModel;

function NewScenario: TActiveModel;
function emptyModel: TActiveModel;
procedure ReadScenario(theFileName: string; var modelVersion: Str13);
procedure SaveScenario(theFileName: string);

implementation

function ValidFormat(theStream: TStream; const theBaseURI: ansistring): boolean;
const
  SIGNATURE_1 = '<?xml version="1.';
  SIGNATURE_2 = '<scenario';
  SIGNATURE_3 = '</scenario>';
var
  origString, lowerString: ansistring;
begin
  Result := False;
  if theStream.Size > 0 then
  begin
    SetLength(origString, theStream.Size);
    theStream.Read(origString[1], theStream.Size);
    if origString <> '' then
    begin
      lowerString := LowerCase(origString);
      if LeftStr(lowerString, 17) = SIGNATURE_1 then
        if pos(SIGNATURE_2, lowerString) <> 0 then
          if pos(SIGNATURE_3, lowerString) <> 0 then
            Result := True;
    end;
  end;
end;

function ValidFormat(theFileName: string): boolean;
var
  theStream: TStream;
begin
  theStream := TFileStream.Create(theFileName, fmOpenRead + fmShareDenyWrite);
  try
    Result := ValidFormat(theStream, FilenameToURI(theFileName));
  finally
    if theStream <> nil then
      theStream.Free;
  end;
end;

function NewScenario: TActiveModel;
begin
  Result := emptyModel;
end;

function emptyModel: TActiveModel;
begin
  Result.Name := kSTANDARD_MODEL_NAME;
  Result.Reference := kSTANDARD_MODEL_REFERENCE;
  Result.Species := kSTANDARD_MODEL_SPECIES;
  Result.Creators := kSTANDARD_MODEL_CREATORS;
  Result.Created := EncodeDateTime(kSTANDARD_MODEL_CREATED_Y,
    kSTANDARD_MODEL_CREATED_M, kSTANDARD_MODEL_CREATED_D,
    kSTANDARD_MODEL_CREATED_H, kSTANDARD_MODEL_CREATED_N, kSTANDARD_MODEL_CREATED_S, 0);
  Result.LastModified := EncodeDateTime(kSTANDARD_MODEL_MODIFIED_Y,
    kSTANDARD_MODEL_MODIFIED_M, kSTANDARD_MODEL_MODIFIED_D,
    kSTANDARD_MODEL_MODIFIED_H, kSTANDARD_MODEL_MODIFIED_N,
    kSTANDARD_MODEL_MODIFIED_S, 0);
  Result.Terms := kSTANDARD_MODEL_TERMS;
  Result.Iterations := 0;
  Result.Imported := false;
end;

procedure ReadScenario(theFileName: string; var modelVersion: Str13);
begin

end;

procedure SaveScenario(theFileName: string);
var
  i, k: integer;
  oldSep: char;
  Doc: TXMLDocument;
  RootNode, ElementNode, SeqNode: TDOMNode;
  theDate, StringToWrite: ansistring;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('scenario');
    TDOMElement(RootNode).SetAttribute('modelversion', '3.0');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    ElementNode := Doc.CreateElement('MIRIAM');
    ElementNode.AppendChild(SimpleNode(Doc, 'Name', gActiveModel.Name));
    ElementNode.AppendChild(SimpleNode(Doc, 'Reference', gActiveModel.Reference));
    ElementNode.AppendChild(SimpleNode(Doc, 'Species', gActiveModel.Species));
    ElementNode.AppendChild(SimpleNode(Doc, 'Creators', gActiveModel.Creators));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.Created);
    ElementNode.AppendChild(SimpleNode(Doc, 'Created', theDate));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.LastModified);
    ElementNode.AppendChild(SimpleNode(Doc, 'LastModified', theDate));
    ElementNode.AppendChild(SimpleNode(Doc, 'Terms', gActiveModel.Terms));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('MIASE');
    if gActiveModel.Code = '' then
      gActiveModel.Code := MIASE_SIMULADREN_STANDARD_CODE;
    ElementNode.AppendChild(SimpleNode(Doc, 'Code', gActiveModel.Code));
    ElementNode.AppendChild(SimpleNode(Doc, 'Comments', gActiveModel.Comments));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('basic');
    ElementNode.AppendChild(SimpleNode(Doc, 'iterations', IntToStr(gActiveModel.Iterations)));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('strucpars');
    ElementNode.AppendChild(SimpleNode(Doc, 'G1',
      FloatToStr(gActiveModel.StrucPars.G1, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'G3',
      FloatToStr(gActiveModel.StrucPars.G3, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GA',
      FloatToStr(gActiveModel.StrucPars.GA, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DA',
      FloatToStr(gActiveModel.StrucPars.DA, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GR',
      FloatToStr(gActiveModel.StrucPars.GR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DR',
      FloatToStr(gActiveModel.StrucPars.DR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GE',
      FloatToStr(gActiveModel.StrucPars.GE, gUSFormatSettings)));
    RootNode.AppendChild(ElementNode);

    WriteXMLFile(Doc, theFileName);
  finally
    Doc.Free;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

end.

