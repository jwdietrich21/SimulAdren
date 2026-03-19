unit ScenarioHandler;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ This unit reads and writes scenarios as XML files }

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
  Classes, SysUtils, DateUtils, DOM, XMLRead, XMLWrite,
  URIParser, SimuladrenTypes, SimuladrenResources, BaseServices, GUIServices;

function NewScenario: TActiveModel;
function emptyModel: TActiveModel;
procedure ReadScenario(theFileName: string; var theModel: tActiveModel);
procedure SaveScenario(theModel: tActiveModel; theFileName: string);

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
  Result.Imported := False;
  Result.Version := '';
end;

procedure ReadScenario(theFileName: string; var theModel: tActiveModel);
{reads a simulation scenario}
var
  i: integer;
  Doc: TXMLDocument;
  RootNode, basicNode: TDOMNode;
  oldSep: char;
  standardDate: TDateTime;
begin
  if FileExists(theFileName) then
    if ValidFormat(theFileName) then
    begin
      oldSep := DefaultFormatSettings.DecimalSeparator;
      DefaultFormatSettings.DecimalSeparator := kPERIOD;
      try
        standardDate := EncodeDateTime(1904, 01, 01, 00, 00, 00, 00);
        ReadXMLFile(Doc, theFileName);
        if assigned(Doc) then
          RootNode := Doc.DocumentElement;
        if assigned(RootNode) and RootNode.HasAttributes and
          (RootNode.Attributes.Length > 0) then
          for i := 0 to RootNode.Attributes.Length - 1 do
            with RootNode.Attributes[i] do
            begin
              if NodeName = 'modelversion' then
              begin
                theModel.Version := UTF8Encode(NodeValue);
                if theModel.Version = '1.0' then
                  theModel.Version := '1';
              end;
            end;
        RootNode := Doc.DocumentElement.FindNode('MIRIAM');
        if assigned(RootNode) then
        begin
          theModel.Name := NodeContent(RootNode, 'Name');
          theModel.Reference := NodeContent(RootNode, 'Reference');
          theModel.Species := NodeContent(RootNode, 'Species');
          theModel.Creators := NodeContent(RootNode, 'Creators');
          if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'Created'),
            theModel.Created) then
            theModel.Created := standardDate;
          if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'LastModified'),
            theModel.LastModified) then
            theModel.LastModified := standardDate;
          theModel.Terms := NodeContent(RootNode, 'Terms');
        end;
        RootNode := Doc.DocumentElement.FindNode('MIASE');
        if assigned(RootNode) then
        begin
          theModel.Code := NodeContent(RootNode, 'Code');
          theModel.Comments := NodeContent(RootNode, 'Comments');
        end;
        if theModel.Code = '' then
          theModel.Code := MIASE_SIMULADREN_STANDARD_CODE;
        if (theModel.Version = '') or (theModel.Version = '1') or
          (LeftStr(theModel.Version, 2) = '1.') then
        begin
          RootNode := Doc.DocumentElement.FindNode('strucpars');
          if assigned(RootNode) then
          begin
            VarFromNode(RootNode, 'G1', theModel.StrucPars.G1);
            VarFromNode(RootNode, 'G3', theModel.StrucPars.G3);
            VarFromNode(RootNode, 'GA', theModel.StrucPars.GA);
            VarFromNode(RootNode, 'DA', theModel.StrucPars.DA);
            VarFromNode(RootNode, 'GR', theModel.StrucPars.GR);
            VarFromNode(RootNode, 'DR', theModel.StrucPars.DR);
            VarFromNode(RootNode, 'GE', theModel.StrucPars.GE);
            VarFromNode(RootNode, 'alpha1', theModel.StrucPars.alpha1);
            VarFromNode(RootNode, 'beta1', theModel.StrucPars.beta1);
            VarFromNode(RootNode, 'alpha3', theModel.StrucPars.alpha3);
            VarFromNode(RootNode, 'beta3', theModel.StrucPars.beta3);
          end;
        end
        else
          ShowVersionError;
        basicNode := Doc.DocumentElement.FindNode('basic');
        if assigned(basicNode) then
        begin
          VarFromNode(basicNode, 'iterations', theModel.Iterations);
        end;
      finally
        if assigned(Doc) then
          Doc.Free;
      end;
      {$IFDEF GUI}
      if AnnotationForm.Visible then
        AnnotationForm.ShowAnnotation(theModel);
      {$ENDIF}
      theModel.Imported := True;
      DefaultFormatSettings.DecimalSeparator := oldSep;
    end
    else
      ShowFileError;
end;


procedure SaveScenario(theModel: tActiveModel; theFileName: string);
var
  oldSep: char;
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  theDate: ansistring;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  theModel.LastModified := Now;
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('scenario');
    TDOMElement(RootNode).SetAttribute('modelversion', DOMString(theModel.Version));
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    ElementNode := Doc.CreateElement('MIRIAM');
    ElementNode.AppendChild(SimpleNode(Doc, 'Name', theModel.Name));
    ElementNode.AppendChild(SimpleNode(Doc, 'Reference', theModel.Reference));
    ElementNode.AppendChild(SimpleNode(Doc, 'Species', theModel.Species));
    ElementNode.AppendChild(SimpleNode(Doc, 'Creators', theModel.Creators));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, theModel.Created);
    ElementNode.AppendChild(SimpleNode(Doc, 'Created', theDate));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, theModel.LastModified);
    ElementNode.AppendChild(SimpleNode(Doc, 'LastModified', theDate));
    ElementNode.AppendChild(SimpleNode(Doc, 'Terms', theModel.Terms));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('MIASE');
    if theModel.Code = '' then
      theModel.Code := MIASE_SIMULADREN_STANDARD_CODE;
    ElementNode.AppendChild(SimpleNode(Doc, 'Code', theModel.Code));
    ElementNode.AppendChild(SimpleNode(Doc, 'Comments', theModel.Comments));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('basic');
    ElementNode.AppendChild(SimpleNode(Doc, 'iterations',
      IntToStr(theModel.Iterations)));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('strucpars');
    ElementNode.AppendChild(SimpleNode(Doc, 'G1',
      FloatToStr(theModel.StrucPars.G1, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'G3',
      FloatToStr(theModel.StrucPars.G3, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GA',
      FloatToStr(theModel.StrucPars.GA, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DA',
      FloatToStr(theModel.StrucPars.DA, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GR',
      FloatToStr(theModel.StrucPars.GR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DR',
      FloatToStr(theModel.StrucPars.DR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GE',
      FloatToStr(theModel.StrucPars.GE, gUSFormatSettings)));
    if (theModel.Version <> '1') and (theModel.Version <> '1.1') then
    begin
      ElementNode.AppendChild(SimpleNode(Doc, 'alpha1',
        FloatToStr(theModel.StrucPars.alpha1, gUSFormatSettings)));
      ElementNode.AppendChild(SimpleNode(Doc, 'beta1',
        FloatToStr(theModel.StrucPars.beta1, gUSFormatSettings)));
      ElementNode.AppendChild(SimpleNode(Doc, 'alpha3',
        FloatToStr(theModel.StrucPars.alpha3, gUSFormatSettings)));
      ElementNode.AppendChild(SimpleNode(Doc, 'beta3',
        FloatToStr(theModel.StrucPars.beta3, gUSFormatSettings)));
    end;
    RootNode.AppendChild(ElementNode);

    WriteXMLFile(Doc, theFileName);
  finally
    Doc.Free;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

end.
