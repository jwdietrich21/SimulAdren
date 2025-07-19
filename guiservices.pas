unit GUIServices;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ General GUI-related services for use by several units }

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
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, Grids, SimuladrenTypes, clipbrd
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  ;

function DarkTheme: boolean;
procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean = False);

implementation

{$IFDEF LCLCocoa}
{The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html}

// Retrieve key's string value from user preferences. Result is encoded using NSStrToStr's default encoding.
function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;
{$ENDIF}

function IsMinMacOS(Maj, Min, Patch: integer): boolean;
  { returns true, if this app runs on a macOS version as specified or newer }
  {$IFDEF DARWIN}
var
  Major, Minor: SInt32;
  theError: SInt16;
  {$IFDEF LCLCocoa}
  minOsVer: NSOperatingSystemVersion;
  {$ENDIF}
  {$ENDIF}
begin
  result := false;
  {$IFDEF DARWIN}
  {$IFDEF LCLCocoa}
  minOsVer.majorVersion:= Maj;
  minOsVer.minorVersion:= Maj;
  minOsVer.patchVersion:= Patch;
  {$ENDIF}
  theError := Gestalt(gestaltSystemVersionMajor, Major);
  if theError = 0 then
    begin
      theError := Gestalt(gestaltSystemVersionMinor, Minor);
      if theError = 0 then
        if (Major = Maj) and (Minor >= Min) or (Major > Maj) then
          Result := True;
    end
  else
  begin
    {$IFDEF LCLCocoa}
    if(NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer)) then
      Result := True
    else   {$ENDIF}
      Result := False
  end;
  {$ENDIF}
end;

function MojaveOrNewer: boolean;
  { returns true, if this app runs on macOS X 10.14 Mojave or newer }
begin
  result := false;
  {$IFDEF DARWIN}
  result := IsMinMacOS(10, 14, 0);
  {$ENDIF}
end;

// DarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function DarkTheme: boolean;
{$IFDEF Windows}
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
  WindowsDarkModeSupported: boolean = false; // may be set to true in future versions
var
  LightKey: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := false;
  {$IFDEF Windows}
  if WindowsDarkModeSupported then
  begin
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKeyReadOnly(KEYPATH) then
        begin
          if Registry.ValueExists(KEYNAME) then
            LightKey := Registry.ReadBool(KEYNAME)
          else
            LightKey := true;
        end
      else
        LightKey := true;
        Result := not LightKey
    finally
      Registry.Free;
    end;
  end
  else
  Result := false;
  {$ELSE}
  {$IFDEF LCLCocoa}
  if MojaveOrNewer then
    Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0
  else
    Result := false;
  {$ELSE}
  Result := false;
  {$ENDIF}
  {$ENDIF}
end;

procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean);
{supports cutting or copying cells from a grid}
var
  theSelection: TGridRect;
  r, c: integer;
  textfromSelection: ansistring;
begin
  theSelection      := theGrid.Selection;
  textfromSelection := '';
  for r := theSelection.Top to theSelection.Bottom do
  begin
    for c := theSelection.Left to theSelection.Right do
    begin
      textfromSelection := textfromSelection + theGrid.Cells[c, r];
      if cut then
      begin
        theGrid.Cells[c, r] := '';
      end;
      if c < theSelection.Right then
        textfromSelection := textfromSelection + kTAB;
    end;
    if r < theSelection.Bottom then
      textfromSelection := textfromSelection + kCRLF;
  end;
  ClipBoard.AsText := textfromSelection;
end;

end.

