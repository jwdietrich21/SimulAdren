unit IPS;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Information Processing Structure }

{ Version 1.0.0 (Rudolphina) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Standard blocks for systems modelling and simulation }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ cACTHrrent versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTIcACTHLAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lclintf, SystemsDiagram, SimuladrenTypes, GUIServices;

type

  TElements = record
    Controller, DAInjection, VTypeAllostery, MMRatio2: TIPSClass;
    DRInjection, MMRatio4: TIPSClass;
    G1, GA, G3, GR, GE, xt, DA, DR, x1, j1, j2: TIPSClass;
    cx, ce, cy, cACTH, cyr, cDA, c1, cN, c2a, c2b, c2c: TConnectionClass;
    cCortisol, cv, cD4, c4a, c4b, c4c: TConnectionClass;
  end;

  { TIPSForm }

  TIPSForm = class(TForm)
    IPSImage: TImage;
    procedure DrawIPS(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    BlockDiagram: TBlockDiagram;
    IPSBitmap: TBitmap;
    Elements: TElements;
  end;


var
  IPSForm: TIPSForm;

implementation

{$R *.lfm}

{ TIPSForm }

procedure TIPSForm.DrawIPS(Sender: TObject);
var
  DrawColour: TColor;
begin
  BlockDiagram := TBlockDiagram.Create;
  IPSBitmap := TBitmap.Create;
  try
    IPSBitmap.Height := IPSImage.Height;
    IPSBitmap.Width := IPSImage.Width;
    if DarkTheme then
    begin
      IPSBitmap.Canvas.Brush.Color := self.Color;
      IPSBitmap.Canvas.Pen.Color := self.Color;
      DrawColour := clSilver;
    end
    else
    begin
      IPSBitmap.Canvas.Brush.Color := clWhite;
      IPSBitmap.Canvas.Pen.Color := clWhite;
      DrawColour := clBlack;
    end;
    IPSBitmap.Canvas.Rectangle(0, 0, IPSBitmap.Width, IPSBitmap.Height);
    IPSBitmap.Canvas.Pen.Color := DrawColour;
    BlockDiagram.canvas := IPSBitmap.Canvas;

    IPSBitmap.Canvas.Pen.Color := clDarkOrange;
    Elements.xt := TTerminalClass.Create;
    Elements.xt.blockDiagram := BlockDiagram;
    BlockDiagram.firstIPSObject := Elements.xt;
    SetRect(Elements.xt.boundsRect, 21, 10, 41, 30);
    Elements.xt.title := 'CRH(t)';
    TTerminalClass(Elements.xt).TextMargin := 5;
    TTerminalClass(Elements.xt).TextPosition := rightmiddle;
    Elements.xt.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.Controller := TPiClass.Create;
    Elements.Controller.blockDiagram := BlockDiagram;
    SetRect(Elements.Controller.boundsRect, 10, 50, 50, 90);
    TInvertableClass(Elements.Controller).invertedSegments := [bottomSegment];
    Elements.Controller.Draw;

    Elements.G1 := TPClass.Create;
    Elements.G1.blockDiagram := BlockDiagram;
    SetRect(Elements.G1.boundsRect, 100, 52, 160, 88);
    Elements.G1.title := 'G1';
    Elements.G1.font.Style := [fsItalic];
    Elements.G1.Draw;

    Elements.GA := TPClass.Create;
    Elements.GA.blockDiagram := BlockDiagram;
    SetRect(Elements.GA.boundsRect, 240, 52, 300, 88);
    Elements.GA.title := 'GA';
    Elements.GA.font.Style := [fsItalic];
    Elements.GA.Draw;

    Elements.DAInjection := TSigmaClass.Create;
    Elements.DAInjection.blockDiagram := BlockDiagram;
    SetRect(Elements.DAInjection.boundsRect, 250, 100, 290, 140);
    Elements.DAInjection.Draw;

    Elements.G3 := TPClass.Create;
    Elements.G3.blockDiagram := BlockDiagram;
    SetRect(Elements.G3.boundsRect, 420, 110, 480, 150);
    Elements.G3.title := 'G3';
    Elements.G3.font.Style := [fsItalic];
    Elements.G3.Draw;

    Elements.MMRatio2 := TPiClass.Create;
    Elements.MMRatio2.blockDiagram := BlockDiagram;
    SetRect(Elements.MMRatio2.boundsRect, 340, 52, 380, 88);
    TInvertableClass(Elements.MMRatio2).invertedSegments := [bottomSegment];
    Elements.MMRatio2.Draw;

    IPSBitmap.Canvas.Pen.Color := clGoldenRod;
    Elements.j1 := TJunctionClass.Create;
    Elements.j1.blockDiagram := BlockDiagram;
    SetRect(Elements.j1.boundsRect, 190, 53, 210, 89);
    Elements.j1.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.DA := TTerminalClass.Create;
    Elements.DA.blockDiagram := BlockDiagram;
    SetRect(Elements.DA.boundsRect, 251, 160, 291, 180);
    Elements.DA.title := 'DA';
    TTerminalClass(Elements.DA).TextMargin := 5;
    TTerminalClass(Elements.DA).TextPosition := leftmiddle;
    Elements.DA.Draw;

    Elements.x1 := TTerminalClass.Create;
    Elements.x1.blockDiagram := BlockDiagram;
    SetRect(Elements.x1.boundsRect, 21, 300, 41, 350);
    Elements.x1.title := '1';
    TTerminalClass(Elements.x1).TextMargin := 5;
    TTerminalClass(Elements.x1).TextPosition := rightmiddle;
    Elements.x1.Draw;

    IPSBitmap.Canvas.Pen.Color := clPurple;
    Elements.j2 := TJunctionClass.Create;
    Elements.j2.blockDiagram := BlockDiagram;
    SetRect(Elements.j2.boundsRect, 390, 261, 400, 301);
    Elements.j2.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.GR := TPClass.Create;
    Elements.GR.blockDiagram := BlockDiagram;
    SetRect(Elements.GR.boundsRect, 280, 260, 340, 300);
    Elements.GR.title := 'GR';
    Elements.GR.font.Style := [fsItalic];
    Elements.GR.Draw;

    Elements.DR := TTerminalClass.Create;
    Elements.DR.blockDiagram := BlockDiagram;
    SetRect(Elements.DR.boundsRect, 281, 177, 341, 197);
    Elements.DR.title := 'DR';
    TTerminalClass(Elements.DR).TextMargin := 5;
    TTerminalClass(Elements.DR).TextPosition := leftmiddle;
    Elements.DR.Draw;

    Elements.DRInjection := TSigmaClass.Create;
    Elements.DRInjection.blockDiagram := BlockDiagram;
    SetRect(Elements.DRInjection.boundsRect, 280, 208, 340, 248);
    Elements.DRInjection.Draw;

    Elements.MMRatio4 := TPiClass.Create;
    Elements.MMRatio4.blockDiagram := BlockDiagram;
    SetRect(Elements.MMRatio4.boundsRect, 200, 260, 240, 300);
    TInvertableClass(Elements.MMRatio4).invertedSegments := [topSegment];
    Elements.MMRatio4.Draw;

    Elements.GE := TPClass.Create;
    Elements.GE.blockDiagram := BlockDiagram;
    SetRect(Elements.GE.boundsRect, 100, 260, 160, 300);
    Elements.GE.title := 'GE';
    Elements.GE.font.Style := [fsItalic];
    Elements.GE.Draw;

    Elements.VTypeAllostery := TSigmaClass.Create;
    Elements.VTypeAllostery.blockDiagram := BlockDiagram;
    SetRect(Elements.VTypeAllostery.boundsRect, 10, 260, 50, 300);
    Elements.VTypeAllostery.Draw;

    IPSBitmap.Canvas.Pen.Color := clDarkOrange;
    Elements.cx := TConnectionClass.Create;
    Elements.cx.blockDiagram := BlockDiagram;
    Elements.cx.sourceObject := Elements.xt;
    Elements.cx.sourceAnchor := bottommiddle;
    Elements.cx.drainObject := Elements.Controller;
    Elements.cx.drainAnchor := topmiddle;
    Elements.cx.Draw;

    Elements.ce := TConnectionClass.Create;
    Elements.ce.blockDiagram := BlockDiagram;
    Elements.ce.sourceObject := Elements.Controller;
    Elements.ce.sourceAnchor := rightmiddle;
    Elements.ce.drainObject := Elements.G1;
    Elements.ce.drainAnchor := leftmiddle;
    Elements.ce.title := 'e(t)';
    Elements.ce.font.Style := [fsItalic];
    TConnectionClass(Elements.ce).TextMargin := 7;
    TConnectionClass(Elements.ce).TextPosition := topmiddle;
    Elements.ce.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    IPSBitmap.Canvas.Pen.Color := clGoldenRod;
    Elements.cACTH := TConnectionClass.Create;
    Elements.cACTH.blockDiagram := BlockDiagram;
    Elements.cACTH.sourceObject := Elements.G1;
    Elements.cACTH.sourceAnchor := rightmiddle;
    Elements.cACTH.drainObject := Elements.GA;
    Elements.cACTH.drainAnchor := leftmiddle;
    Elements.cACTH.title := 'ACTH(t)';
    Elements.cACTH.font.Style := [fsItalic];
    TConnectionClass(Elements.cACTH).TextMargin := 7;
    TConnectionClass(Elements.cACTH).TextPosition := topmiddle;
    Elements.cACTH.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.cyr := TConnectionClass.Create;
    Elements.cyr.blockDiagram := BlockDiagram;
    Elements.cyr.sourceObject := Elements.GE;
    Elements.cyr.sourceAnchor := leftmiddle;
    Elements.cyr.drainObject := Elements.VTypeAllostery;
    Elements.cyr.drainAnchor := rightmiddle;
    Elements.cyr.chirality := cright;
    Elements.cyr.title := 'yR(t)';
    Elements.cyr.font.Style := [fsItalic];
    TConnectionClass(Elements.cyr).TextMargin := 7;
    TConnectionClass(Elements.cyr).TextPosition := bottommiddle;
    Elements.cyr.Draw;

    Elements.cN := TConnectionClass.Create;
    Elements.cN.blockDiagram := BlockDiagram;
    Elements.cN.sourceObject := Elements.VTypeAllostery;
    Elements.cN.sourceAnchor := topmiddle;
    Elements.cN.drainObject := Elements.Controller;
    Elements.cN.drainAnchor := bottommiddle;
    Elements.cN.Draw;

    Elements.cV := TConnectionClass.Create;
    Elements.cV.blockDiagram := BlockDiagram;
    Elements.cV.sourceObject := Elements.MMRatio4;
    Elements.cV.sourceAnchor := leftmiddle;
    Elements.cV.drainObject := Elements.GE;
    Elements.cV.drainAnchor := rightmiddle;
    Elements.cV.title := 'v(t)';
    Elements.cV.font.Style := [fsItalic];
    TConnectionClass(Elements.cV).TextMargin := 7;
    TConnectionClass(Elements.cV).TextPosition := bottommiddle;
    Elements.cV.Draw;

    Elements.cDA := TConnectionClass.Create;
    Elements.cDA.blockDiagram := BlockDiagram;
    Elements.cDA.sourceObject := Elements.DA;
    Elements.cDA.sourceAnchor := topmiddle;
    Elements.cDA.drainObject := Elements.DAInjection;
    Elements.cDA.drainAnchor := bottommiddle;
    Elements.cDA.Draw;

    IPSBitmap.Canvas.Pen.Color := clPurple;
    Elements.cy := TConnectionClass.Create;
    Elements.cy.blockDiagram := BlockDiagram;
    Elements.cy.sourceObject := Elements.MMRatio2;
    Elements.cy.sourceAnchor := rightmiddle;
    Elements.cy.drainObject := Elements.G3;
    Elements.cy.drainAnchor := topmiddle;
    Elements.cy.chirality := cright;
    Elements.cy.title := 'PR(F, t)';
    Elements.cy.font.Style := [fsItalic];
    TConnectionClass(Elements.cy).TextMargin := 7;
    TConnectionClass(Elements.cy).TextPosition := topmiddle;
    Elements.cy.Draw;

    Elements.cCortisol := TConnectionClass.Create;
    Elements.cCortisol.blockDiagram := BlockDiagram;
    Elements.cCortisol.sourceObject := Elements.G3;
    Elements.cCortisol.sourceAnchor := bottommiddle;
    Elements.cCortisol.drainObject := Elements.GR;
    Elements.cCortisol.drainAnchor := rightmiddle;
    Elements.cCortisol.chirality := cright;
    Elements.cCortisol.title := 'F(t)';
    Elements.cCortisol.font.Style := [fsItalic];
    TConnectionClass(Elements.cCortisol).TextMargin := 7;
    TConnectionClass(Elements.cCortisol).TextPosition := bottommiddle;
    Elements.cCortisol.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.c1 := TConnectionClass.Create;
    Elements.c1.blockDiagram := BlockDiagram;
    Elements.c1.sourceObject := Elements.x1;
    Elements.c1.sourceAnchor := topmiddle;
    Elements.c1.drainObject := Elements.VTypeAllostery;
    Elements.c1.drainAnchor := bottommiddle;
    Elements.c1.Draw;

    IPSBitmap.Canvas.Pen.Color := clGoldenRod;
    Elements.c2a := TConnectionClass.Create;
    Elements.c2a.blockDiagram := BlockDiagram;
    Elements.c2a.sourceObject := Elements.j1;
    Elements.c2a.sourceAnchor := bottommiddle;
    Elements.c2a.drainObject := Elements.DAInjection;
    Elements.c2a.drainAnchor := leftmiddle;
    Elements.c2a.Draw;

    Elements.c2b := TConnectionClass.Create;
    Elements.c2b.blockDiagram := BlockDiagram;
    Elements.c2b.sourceObject := Elements.GA;
    Elements.c2b.sourceAnchor := rightmiddle;
    Elements.c2b.drainObject := Elements.MMRatio2;
    Elements.c2b.drainAnchor := leftmiddle;
    Elements.c2b.Draw;

    Elements.c2c := TConnectionClass.Create;
    Elements.c2c.blockDiagram := BlockDiagram;
    Elements.c2c.sourceObject := Elements.DAInjection;
    Elements.c2c.sourceAnchor := rightmiddle;
    Elements.c2c.drainObject := Elements.MMRatio2;
    Elements.c2c.drainAnchor := bottommiddle;
    Elements.c2c.chirality := cleft;
    Elements.c2c.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    Elements.cD4 := TConnectionClass.Create;
    Elements.cD4.blockDiagram := BlockDiagram;
    Elements.cD4.sourceObject := Elements.DR;
    Elements.cD4.sourceAnchor := bottommiddle;
    Elements.cD4.drainObject := Elements.DRInjection;
    Elements.cD4.drainAnchor := topmiddle;
    Elements.cD4.Draw;

    IPSBitmap.Canvas.Pen.Color := clPurple;
    Elements.c4a := TConnectionClass.Create;
    Elements.c4a.blockDiagram := BlockDiagram;
    Elements.c4a.sourceObject := Elements.j2;
    Elements.c4a.sourceAnchor := topmiddle;
    Elements.c4a.drainObject := Elements.DRInjection;
    Elements.c4a.drainAnchor := rightmiddle;
    Elements.c4a.Draw;

    Elements.c4b := TConnectionClass.Create;
    Elements.c4b.blockDiagram := BlockDiagram;
    Elements.c4b.sourceObject := Elements.GR;
    Elements.c4b.sourceAnchor := leftmiddle;
    Elements.c4b.drainObject := Elements.MMRatio4;
    Elements.c4b.drainAnchor := rightmiddle;
    Elements.c4b.Draw;

    Elements.c4c := TConnectionClass.Create;
    Elements.c4c.blockDiagram := BlockDiagram;
    Elements.c4c.sourceObject := Elements.DRInjection;
    Elements.c4c.sourceAnchor := leftmiddle;
    Elements.c4c.drainObject := Elements.MMRatio4;
    Elements.c4c.drainAnchor := topmiddle;
    Elements.c4c.chirality := cleft;
    Elements.c4c.Draw;
    IPSBitmap.Canvas.Pen.Color := DrawColour;

    IPSImage.Canvas.Draw(0, 0, IPSBitmap);
  finally
    IPSBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure TIPSForm.FormCreate(Sender: TObject);
begin
  DrawIPS(Sender);
end;

end.
