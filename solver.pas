unit Solver;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Solver for algebraic equations defined by parameter sets }

{ Version 2.1.0 (Foudre) }

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
  Classes, SysUtils, Math, uComplex;

type
  TLRoot = extended;
  TQRoots = array[0..1] of extended;
  TCRoots = array[0..2] of extended;
  TRRoots = array[0..3] of extended;

function Solve(a, b: extended): TLRoot;
function Solve(a, b, c: extended): TQRoots;
function Solve(a, b, c, d: extended): TCRoots;
function Solve(a, b, c, d, e: extended): TRRoots;
function arc(chi: extended): extended;
function arccos(cosphi: extended): extended;
function cbrt(x: extended): extended;
function cub(x: extended): extended;
function quart(x: extended): extended;
procedure SortTwo(var x, y: extended);
procedure SortThree(var x, y, z: extended);
procedure ShellSort(var a: array of extended);

implementation

function arc(chi: extended): extended;
  {converts an angle from degree to radian}
  {rechnet Winkel von Grad nach Bogenmaß um}
begin
  Result := 2 * pi * (chi / 360);
end;

function arccos(cosphi: extended): extended;
  {calculates the arcus cosine of a number between -1 and 1}
  {errechnet den Arcus-Cosinus einer Zahl zwischen -1 und 1}
var
  arcsin: extended;
begin
  arcsin := arctan(cosphi / sqrt(1 - sqr(cosphi)));
  Result := arc(90) - arcsin;
end;

function cbrt(x: extended): extended;
  {calculates cubic root of x}
  {berechnet Kubikwurzel von x}
begin
  Result := sign(x) * power(abs(x), 1 / 3);
end;

function cub(x: extended): extended;
  {calculates cube of x}
  {berechnet Kubus von x}
begin
  Result := x * x * x;
end;

function quart(x: extended): extended;
  {calculates quartic of x}
  {berechnet Biquadrat von x}
begin
  Result := x * x * x * x;
end;

procedure SortTwo(var x, y: extended);
  {sorts two numbers}
var
  t: extended;
begin
  if y < x then
  begin
    t := x;
    x := y;
    y := t;
  end;
end;

procedure SortThree(var x, y, z: extended);
  {sorts three numbers}
begin
  SortTwo(x, y);
  SortTwo(x, z);
  SortTwo(y, z);
end;

procedure ShellSort(var a: array of extended);
  {sorts a vector of arbitrary length}
var
  i, j, h, n: integer;
  v: extended;
begin
  n := length(a);
  h := 1;
  repeat
    h := 3 * h + 1
  until h > n;
  repeat
    h := h div 3;
    for i := h to n - 1 do
    begin
      v := a[i];
      j := i;
      while (j >= h) and (a[j - h] > v) do
      begin
        a[j] := a[j - h];
        j := j - h;
      end;
      a[j] := v;
    end
  until h = 1;
end;

function Solve(a, b: extended): TLRoot;
  {solves linear equation ax + b = 0 with parameters a and b}
  {löst lineare Gleichung ax + b = 0 mit Parametern a und b}
begin
  if a = 0 then
    Result := Math.NaN // horizontal line
  else
    Result := -b / a;
end;

function Solve(a, b, c: extended): TQRoots;
  {solves quadratic equation ax^2 + bx + c = 0 with parameters a, b and c}
  {löst quadratische Gleichung ax^2 + bx + c = 0 mit Parametern a, b, und c}
var
  Det1, Det2: extended;
begin
  Result[0] := Math.NaN;
  Result[1] := Math.NaN;
  if a <> 0 then                        // solution possible?
  begin
    Det1 := -c / a;
    Det2 := sqr(b) - 4 * a * c;
    if (Det1 >= 0) and (Det2 >= 0) then  // solution possible?
    begin
      Result[0] := -(b + sqrt(sqr(b) - 4 * a * c)) / (2 * a);
      Result[1] := -(b - sqrt(sqr(b) - 4 * a * c)) / (2 * a);
    end;
  end;
  SortTwo(Result[0], Result[1]);
end;

function Solve(a, b, c, d: extended): TCRoots;
  {solves cubic equation ax^3 + bx^2 + cx + d = 0 with parameters a, b, c, and d}
  {löst kubische Gleichung ax^3 + bx^2 + cx + d = 0 mit Parametern a, b, c und d}
var
  p, q, u, v, Det, phi, y1, y2, y3: extended;
begin
  Result[0] := Math.NaN;
  Result[1] := Math.NaN;
  Result[2] := Math.NaN;

  if a <> 0 then                        // finite solution possible?
  begin
    {
    These are the original equations stated in the literature:

    r := c / a - 1 / 3 * sqr(b / a);
    s := 2 / 27 * power((b / a), 3) - 1 / 3 * c * b / sqr(a) + d / a;
    p := r / 3;
    q := s / 2;

    However, the following implementation generates a higher precision:
    }

    p := c / (3 * a) - sqr(b) / (9 * sqr(a));
    q := cub(b) / (27 * cub(a)) - c * b / (6 * sqr(a)) + d / (2 * a);

    Det := cub(p) + sqr(q);

    if Det >= 0 then
    begin {Cardano's formula, one real solution}
      u := cbrt(-q + sqrt(Det));
      v := cbrt(-q - sqrt(Det));
      y1 := u + v;        {real solution of Cardano's equation}
      y2 := -(u + v) / 2; {Real part of the first complex solution}
      y3 := y2;           {Real part of the second complex solution (=y2)}
    end
    else
    begin {Casus irreducibilis, three real solutions}
      u := -q / (sqrt(-cub(p)));   {cos phi}
      phi := arccos(u);            {angle as radian}
      y1 := 2 * sqrt(-p) * cos(phi / 3);
      y2 := -2 * sqrt(-p) * cos(phi / 3 + arc(60));
      y3 := -2 * sqrt(-p) * cos(phi / 3 - arc(60));
    end;

    Result[0] := y1 - b / (3 * a);
    Result[1] := y2 - b / (3 * a);
    Result[2] := y3 - b / (3 * a);

  end;
  SortThree(Result[0], Result[1], Result[2]);
end;

function Solve(a, b, c, d, e: extended): TRRoots;
  {solves a quartic equation ax^4 + bx^3 + cx^2 + dx + e = 0}
  {löst quartische Gleichung ax^4 + bx^3 + cx^2 + dx + e = 0}
var
  t0, t1, t2, t3, radicand, rA, rB, w: extended;
  ResolventRoots: TCRoots;
begin
  Result[0] := Math.NaN;
  Result[1] := Math.NaN;
  Result[2] := Math.NaN;
  Result[3] := Math.NaN;

  if a <> 0 then                        // finite solution possible?
    begin
    t0 := -a * sqr(d) + sqr(b) * e;
    t1 := sqr(b) * d - 4 * a * c * d + 8 * a * b * e;
    t2 := sqr(b) * c - 4 * a * sqr(c) + 2 * a * b * d + 16 * sqr(a) * e;
    t3 := cub(b) - 4 * a * b * c + 8 * sqr(a) * d;
    if (t3 = 0) and (t2 = 0) then
    begin
      Result[0] := (-b + sqrt(3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[1] := (-b + sqrt(3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[2] := (-b - sqrt(3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[3] := (-b - sqrt(3 * sqr(b) - 8 * a * c)) / (4 * a);
    end
    else if (t3 = 0) and (t2 <> 0) then
    begin
      Result[0] := -(b + sqrt(2 * sqrt(quart(b) - 8 * sqr(b) * a * c +
        16 * sqr(a) * sqr(c) - 64 * cub(a) * e) + 3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[1] := -(b + sqrt(-2 * sqrt(quart(b) - 8 * sqr(b) * a * c +
        16 * sqr(a) * sqr(c) - 64 * cub(a) * e) + 3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[2] := -(b - sqrt(2 * sqrt(quart(b) - 8 * sqr(b) * a * c +
        16 * sqr(a) * sqr(c) - 64 * cub(a) * e) + 3 * sqr(b) - 8 * a * c)) / (4 * a);
      Result[3] := -(b - sqrt(-2 * sqrt(quart(b) - 8 * sqr(b) * a * c +
        16 * sqr(a) * sqr(c) - 64 * cub(a) * e) + 3 * sqr(b) - 8 * a * c)) / (4 * a);
    end
    else
    begin
      ResolventRoots := Solve(t3, t2, t1, t0);
      w := ResolventRoots[0];
      radicand := (cub(b) + 8 * sqr(a) * d - 4 * a * b * c) / (b + 4 * a * w);
      rA := sqrt(radicand);
      rB := (cub(b) - 4 * sqr(a) * d - 2 * a * b * c + 6 * a * sqr(b) * w - 16 * sqr(a) * c * w) / (b + 4 * a * w);
      Result[0] := (-b - rA - sqrt(2) * sqrt(rB + rA * (b + 4 * a * w))) / (4 * a);
      Result[1] := (-b - rA + sqrt(2) * sqrt(rB + rA * (b + 4 * a * w))) / (4 * a);
      Result[2] := (-b + rA - sqrt(2) * sqrt(rB - rA * (b + 4 * a * w))) / (4 * a);
      Result[3] := (-b + rA + sqrt(2) * sqrt(rB - rA * (b + 4 * a * w))) / (4 * a);
    end;
  end;

  ShellSort(Result);
end;

end.

{ References: }

{ 1. Yacoub, M.D., & Fraidenraich, G. (2012). 96.33 A solution to the quartic }
{    equation. The Mathematical Gazette, 96, 271 - 275. }


