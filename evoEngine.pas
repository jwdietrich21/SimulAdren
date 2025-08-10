unit evoEngine;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Functionality for evolutionary algorithms }

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
  Classes, SysUtils, Math, RandomFunctions, SimulAdrenTypes, SimulationEngine;

const
  LowerBound = -50;
  UpperBound = 50;
  PopulationSize = 100;
  Generations = 20;
  MutationRate = 1;
  TournamentSize = 3;

type
  TIndividual = record
    a, b, c: real;
    fitness: real;
  end;
  TPopulation = array of TIndividual;
  TParents = array[0..1] of TIndividual;
  TChildren = array[0..1] of TIndividual;
  TFittest = array of TIndividual;
  TAllPopulations = array of TPopulation;
  TAllele = array[0..1] of real;

var
  LowEdge, HighEdge: real;

function Fitness(const theGuess: TIndividual; const theTarget: real): real;
function InitialPopulation(const size: integer;
  const lowBound, highBound: real): TPopulation;
function Selection(const population: TPopulation;
  const TournamentSize: integer): TPopulation;
function Crossover(const parents: TParents): TChildren;
function Mutated(const Individual: TIndividual; const MutationRate: integer;
  const lowBound, highBound: real): TIndividual;
procedure GeneticAlgorithm(const size: integer; const CRH: extended;
  var params: TParams; const lowBound, highBound: real;
  const EvoTargets: TEvoTargets; const generations: integer;
  const mutationRate: integer; var AllPopulations: TAllPopulations;
  var theFittest: TFittest);

implementation

function Fitness(const theGuess: TIndividual; const theTarget: real): real;
var
  vertexX, vertexY, yLeft, yRight, curviness: real;
begin
  with theGuess do
  begin
    if a <= 0 then
      curviness := Math.Infinity
    else
    begin
      vertexX := -b / (2 * a); // x value at vertex
      vertexY := a * sqr(vertexX) + b * vertexX + c; // y value at vertex
      yLeft := a * sqr(LowEdge) + b * lowEdge + c; // y coordinate at lower bound
      yRight := a * sqr(HighEdge) + b * HighEdge + c; // y coordinate at upper bound
      curviness := abs(yLeft - vertexX) + abs(yRight - vertexX);
    end;
  end;
  Result := -(curviness - theTarget);
end;

function InitialPopulation(const size: integer;
  const lowBound, highBound: real): TPopulation;
var
  i: integer;
  individual: TIndividual;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
  begin
    individual.a := runif(lowBound, highBound);
    individual.b := runif(lowBound, highBound);
    individual.c := runif(lowBound, highBound);
    Result[i] := individual;
  end;
end;

function IncIndex(const size: integer): TIntArray;
var
  i: integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    Result[i] := i;
end;

function Fittest(const Population: TPopulation): TIndividual;
var
  i, index: integer;
  bestScore: real;
begin
  index := 0;
  bestScore := -Math.Infinity;
  for i := 0 to length(Population) - 1 do
  begin
    if Population[i].fitness > bestScore then
    begin
      bestScore := Population[i].fitness;
      index := i;
    end;
  end;
  Result := Population[index];
end;

function Selection(const population: TPopulation;
  const TournamentSize: integer): TPopulation;
var
  indices, tournament: TIntArray;
  competitors: TPopulation;
  winner: TIndividual;
  i, j: integer;
begin
  SetLength(tournament, TournamentSize);
  SetLength(competitors, TournamentSize);
  SetLength(Result, length(Population));
  indices := IncIndex(length(population));
  for i := 0 to length(population) - 1 do
  begin
    tournament := Sample(Indices, TournamentSize);
    for j := 0 to TournamentSize - 1 do
      competitors[j] := population[tournament[j]];
    winner := Fittest(competitors);
    Result[i] := winner;
  end;
end;

function Crossover(const parents: TParents): TChildren;
var
  alleles: record
    a, b, c: tAllele;
    end;
  meioticIndex, crossing: TIntArray;
begin
  SetLength(meioticIndex, 2);
  SetLength(crossing, 2);
  meioticIndex[0] := 0;
  meioticIndex[1] := 1;
  alleles.a[0] := parents[0].a;
  alleles.a[1] := parents[1].a;
  alleles.b[0] := parents[0].b;
  alleles.b[1] := parents[1].b;
  alleles.c[0] := parents[0].c;
  alleles.c[1] := parents[1].c;
  crossing[0] := Sample(meioticIndex, 1)[0];
  crossing[1] := 1 - crossing[0];
  Result[0].a := alleles.a[crossing[0]];
  Result[1].a := alleles.a[crossing[1]];
  crossing[0] := Sample(meioticIndex, 1)[0];
  crossing[1] := 1 - crossing[0];
  Result[0].b := alleles.b[crossing[0]];
  Result[1].b := alleles.b[crossing[1]];
  crossing[0] := Sample(meioticIndex, 1)[0];
  crossing[1] := 1 - crossing[0];
  Result[0].c := alleles.c[crossing[0]];
  Result[1].c := alleles.c[crossing[1]];
end;

function Mutated(const Individual: TIndividual; const MutationRate: integer;
  const lowBound, highBound: real): TIndividual;
var
  intensity: real;
begin
  Result := Individual;
  if random < MutationRate then
  begin
    intensity := runif(-1, 1);
    Result.a := Individual.a * intensity;
    Result.a := max(min(Result.a, highBound), lowBound);
    Result.b := Individual.b * intensity;
    Result.b := max(min(Result.b, highBound), lowBound);
    Result.c := Individual.c * intensity;
    Result.c := max(min(Result.c, highBound), lowBound);
  end;
end;

procedure GeneticAlgorithm(const size: integer; const CRH: extended;
  var params: TParams; const lowBound, highBound: real;
  const EvoTargets: TEvoTargets; const generations: integer;
  const mutationRate: integer; var AllPopulations: TAllPopulations;
  var theFittest: TFittest);
var
  curPopulation, nextPopulation: TPopulation;
  bestIndividual: TIndividual;
  parents: TParents;
  children: TChildren;
  i, j, k: integer;
begin
  SetLength(AllPopulations, generations);
  SetLength(theFittest, generations);
  SetLength(nextPopulation, size);
  curPopulation := InitialPopulation(Populationsize, LowerBound, UpperBound);
  if isNan(params.GR) then    // for development and debugging only
    params.GR := 1;
  if isNan(params.GE) then
    params.GE := 1;
  {for i := 0 to generations - 1 do
  begin
    for j := 0 to size - 1 do
      curPopulation[j].fitness := Fitness(curPopulation[j], 0);
    bestIndividual := Fittest(curPopulation);
    theFittest[i] := bestIndividual;
    AllPopulations[i] := CurPopulation;
    curPopulation := Selection(curPopulation, TournamentSize);
    for k := 0 to length(curPopulation) - 1 do
    begin
      if not odd(k) then
      begin
        parents[0] := curPopulation[k];
        parents[1] := curPopulation[k + 1];
        children := Crossover(parents);
        nextPopulation[k] := Mutated(children[0], MutationRate, LowerBound, UpperBound);
        nextPopulation[k + 1] :=
          Mutated(children[1], MutationRate, LowerBound, UpperBound);
      end;
    end;
    nextPopulation[0] := bestIndividual;
    curPopulation := nextPopulation;
  end;}

end;

end.
