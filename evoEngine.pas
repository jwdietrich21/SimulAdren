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
  LowerBound = 0;
  UpperBound = 50;
  PopulationSize = 100;
  Generations = 20;
  MutationRate = 1;
  TournamentSize = 3;

type
  TIndividual = record
    GR, GE: extended;
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

function Fitness(const CRH: extended; const params: TParams;
  const theGuess: TIndividual; const EvoTargets: TEvoTargets): real;
function InitialPopulation(const size: integer; var params: TParams;
  const lowBound, highBound: real): TPopulation;
function Selection(const population: TPopulation;
  const TournamentSize: integer): TPopulation;
function Crossover(const parents: TParents): TChildren;
function Mutated(const Individual: TIndividual; var params: TParams;
  const MutationRate: integer; const lowBound, highBound: real): TIndividual;
procedure GeneticAlgorithm(const size: integer; const CRH: extended;
  var params: TParams; const lowBound, highBound: real;
  const EvoTargets: TEvoTargets; const generations: integer;
  const mutationRate: integer; var AllPopulations: TAllPopulations;
  var theFittest: TFittest);

implementation

function Fitness(const CRH: extended; const params: TParams;
  const theGuess: TIndividual; const EvoTargets: TEvoTargets): real;
var
  i: integer;
  distanceA, distanceF, distance: real;
  steadyState: TPredictionArray;
begin
  with theGuess do
  begin
    if (GE <= 0) or (GR <= 0) then
      distance := Math.Infinity
    else
    begin
      steadyState := PredictSteadyState(CRH, params);
      if steadyState[0].ACTH > steadyState[1].ACTH then
        i := 0
      else
        i := 1;
      distanceA := steadyState[i].ACTH - EvoTargets.ACTH;
      distanceF := steadyState[i].F - EvoTargets.F;
      // Euclidian distance of ACTH and F from the target:
      Result := sqrt(sqr(distanceA) + sqr(distanceF));
    end;
  end;
  Result := -distance;
end;

function InitialPopulation(const size: integer; var params: TParams;
  const lowBound, highBound: real): TPopulation;
var
  i: integer;
  individual: TIndividual;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
  begin
    individual.GE := runif(lowBound, highBound);
    individual.GR := runif(lowBound, highBound);
    Result[i] := individual;
  end;
end;

function IncIndex(const size: integer): TIntArray;
  {delivers ordered array of integer}
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
    GE, GR: tAllele;
    end;
  meioticIndex, crossing: TIntArray;
begin
  SetLength(meioticIndex, 2);
  SetLength(crossing, 2);
  meioticIndex[0] := 0;
  meioticIndex[1] := 1;
  alleles.GE[0] := parents[0].GE;
  alleles.GE[1] := parents[1].GE;
  alleles.GR[0] := parents[0].GR;
  alleles.GR[1] := parents[1].GR;
  crossing[0] := Sample(meioticIndex, 1)[0];
  crossing[1] := 1 - crossing[0];
  Result[0].GE := alleles.GE[crossing[0]];
  Result[1].GE := alleles.GE[crossing[1]];
  crossing[0] := Sample(meioticIndex, 1)[0];
  crossing[1] := 1 - crossing[0];
  Result[0].GR := alleles.GR[crossing[0]];
  Result[1].GR := alleles.GR[crossing[1]];
end;

function Mutated(const Individual: TIndividual; var params: TParams;
  const MutationRate: integer; const lowBound, highBound: real): TIndividual;
var
  intensity: real;
begin
  Result := Individual;
  if random < MutationRate then
  begin
    intensity := runif(-1, 1);
    Result.GE := Individual.GE * intensity;
    Result.GE := max(min(Result.GE, highBound), lowBound);
    Result.GR := Individual.GR * intensity;
    Result.GR := max(min(Result.GR, highBound), lowBound);
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
  curPopulation := InitialPopulation(Populationsize, params, LowerBound, UpperBound);
  if isNan(params.GR) then    // for development and debugging only
    params.GR := 1;
  if isNan(params.GE) then
    params.GE := 1;
  {for i := 0 to generations - 1 do
  begin
    for j := 0 to size - 1 do
      curPopulation[j].fitness := Fitness(CRH, params, curPopulation[j],
        EvoTargets);
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
        nextPopulation[k] := Mutated(children[0], params, MutationRate, LowerBound, UpperBound);
        nextPopulation[k + 1] :=
          Mutated(children[1], params, MutationRate, LowerBound, UpperBound);
      end;
    end;
    nextPopulation[0] := bestIndividual;
    curPopulation := nextPopulation;
  end;    }

end;

end.
