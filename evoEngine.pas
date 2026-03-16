unit evoEngine;

{ SimulAdren }

{ Simulator for HPA feedback control }

{ Simulation program for the hypothalamus-pituitary-adrenal axis }
{ Functionality for evolutionary algorithms }

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
  Classes, SysUtils, Math, RandomFunctions, SimulAdrenTypes, SimulationEngine;

const
  LowerBound = 0;
  UpperBound = 100;
  PopulationSize = 50;
  Generations = 50;
  MutationRate = 0.1;
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

function Fitness(const CRH: extended; const params: TStrucPars;
  const theGuess: TIndividual; const EvoTargets: TEvoTargets): real;
function InitialPopulation(const size: integer; const params: TStrucPars;
  const lowBound, highBound: real): TPopulation;
function Selection(const population: TPopulation;
  const TournamentSize: integer): TPopulation;
function Crossover(const parents: TParents; const params: TStrucPars): TChildren;
function Mutated(const Individual: TIndividual; const params: TStrucPars;
  const MutationRate: real; const lowBound, highBound: real): TIndividual;
procedure GeneticAlgorithm(const CRH: extended;
  var model: tActiveModel; const EvoTargets: TEvoTargets;
  var AllPopulations: TAllPopulations; var theFittest: TFittest);

implementation

function Fitness(const CRH: extended; const params: TStrucPars;
  const theGuess: TIndividual; const EvoTargets: TEvoTargets): real;
  { A higher result denotes a higher fitness }
var
  i: integer; // index for steady-state solution to use
  distanceA, distanceF, distance: real; // distances from target
  steadyState: TPredictionArray; // steady-state solutions
  testParams: TStrucPars; // parameter set for feedback loop to test
begin
  testParams := params; // passed parameters for feedback loop
  if isNan(testParams.GE) then  // NaN if to be estimated by GA
    testParams.GE := theGuess.GE;
  if isNan(testParams.GR) then
    testParams.GR := theGuess.GR;
  // penalise physiologically nonsense parameters:
  if (theGuess.GE <= 0) or (theGuess.GR <= 0) then
    distance := Math.Infinity
  else
  begin
    steadyState := PredictSteadyState(CRH, testParams);
    if steadyState[0].ACTH > steadyState[1].ACTH then
      i := 0
    else
      i := 1;
    distanceA := steadyState[i].ACTH - EvoTargets.ACTH;
    distanceF := steadyState[i].F - EvoTargets.F;
    // Euclidian distance of ACTH and F from the target:
    distance := sqrt(sqr(distanceA) + sqr(distanceF));
  end;
  Result := -distance;
end;

function InitialPopulation(const size: integer; const params: TStrucPars;
  const lowBound, highBound: real): TPopulation;
  { params: passed record of parameters. Parameters to be modified marked by NaN }
var
  i: integer;
  individual: TIndividual;
begin
  assert(size <> 0, kError100);
  assert(size >= 0, kError101);
  assert(highBound > lowBound, kError103);
  SetLength(Result, size);
  for i := 0 to size - 1 do
  begin
    if isNan(params.GE) then   // NaN if to be estimated by GA
      individual.GE := runif(lowBound, highBound);
    if isNan(params.GR) then
      individual.GR := runif(lowBound, highBound);
    Result[i] := individual;
  end;
end;

function Fittest(const Population: TPopulation): TIndividual;
{ delivers the fittest member of a population }
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
  { evolutionary selection algorithm based on a tournament method }
var
  indices, tournament: TIntArray;
  competitors: TPopulation;
  winner: TIndividual;
  i, j: integer;
begin
  assert(TournamentSize <> 0, kError100);
  assert(TournamentSize >= 0, kError101);
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

function Crossover(const parents: TParents; const params: TStrucPars): TChildren;
{ params: passed record of parameters. Parameters to be modified marked by NaN }
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
  if isNan(params.GE) then  // NaN if to be estimated by GA
  begin
    alleles.GE[0] := parents[0].GE;
    alleles.GE[1] := parents[1].GE;
    crossing[0] := Sample(meioticIndex, 1)[0];
    crossing[1] := 1 - crossing[0];
    Result[0].GE := alleles.GE[crossing[0]];
    Result[1].GE := alleles.GE[crossing[1]];
  end;
  if isNan(params.GR) then
  begin
    alleles.GR[0] := parents[0].GR;
    alleles.GR[1] := parents[1].GR;
    crossing[0] := Sample(meioticIndex, 1)[0];
    crossing[1] := 1 - crossing[0];
    Result[0].GR := alleles.GR[crossing[0]];
    Result[1].GR := alleles.GR[crossing[1]];
  end;
end;

function Mutated(const Individual: TIndividual; const params: TStrucPars;
  const MutationRate: real; const lowBound, highBound: real): TIndividual;
  { params: passed record of parameters. Parameters to be modified marked by NaN }
var
  intensity: real;
begin
  assert(MutationRate <> 0, kError100);
  assert(MutationRate >= 0, kError101);
  assert(highBound > lowBound, kError103);
  Result := Individual;
  if random < MutationRate then
  begin
    intensity := runif(-1, 1);
    if isNan(params.GE) then  // NaN if to be estimated by GA
    begin
      Result.GE := Individual.GE * intensity;
      Result.GE := max(min(Result.GE, highBound), lowBound);
    end;
    if isNan(params.GR) then
    begin
      Result.GR := Individual.GR * intensity;
      Result.GR := max(min(Result.GR, highBound), lowBound);
    end;
  end;
end;

procedure GeneticAlgorithm(const CRH: extended;
  var model: tActiveModel; const EvoTargets: TEvoTargets;
  var AllPopulations: TAllPopulations; var theFittest: TFittest);
  { params: passed record of parameters. Parameters to be modified marked by NaN }
var
  curPopulation, nextPopulation: TPopulation;
  bestIndividual: TIndividual;
  parents: TParents;
  children: TChildren;
  i, j, k: integer;
begin
  assert(EvoTargets.PopulationSize <> 0, kError100);
  assert(EvoTargets.PopulationSize >= 0, kError101);
  assert(EvoTargets.HighEdge > EvoTargets.LowEdge, kError103);
  assert(EvoTargets.Generations <> 0, kError100);
  assert(EvoTargets.Generations >= 0, kError101);
  assert(EvoTargets.MutationRate <> 0, kError100);
  assert(EvoTargets.MutationRate >= 0, kError101);
  SetLength(AllPopulations, EvoTargets.Generations);
  SetLength(theFittest, EvoTargets.Generations);
  SetLength(nextPopulation, EvoTargets.PopulationSize);
  curPopulation := InitialPopulation(EvoTargets.PopulationSize, model.StrucPars, EvoTargets.LowEdge, EvoTargets.HighEdge);
  for i := 0 to EvoTargets.Generations - 1 do
  begin
    for j := 0 to EvoTargets.PopulationSize - 1 do
      curPopulation[j].fitness :=
        Fitness(CRH, model.StrucPars, curPopulation[j], EvoTargets);
    bestIndividual := Fittest(curPopulation);
    theFittest[i] := bestIndividual;
    AllPopulations[i] := CurPopulation;
    curPopulation := Selection(curPopulation, EvoTargets.TournamentSize);
    for k := 0 to length(curPopulation) - 1 do
    begin
      if not odd(k) then
      begin
        parents[0] := curPopulation[k];
        parents[1] := curPopulation[k + 1];
        children := Crossover(parents, model.StrucPars);
        nextPopulation[k] := Mutated(children[0], model.StrucPars, EvoTargets.MutationRate,
          EvoTargets.LowEdge, EvoTargets.HighEdge);
        nextPopulation[k + 1] :=
          Mutated(children[1], model.StrucPars, EvoTargets.MutationRate, EvoTargets.LowEdge, EvoTargets.HighEdge);
      end;
    end;
    nextPopulation[0] := bestIndividual;
    curPopulation := nextPopulation;
  end;
  if isNan(model.StrucPars.GR) then  // NaN if to be estimated by GA
    model.StrucPars.GR := theFittest[generations - 1].GR;
  if isNan(model.StrucPars.GE) then
    model.StrucPars.GE := theFittest[generations - 1].GE;
end;

end.
