module Coord = {
  type t = {
    x: int,
    y: int,
    value: int,
  };
};

module Direction = {
  type t =
    | LEFT
    | RIGHT
    | UP
    | DOWN;
};

module Stepper = {
  type t = {
    step: int,
    direction: Direction.t,
    remain: int,
  };

  let makeNextStep = step => {
    switch (step.remain == 1, step.direction) {
    | (false, _) => {...step, remain: step.remain - 1}
    | (true, RIGHT) => {...step, direction: UP, remain: step.step}
    | (true, UP) => {
        step: step.step + 1,
        direction: LEFT,
        remain: step.step + 1,
      }
    | (true, LEFT) => {...step, direction: DOWN, remain: step.step}
    | (true, DOWN) => {
        step: step.step + 1,
        direction: RIGHT,
        remain: step.step + 1,
      }
    };
  };
};

module type Generator = {
  type t;

  let makeValue: t => int;
  let endCondition: (int, int) => bool;
};

module Finder = (Generator: Generator) => {
  open Coord;
  open Stepper;
  open Direction;

  let makeValue = (t: Generator.t) => t->Generator.makeValue;

  let rec make = (target, coord, step, allCoords) => {
    let nextCoord =
      switch (step.direction) {
      | RIGHT => {...coord, x: coord.x + 1}
      | UP => {...coord, y: coord.y + 1}
      | LEFT => {...coord, x: coord.x - 1}
      | DOWN => {...coord, y: coord.y - 1}
      };

    let nextValue = 
      switch (Generator.t) {
        | int => coord.value->Generator.makeValue
        | list => allCoords->Generator.makeValue
      };

    let nextCoord = {...nextCoord, value: nextValue};
    let nextStep = step->makeNextStep;

    let nextAllCoords = [nextCoord, ...allCoords];

    target->Generator.endCondition(nextCoord.value)
      ? nextCoord : target->make(nextCoord, nextStep, nextAllCoords);
  };

  let coordLength = coord =>
    coord.x->Js.Math.abs_int + coord.y->Js.Math.abs_int;
};

module PlusOneGenerator = {
  type t = int;
  let makeValue = value => value + 1;
  let endCondition = (target, value) => target == value;
};

module PlusOneFinder = Finder(PlusOneGenerator);

let startCoord: Coord.t = {x: 0, y: 0, value: 1};
let startStep: Stepper.t = {step: 1, direction: RIGHT, remain: 1};

12
->PlusOneFinder.make(startCoord, startStep)
->PlusOneFinder.coordLength
->Js.log;
23
->PlusOneFinder.make(startCoord, startStep)
->PlusOneFinder.coordLength
->Js.log;
1024
->PlusOneFinder.make(startCoord, startStep)
->PlusOneFinder.coordLength
->Js.log;
265149
->PlusOneFinder.make(startCoord, startStep)
->PlusOneFinder.coordLength
->Js.log;

module AroundGenerator = {
  type t = list;
  let makeValue = allValues =>
    allValues
    ->Belt.List.keep(coord => {
        switch (
          Js.Math.abs_int(nextCoord.x - coord.x),
          Js.Math.abs_int(nextCoord.y - coord.y),
        ) {
        | (1, 0)
        | (1, 1)
        | (0, 1) => true
        | _ => false
        }
      })
    ->Belt.List.map(coord => coord.value)
    ->Belt.List.reduce(0, (+));

  let endCondition = (target, value) => target <= value;
};

module AroundFinder = Finder(AroundGenerator);

4->AroundFinder.make(startCoord, startStep, [startCoord])->Js.log;
363->AroundFinder.make(startCoord, startStep, [startCoord])->Js.log;
265149->AroundFinder.make(startCoord, startStep, [startCoord])->Js.log;
