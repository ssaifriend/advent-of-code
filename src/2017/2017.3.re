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

module PlusOneGenerator = {
  open Coord;

  type t = list(Coord.t);
  let makeValue = ((_, allValues)) =>
    switch (allValues->Belt.List.head) {
    | Some(coord) => coord.value + 1
    | None => 0
    };

  let endCondition = (target, value) => target == value;
};

module Finder = (G: Generator with type t := (Coord.t, list(Coord.t))) => {
  open Coord;
  open Stepper;
  open Direction;

  let rec find = (target, coord, step, allCoords) => {
    let nextCoord =
      switch (step.direction) {
      | RIGHT => {...coord, x: coord.x + 1}
      | UP => {...coord, y: coord.y + 1}
      | LEFT => {...coord, x: coord.x - 1}
      | DOWN => {...coord, y: coord.y - 1}
      };

    let nextValue = G.makeValue((nextCoord, allCoords));

    let nextCoord = {...nextCoord, value: nextValue};
    let nextStep = step->Stepper.makeNextStep;

    let nextAllCoords = [nextCoord, ...allCoords];

    target->G.endCondition(nextCoord.value)
      ? nextCoord : target->find(nextCoord, nextStep, nextAllCoords);
  };

  let make = target => {
    let startCoord: Coord.t = {x: 0, y: 0, value: 1};
    let startStep: Stepper.t = {step: 1, direction: RIGHT, remain: 1};

    target->find(startCoord, startStep, [startCoord]);
  };

  let coordLength = coord =>
    coord.x->Js.Math.abs_int + coord.y->Js.Math.abs_int;
};

module PlusOneFinder = Finder(PlusOneGenerator);

12->PlusOneFinder.make->PlusOneFinder.coordLength->Js.log;
23->PlusOneFinder.make->PlusOneFinder.coordLength->Js.log;
1024->PlusOneFinder.make->PlusOneFinder.coordLength->Js.log;
265149->PlusOneFinder.make->PlusOneFinder.coordLength->Js.log;

module AroundGenerator = {
  open Coord;

  type t = list(Coord.t);
  let makeValue = ((nextCoord, allValues)) =>
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

4->AroundFinder.make->Js.log;
363->AroundFinder.make->Js.log;
265149->AroundFinder.make->Js.log;
