module OperationType = {
  type t =
    | TURN(bool)
    | TOGGLE;
};

module Coord = {
  type t = {
    x: int,
    y: int,
  };
};

module Operation = {
  type t = {
    type_: OperationType.t,
    start: Coord.t,
    end_: Coord.t,
  };
};

module Parser = {
  open OperationType;
  open Operation;

  let make = input => {
    let re =
      Js.Re.fromString(
        "(turn off|turn on|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)",
      );
    let result = re->Js.Re.exec_(input);
    switch (result) {
    | None => raise(Not_found)
    | Some(r) =>
      let captures =
        r
        ->Js.Re.captures
        ->Belt.Array.map(Js.Nullable.toOption)
        ->Belt.Array.keepMap(x => x);
      let opType =
        switch (captures[1]) {
        | "turn off" => TURN(false)
        | "turn on" => TURN(true)
        | "toggle" => TOGGLE
        | _ => raise(Not_found)
        };

      {
        type_: opType,
        start: {
          x: captures[2]->Belt.Int.fromString->Belt.Option.getExn,
          y: captures[3]->Belt.Int.fromString->Belt.Option.getExn,
        },
        end_: {
          x: captures[4]->Belt.Int.fromString->Belt.Option.getExn,
          y: captures[5]->Belt.Int.fromString->Belt.Option.getExn,
        },
      };
    };
  };
};

module CoordMap = {
  module Comp =
    Belt.Id.MakeComparable({
      type t = Coord.t;
      let cmp = (coord1: t, coord2: t) => {
        switch (Pervasives.compare(coord1.x, coord2.x)) {
        | 0 => Pervasives.compare(coord1.y, coord2.y)
        | c => c
        };
      };
    });

  type t = Belt.Map.t(Coord.t, int, Comp.identity);
};

module type Executer = {
  let doOperation: (CoordMap.t, Coord.t, OperationType.t) => CoordMap.t;
  let counter: CoordMap.t => int;
};

module Runner = (Executer: Executer) => {
  open Coord;

  let makeCoords = (operation: Operation.t) =>
    Belt.Array.range(operation.start.x, operation.end_.x)
    ->Belt.Array.reduce([], (coords, x) => {
        Belt.Array.range(operation.start.y, operation.end_.y)
        ->Belt.Array.reduce(coords, (coords, y) => {[{x, y}, ...coords]})
      });

  let doOperation = (operation: Operation.t, map) => {
    operation
    ->makeCoords
    ->Belt.List.reduce(map, (map, coord) =>
        map->Executer.doOperation(coord, operation.type_)
      );
  };

  let rec doOperations = (operations, map) => {
    switch (operations->Belt.List.head, operations->Belt.List.tail) {
    | (None, _) => map
    | (Some(operation), None) => operation->doOperation(map)
    | (Some(operation), Some(tail)) =>
      let map = operation->doOperation(map);

      tail->doOperations(map);
    };
  };

  let turnOnLightsCount = Executer.counter;
};

module BooleanExecuter = {
  let doOperation = (map, coord: Coord.t, type_: OperationType.t) => {
    switch (type_) {
    | TURN(true) => map->Belt.Map.set(coord, 0)
    | TURN(false) => map->Belt.Map.remove(coord)
    | TOGGLE =>
      map->Belt.Map.has(coord)
        ? map->Belt.Map.remove(coord) : map->Belt.Map.set(coord, 0)
    };
  };

  let counter = map => {
    map->Belt.Map.size;
  };
};

module BooleanRunner = Runner(BooleanExecuter);

let map = Belt.Map.make(~id=(module CoordMap.Comp));

"turn on 0,0 through 999,999"
->Js.String2.split("\n")
->Belt.Array.map(Parser.make)
->Belt.List.fromArray
->BooleanRunner.doOperations(map)
->BooleanRunner.turnOnLightsCount
->Js.log;

Node.Fs.readFileAsUtf8Sync("input/2015/2015.6.input")
->Js.String2.split("\n")
->Belt.Array.map(Parser.make)
->Belt.List.fromArray
->BooleanRunner.doOperations(map)
->BooleanRunner.turnOnLightsCount
->Js.log;

module CountExecuter = {
  let doOperation = (map, coord: Coord.t, type_: OperationType.t) => {
    switch (map->Belt.Map.get(coord), type_) {
    | (None, TURN(true)) => map->Belt.Map.set(coord, 1)
    | (None, TURN(false)) => map
    | (None, TOGGLE) => map->Belt.Map.set(coord, 2)

    | (Some(oldValue), TURN(true)) =>
      map->Belt.Map.set(coord, oldValue + 1)
    | (Some(oldValue), TURN(false)) =>
      oldValue > 0 ? map->Belt.Map.set(coord, oldValue - 1) : map
    | (Some(oldValue), TOGGLE) => map->Belt.Map.set(coord, oldValue + 2)
    };
  };

  let counter = map => {
    map->Belt.Map.valuesToArray->Belt.Array.reduce(0, (+));
  };
};

module CountRunner = Runner(CountExecuter);

"turn on 0,0 through 0,0"
->Js.String2.split("\n")
->Belt.Array.map(Parser.make)
->Belt.List.fromArray
->CountRunner.doOperations(map)
->CountRunner.turnOnLightsCount
->Js.log;

Node.Fs.readFileAsUtf8Sync("input/2015/2015.6.input")
->Js.String2.split("\n")
->Belt.Array.map(Parser.make)
->Belt.List.fromArray
->CountRunner.doOperations(map)
->CountRunner.turnOnLightsCount
->Js.log;
