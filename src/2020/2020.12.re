let data = Node.Fs.readFileSync("input/2020/2020.12.input", `utf8);
let rows = data->Js.String2.split("\n");

module Action = {
  type t =
    | East
    | West
    | North
    | South
    | Left
    | Right
    | Forward;

  exception InvalidAction;

  let make = action => {
    switch (action) {
    | "E" => East
    | "W" => West
    | "N" => North
    | "S" => South
    | "L" => Left
    | "R" => Right
    | "F" => Forward
    | _ => raise(InvalidAction)
    };
  };
};

module Coord = {
  type t = {
    x: int,
    y: int,
  };
};

module Operation = {
  type t = {
    action: Action.t,
    value: int,
  };

  let make = inputStr => {
    {
      action: inputStr->Js.String2.get(0)->Action.make,
      value:
        inputStr
        ->Js.String2.sliceToEnd(~from=1)
        ->Belt.Int.fromString
        ->Belt.Option.getExn,
    };
  };
};

module Ship = {
  open Action;

  type t = {
    coord: Coord.t,
    direction: Action.t,
  };

  let rec rotate = (pos, operation: Operation.t) => {
    let direction =
      switch (pos.direction) {
      | East => operation.action == Left ? North : South
      | West => operation.action == Left ? South : North
      | North => operation.action == Left ? West : East
      | South => operation.action == Left ? East : West
      | currentDirection => currentDirection
      };

    let movedPosition = {...pos, direction};
    if (operation.value - 90 > 0) {
      movedPosition->rotate({...operation, value: operation.value - 90});
    } else {
      movedPosition;
    };
  };

  let moveX = (pos, op, value) => {
    {
      ...pos,
      coord: {
        ...pos.coord,
        x: op(pos.coord.x, value),
      },
    };
  };

  let moveY = (pos, op, value) => {
    {
      ...pos,
      coord: {
        ...pos.coord,
        y: op(pos.coord.y, value),
      },
    };
  };

  let moveDirection = (pos, action, value) => {
    switch (action) {
    | East => pos->moveX((+), value)
    | West => pos->moveX((-), value)
    | North => pos->moveY((+), value)
    | South => pos->moveY((-), value)
    | _ => pos
    };
  };

  let move = (pos, operation: Operation.t) => {
    switch (operation.action) {
    | Left
    | Right => pos->rotate(operation)
    | East
    | West
    | North
    | South => pos->moveDirection(operation.action, operation.value)
    | Forward => pos->moveDirection(pos.direction, operation.value)
    };
  };
};

let operations = rows->Belt.Array.map(Operation.make);
let initialPosition: Ship.t = {
  coord: {
    x: 0,
    y: 0,
  },
  direction: Action.East,
};
let movedPosition =
  operations->Belt.Array.reduce(initialPosition, (pos, operation) =>
    pos->Ship.move(operation)
  );

Js.log(movedPosition);
Js.log(
  movedPosition.coord.x->Js.Math.abs_int
  + movedPosition.coord.y->Js.Math.abs_int,
);
