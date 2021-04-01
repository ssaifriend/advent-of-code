let data = Node.Fs.readFileAsUtf8Sync("input/2020/2020.12.input")
let rows = data->Js.String2.split("\n")

module Action = {
  type t =
    | East
    | West
    | North
    | South
    | Left
    | Right
    | Forward

  exception InvalidAction

  let make = action =>
    switch action {
    | "E" => East
    | "W" => West
    | "N" => North
    | "S" => South
    | "L" => Left
    | "R" => Right
    | "F" => Forward
    | _ => raise(InvalidAction)
    }
}

module Coord = {
  type t = {
    x: int,
    y: int,
  }
}

module Operation = {
  type t = {
    action: Action.t,
    value: int,
  }

  let make = inputStr => {
    action: inputStr->Js.String2.get(0)->Action.make,
    value: inputStr->Js.String2.sliceToEnd(~from=1)->Util.Int.fromStringExn,
  }
}

module Ship = {
  open Action

  type t = {
    coord: Coord.t,
    direction: Action.t,
  }

  let rec rotate = (pos, operation: Operation.t) => {
    let direction = switch pos.direction {
    | East => operation.action == Left ? North : South
    | West => operation.action == Left ? South : North
    | North => operation.action == Left ? West : East
    | South => operation.action == Left ? East : West
    | currentDirection => currentDirection
    }

    let movedPosition = {...pos, direction: direction}
    if operation.value - 90 > 0 {
      movedPosition->rotate({...operation, value: operation.value - 90})
    } else {
      movedPosition
    }
  }

  let moveX = (pos, op, value) => {
    ...pos,
    coord: {
      ...pos.coord,
      x: op(pos.coord.x, value),
    },
  }

  let moveY = (pos, op, value) => {
    ...pos,
    coord: {
      ...pos.coord,
      y: op(pos.coord.y, value),
    },
  }

  let moveDirection = (pos, action, value) =>
    switch action {
    | East => pos->moveX(\"+", value)
    | West => pos->moveX(\"-", value)
    | North => pos->moveY(\"+", value)
    | South => pos->moveY(\"-", value)
    | _ => pos
    }

  let move = (pos, operation: Operation.t) =>
    switch operation.action {
    | Left
    | Right =>
      pos->rotate(operation)
    | East
    | West
    | North
    | South =>
      pos->moveDirection(operation.action, operation.value)
    | Forward => pos->moveDirection(pos.direction, operation.value)
    }
}

let operations = rows->Belt.Array.map(Operation.make)
let initialPosition: Ship.t = {
  coord: {
    x: 0,
    y: 0,
  },
  direction: Action.East,
}
let movedPosition = operations->Belt.Array.reduce(initialPosition, Ship.move)

Js.log(movedPosition)
Js.log(movedPosition.coord.x->Js.Math.abs_int + movedPosition.coord.y->Js.Math.abs_int)

// part2
module ShipWithWayPoint = {
  open Action

  type t = {
    coord: Coord.t,
    unit: Coord.t,
  }

  let rec rotate = (pos, operation: Operation.t) => {
    let unit: Coord.t = switch operation.action {
    | Right => {y: pos.unit.x * -1, x: pos.unit.y}
    | Left => {y: pos.unit.x, x: pos.unit.y * -1}
    | _ => pos.unit
    }

    let movedPosition = {...pos, unit: unit}
    if operation.value - 90 > 0 {
      movedPosition->rotate({...operation, value: operation.value - 90})
    } else {
      movedPosition
    }
  }

  let moveWayPoint = (pos, action, value) => {
    let moveX = (pos, op, value) => {
      ...pos,
      unit: {
        ...pos.unit,
        x: op(pos.unit.x, value),
      },
    }

    let moveY = (pos, op, value) => {
      ...pos,
      unit: {
        ...pos.unit,
        y: op(pos.unit.y, value),
      },
    }

    switch action {
    | East => pos->moveX(\"+", value)
    | West => pos->moveX(\"-", value)
    | North => pos->moveY(\"+", value)
    | South => pos->moveY(\"-", value)
    | _ => pos
    }
  }

  let moveDirection = (pos, value) => {
    ...pos,
    coord: {
      x: pos.coord.x + value * pos.unit.x,
      y: pos.coord.y + value * pos.unit.y,
    },
  }

  let move = (pos, operation: Operation.t) =>
    switch operation.action {
    | Left
    | Right =>
      pos->rotate(operation)
    | East
    | West
    | North
    | South =>
      pos->moveWayPoint(operation.action, operation.value)
    | Forward => pos->moveDirection(operation.value)
    }
}

let initialPosition: ShipWithWayPoint.t = {
  coord: {
    x: 0,
    y: 0,
  },
  unit: {
    x: 10,
    y: 1,
  },
}
let movedPosition = operations->Belt.Array.reduce(initialPosition, ShipWithWayPoint.move)

Js.log(movedPosition)
Js.log(movedPosition.coord.x->Js.Math.abs_int + movedPosition.coord.y->Js.Math.abs_int)
