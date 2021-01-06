let data = Node.Fs.readFileSync("input/2020/2020.11.input", `utf8);
let rows = data->Js.String2.split("\n");

module Seat = {
  type t =
    | Empty
    | Occupied
    | Floor;

  exception InvalidState;

  let make = (inputStr) => 
    switch (inputStr) {
    | "L" => Empty
    | "." => Floor
    | _ => raise(InvalidState)
    }
};

module Seats = {
  type t = array(Seat.t);
};

module Coord = {
  type t = {
    row: int,
    col: int,
  };

  let makeAroundCoords = (r, c) => {
    [
      {row: r - 1, col: c - 1},
      {row: r - 1, col: c},
      {row: r - 1, col: c + 1},
      {row: r, col: c - 1},
      // {row: r, col: c}, // 내 위치
      {row: r, col: c + 1},
      {row: r + 1, col: c - 1},
      {row: r + 1, col: c},
      {row: r + 1, col: c + 1},
    ];
  };
};


let seatsMap =
  rows->Belt.Array.map(row => {
    row
    ->Js.String2.split("")
    ->Belt.Array.map(Seat.make)
  });


// part1
let occupiedCount = (seatsMap, positions: list(Coord.t)) => {
  positions
  ->Belt.List.map(position => {
      switch (seatsMap->Belt.Array.get(position.row)) {
      | None => false
      | Some(seats) =>
        switch (seats->Belt.Array.get(position.col)) {
        | Some(seat) => seat == Seat.Occupied
        | _ => false
        }
      }
    })
  ->Belt.List.keep(x => x)
  ->Belt.List.size;
};

module type MakeNextState = {let next: int => Seat.t;};

module NextState = (Item: MakeNextState) => {
  let nextWithIndex = (seatsMap, r, c) => {
    let positions = Coord.makeAroundCoords(r, c);
    let occupiedCount = seatsMap->occupiedCount(positions);

    Item.next(occupiedCount);
  };
};

module MakeEmptyNextState = {
  open Seat;
  let next = occupiedCount => occupiedCount == 0 ? Occupied : Empty;
};
module EmptyState = NextState(MakeEmptyNextState);

module MakeOccupiedNextState = {
  open Seat;
  let next = occupiedCount => occupiedCount >= 4 ? Empty : Occupied;
};
module OccupiedState = NextState(MakeOccupiedNextState);

let makeNextState = seatsMap => {
  seatsMap->Belt.Array.mapWithIndex((rIndex, seats) => {
    seats->Belt.Array.mapWithIndex((cIndex, seat) => {
      switch (seat) {
      | Seat.Floor => Seat.Floor
      | Empty => seatsMap->EmptyState.nextWithIndex(rIndex, cIndex)
      | Occupied => seatsMap->OccupiedState.nextWithIndex(rIndex, cIndex)
      }
    })
  });
};

module SeatsCompare = {
  type t = {
    seat1: Seats.t,
    seat2: Seats.t,
  };

  let isEqualSeatWithIndex = (seats, index) =>
    seats.seat1[index] == seats.seat2[index];

  let rec isEqualSeats = (seats, index): bool => {
    let result = seats->isEqualSeatWithIndex(index);

    if (!result || seats.seat1->Belt.Array.size == index + 1) {
      result;
    } else {
      seats->isEqualSeats(index + 1);
    };
  };

  let make = (seat1, seat2) => {
    {seat1, seat2};
  };
};

module SeatsMap = {
  type t = {
    seat1: array(Seats.t),
    seat2: array(Seats.t),
  };

  let rec isEqualRow = (seatsMap, index): bool => {
    let result =
      SeatsCompare.make(seatsMap.seat1[index], seatsMap.seat2[index])
      ->SeatsCompare.isEqualSeats(0);

    if (!result || seatsMap.seat1->Belt.Array.size == index + 1) {
      result;
    } else {
      seatsMap->isEqualRow(index + 1);
    };
  };

  let isEqual = seatsMap => {
    seatsMap->isEqualRow(0);
  };
};


let rec findNoChangeState = seatsMap => {
  let seatsMap: SeatsMap.t = { // 확실한 네이밍 필요?..
    seat1: seatsMap,
    seat2: seatsMap->makeNextState,
  };

  seatsMap->SeatsMap.isEqual
    ? seatsMap.seat2 : seatsMap.seat2->findNoChangeState;
};

let noChangeSeatsMap = seatsMap->findNoChangeState;
let occupiedCount =
  noChangeSeatsMap->Belt.Array.reduce(0, (sum, row) => {
    sum + row->Belt.Array.keep(seat => seat == Occupied)->Belt.Array.size
  });

Js.log(occupiedCount);
