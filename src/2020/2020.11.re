let data = Node.Fs.readFileSync("input/2020/2020.11.input", `utf8);
let rows = data->Js.String2.split("\n");

type seatState =
  | Empty
  | Occupied
  | Floor;

exception InvalidState;

let seats =
  rows->Belt.Array.map(row => {
    row
    ->Js.String2.split("")
    ->Belt.Array.map(seat =>
        switch (seat) {
        | "L" => Empty
        | "." => Floor
        | _ => raise(InvalidState)
        }
      )
  });

// part1
type coord = {
  row: int,
  col: int,
};

let makePositions = (r, c) => {
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
}

let occupiedCount = (seats, positions) => {
  positions
    ->Belt.List.map(position => {
        switch (seats->Belt.Array.get(position.row)) {
        | Some(row) =>
          switch (row->Belt.Array.get(position.col)) {
          | Some(seat) => seat == Occupied
          | _ => false
          }
        | None => false
        }
      })
    ->Belt.List.keep(x => x)
    ->Belt.List.size;
}

let emptyToNextState = (occupiedCount) => occupiedCount == 0 ? Occupied : Empty;
let occupiedToNextState = (occupiedCount) => occupiedCount >= 4 ? Empty : Occupied;

let stateChange = (seats, r, c, nextState) => {
  let positions = makePositions(r, c);
  let occupiedCount = seats->occupiedCount(positions);

  nextState(occupiedCount);
};

let nextState = seats => {
  seats->Belt.Array.mapWithIndex((rIndex, rows) => {
    rows->Belt.Array.mapWithIndex((cIndex, seat) => {
      switch (seat) {
      | Empty => seats->stateChange(rIndex, cIndex, emptyToNextState)
      | Occupied => seats->stateChange(rIndex, cIndex, occupiedToNextState)
      | Floor => Floor
      }
    })
  });
};

let compareSeat = (beforeSeat, nextSeat) => beforeSeat == nextSeat;
let rec compareCol = (beforeSeatCols, nextSeatCols, index): bool => {
  let result = compareSeat(beforeSeatCols[index], nextSeatCols[index]);

  if (!result) {
    result
  } else {
    if (beforeSeatCols->Belt.Array.size == index + 1) {
      result
    } else {
      compareCol(beforeSeatCols, nextSeatCols, index + 1)
    }
  }
}

let rec compareRow = (beforeSeats, nextSeats, index): bool => {
  let result = compareCol(beforeSeats[index], nextSeats[index], 0);

  if (!result) {
    result
  } else {
    if (beforeSeats->Belt.Array.size == index + 1) {
      result
    } else {
      compareRow(beforeSeats, nextSeats, index + 1)
    }
  }
}

let compare = (beforeSeats, nextSeats) => {
  compareRow(beforeSeats, nextSeats, 0)
};

let rec findNoChangeState = seats => {
  let beforeSeats = seats;
  let nextSeats = seats->nextState;

  compare(beforeSeats, nextSeats) ? nextSeats : nextSeats->findNoChangeState;
};

let noChangeSeats = seats->findNoChangeState
let occupiedCount = 
  noChangeSeats
  ->Belt.Array.reduce(0, (sum, row) => {
    sum + row->Belt.Array.keep((seat) => seat == Occupied)->Belt.Array.size
  })

Js.log(occupiedCount)