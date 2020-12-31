let data = Node.Fs.readFileSync("input/2020/2020.5.input", `utf8);

type boarding = {
  rows: array(string),
  cols: array(string),
};

let boardings =
  data
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => {
      {
        rows:
          row->Js.String2.substring(~from=0, ~to_=7)->Js.String2.split(""),
        cols:
          row->Js.String2.substring(~from=7, ~to_=10)->Js.String2.split(""),
      }
    });

// part1
type boundary = {
  low: int,
  high: int,
};
type result = {
  row: int,
  col: int,
};

let binaryRange = (b, isUpper) => {
  let range = (b.high + 1 - b.low) / 2;
  if (isUpper) {
    {low: b.low + range, high: b.high};
  } else {
    {low: b.low, high: b.high - range};
  };
};

// part1
let seatIds =
  boardings
  ->Belt.Array.map(boarding => {
      let seat = {
        row:
          boarding.rows
          ->Belt.Array.reduce({low: 0, high: 127}, (b, c) =>
              b->binaryRange(c == "B")
            ).
            low,
        col:
          boarding.cols
          ->Belt.Array.reduce({low: 0, high: 7}, (b, c) =>
              b->binaryRange(c == "R")
            ).
            low,
      };

      seat.row * 8 + seat.col;
    })
  ->Belt.Set.Int.fromArray;

Js.log(seatIds->Belt.Set.Int.maximum);

// part2
let minSeatId = seatIds->Belt.Set.Int.minimum->Belt.Option.getExn;
let seatLength = seatIds->Belt.Set.Int.size;
let seatIdArray = seatIds->Belt.Set.Int.toArray;
let seatId =
  seatIdArray->Belt.Array.reduceWithIndex(
    minSeatId, (seatId, currentSeatId, index) => {
    switch (index) {
    | 0 => seatId
    | _ when index + 1 != seatLength =>
      seatIdArray[index + 1] != currentSeatId + 1 ? currentSeatId + 1 : seatId
    | _ => seatId
    }
  });

Js.log(seatId);
