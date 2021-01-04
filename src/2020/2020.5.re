let data = Node.Fs.readFileSync("input/2020/2020.5.input", `utf8);

type boarding = {
  rows: array(string),
  cols: array(string),
};

let boardings =
  data
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => {
      let spliter = (row, ~from, ~to_) =>
        row->Js.String2.substring(~from, ~to_)->Js.String2.split("");
      {
        rows: row->spliter(~from=0, ~to_=7),
        cols: row->spliter(~from=7, ~to_=10),
      };
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

let binaryRange = (b, ~isUpper) => {
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
      let reducer = (rows, boundary, ~upperComp) =>
        rows->Belt.Array.reduce(boundary, (b, c) =>
          b->binaryRange(~isUpper=c->upperComp)
        );
      let getLow = boundary => boundary.low;
      let seat = {
        row:
          boarding.rows
          ->reducer({low: 0, high: 127}, ~upperComp=c => c == "B")
          ->getLow,
        col:
          boarding.cols
          ->reducer({low: 0, high: 7}, ~upperComp=c => c == "R")
          ->getLow,
      };

      seat.row * 8 + seat.col;
    })
  ->Belt.Set.Int.fromArray;

Js.log(seatIds->Belt.Set.Int.maximum);

// part2
let seatLength = seatIds->Belt.Set.Int.size;
let seatIdArray = seatIds->Belt.Set.Int.toArray;
let rec findSeatId = (seatIds, currentSeatId, index) =>
  switch (index) {
  | _ when index + 1 == seatLength => currentSeatId
  | _ when seatIds[index] + 1 != seatIds[index + 1] =>
    seatIds->findSeatId(seatIds[index] + 1, index + 1)
  | _ => seatIds->findSeatId(currentSeatId, index + 1)
  };
let seatId = seatIdArray->findSeatId(seatIdArray[0], 0);

Js.log(seatId);
