let data = Node.Fs.readFileSync("input/2020/2020.5.input", `utf8);

module Boarding = {
  type t = {
    rows: array(string),
    cols: array(string),
  };

  let spliter = (row, ~from, ~to_) => {
    row->Js.String2.substring(~from, ~to_)->Js.String2.split("");
  };

  let make = inputStr => {
    inputStr
    ->Js.String2.split("\n")
    ->Belt.Array.map(row => {
        {
          rows: row->spliter(~from=0, ~to_=7),
          cols: row->spliter(~from=7, ~to_=10),
        }
      });
  };
};

// part1
module BinaryBoarding = {
  type t = {
    low: int,
    high: int,
  };

  let doBinary = (b, ~isUpper) => {
    let range = (b.high + 1 - b.low) / 2;
    if (isUpper) {
      {low: b.low + range, high: b.high};
    } else {
      {low: b.low, high: b.high - range};
    };
  };

  let getLow = boundary => boundary.low;

  let reducer = (inputs, boundary, ~upperComp) =>
    inputs->Belt.Array.reduce(boundary, (b, c) =>
      b->doBinary(~isUpper=c->upperComp)
    );
};

// part1
let seatIds =
  data
  ->Boarding.make
  ->Belt.Array.map(boarding => {
      let seatRow =
        boarding.rows
        ->BinaryBoarding.reducer({low: 0, high: 127}, ~upperComp=c =>
            c == "B"
          )
        ->BinaryBoarding.getLow;
      let seatCol =
        boarding.cols
        ->BinaryBoarding.reducer({low: 0, high: 7}, ~upperComp=c => c == "R")
        ->BinaryBoarding.getLow;

      seatRow * 8 + seatCol;
    })
  ->Belt.Set.Int.fromArray;

Js.log(seatIds->Belt.Set.Int.maximum);

// part2
let seatLength = seatIds->Belt.Set.Int.size;
let seatIdList = seatIds->Belt.Set.Int.toList;
let rec findSeatId = (seatIds, currentSeatId) =>
  switch (seatIds->Belt.List.head, seatIds->Belt.List.get(1)) {
  | (None, _)
  | (Some(_), None) => currentSeatId
  | (Some(seatId), Some(nextSeatId)) when seatId + 1 != nextSeatId =>
    seatId + 1
  | _ => seatIds->Belt.List.tailExn->findSeatId(currentSeatId)
  };
let seatId = seatIdList->findSeatId(seatIdList->Belt.List.headExn);

Js.log(seatId);
