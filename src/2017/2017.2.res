// part1
let getDifference = inputStr =>
  inputStr
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row->Js.String2.split("\t")->Belt.Array.keepMap(Belt.Int.fromString)->Belt.Set.Int.fromArray
  )
  ->Belt.Array.map(row =>
    row->Belt.Set.Int.maximum->Belt.Option.getExn - row->Belt.Set.Int.minimum->Belt.Option.getExn
  )
  ->Belt.Array.reduce(0, \"+")

"5	1	9	5
7	5	3
2	4	6	8"
->getDifference
->Js.log

Node.Fs.readFileAsUtf8Sync("input/2017/2017.2.input")->getDifference->Js.log

// part2
let getDivide = inputStr =>
  inputStr
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row
    ->Js.String2.split("\t")
    ->Belt.Array.keepMap(Belt.Int.fromString)
    ->Belt.Set.Int.fromArray
    ->Belt.Set.Int.toArray
  )
  ->Belt.Array.map(row =>
    row->Belt.Array.reduce(list{}, (tuples1, number1) =>
      row->Belt.Array.reduce(tuples1, (tuples2, number2) => list{(number1, number2), ...tuples2})
    )
  )
  ->Belt.Array.map(row =>
    row
    ->Belt.List.keep(((number1, number2)) => number1 > number2)
    ->Belt.List.keep(((number1, number2)) => mod(number1, number2) == 0)
    ->Belt.List.map(((number1, number2)) => number1 / number2)
    ->Belt.List.headExn
  )
  ->Belt.Array.reduce(0, \"+")

"5	9	2	8
9	4	7	3
3	8	6	5"
->getDivide
->Js.log

Node.Fs.readFileAsUtf8Sync("input/2017/2017.2.input")->getDivide->Js.log

/*
module type Calculable = {
  type nonrec t;
  let strConverter: string => option(t);
  let result: array(t) => t;
  let initializr: t;
  let appender: (t, t) => t;
};

module SpreadSheet = (Calculator: Calculable) => {
  open Belt;
  let puzzleInput = fileName =>
    fileName->Node_fs.readFileAsUtf8Sync->(Js.String2.split("\n"));

  let row = inputRow =>
    inputRow
    ->Js.String2.splitByRe([%re "/\\s+/"])
    ->Array.keepMap(x => x)
    ->Array.keepMap(Calculator.strConverter);

  let rows = puzzleInput => puzzleInput->Array.map(row);

  let checksum =
    "input/2017/2017.2.input"
    ->puzzleInput
    ->rows
    ->Array.map(Calculator.result)
    ->Array.reduce(Calculator.initializr, Calculator.appender);
};

module P1Calculator: Calculable = {
  type t = int;
  let strConverter = Belt.Int.fromString;
  let result = row => {
    open Belt.Set.Int;
    let s = row->fromArray;
    switch (s->maximum, s->minimum) {
    | (Some(max), Some(min)) => max - min
    | _ => 0
    };
  };
  let initializr = 0;
  let appender = (a, b) => a + b;
};

module SpreadSheetP1 = SpreadSheet(P1Calculator);
SpreadSheetP1.checksum->Js.log;

module P2Calculator = {
  type t = float; // 여기를 type t = float; 로!
  let strConverter = Belt.Float.fromString;
  let result = row => {
    open Belt.Array;
    let isDividable = (a, b) => mod_float(a, b) == 0.0 && a > b;
    row
    ->keepMap(a => row->getBy(isDividable(a))->Belt.Option.map(b => a /. b))
    ->get(0)
    ->Belt.Option.getWithDefault(0.0);
  };
  let initializr = 0.0;
  let appender = (a, b) => a +. b;
};

module SpreadSheetP2 = SpreadSheet(P2Calculator);
SpreadSheetP2.checksum->Js.log;
*/

/*
module type Calculable = {
  type nonrec t;
  let strConverter: string => option(t);
  let result: array(t) => t;
  let initializr: t;
  let appender: (t, t) => t;
};

module SpreadSheet = (Calculator: Calculable with type t = int) => {
  open Belt;
  let puzzleInput = fileName =>
    fileName->Node_fs.readFileAsUtf8Sync->(Js.String2.split("\n"));

  let row = inputRow =>
    inputRow
    ->Js.String2.splitByRe([%re "/\\s+/"])
    ->Array.keepMap(x => x)
    ->Array.keepMap(Calculator.strConverter);

  let rows = puzzleInput => puzzleInput->Array.map(row);

  let checksum =
    "input/2017/2017.2.input"
    ->puzzleInput
    ->rows
    ->Array.map(Calculator.result)
    ->Array.reduce(0, (a, b) => a + b); // ->Array.reduce(0, (a, b) => a + b); 이렇게 쓰고, initializr, appender 선언을 지우고 싶습니다.
};

module P1Calculator = {
  type t = int;
  let strConverter = Belt.Int.fromString;
  let result = row => {
    open Belt.Set.Int;
    let s = row->fromArray;
    switch (s->maximum, s->minimum) {
    | (Some(max), Some(min)) => max - min
    | _ => 0
    };
  };
  let initializr = 0;
  let appender = (a, b) => a + b;
};

module SpreadSheetP1 = SpreadSheet(P1Calculator);
SpreadSheetP1.checksum->Js.log;

module P2Calculator = {
  type t = int;
  let strConverter = Belt.Int.fromString;
  let result = row => {
    open Belt.Array;
    let isDividable = (a, b) => a mod b == 0 && a > b;
    row
    ->keepMap(a => row->getBy(isDividable(a))->Belt.Option.map(b => a / b))
    ->get(0)
    ->Belt.Option.getWithDefault(0);
  };
  let initializr = 0;
  let appender = (a, b) => a + b;
};

module SpreadSheetP2 = SpreadSheet(P2Calculator);
SpreadSheetP2.checksum->Js.log;
*/
