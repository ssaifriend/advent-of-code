// part1
let getDifference = inputStr =>
  inputStr
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
      row
      ->Js.String2.split("\t")
      ->Belt.Array.keepMap(Belt.Int.fromString)
      ->Belt.Set.Int.fromArray
    )
  ->Belt.Array.map(row =>
      row->Belt.Set.Int.maximum->Belt.Option.getExn
      - row->Belt.Set.Int.minimum->Belt.Option.getExn
    )
  ->Belt.Array.reduce(0, (+));

"5	1	9	5
7	5	3
2	4	6	8"->getDifference->Js.log;

Node.Fs.readFileAsUtf8Sync("input/2017/2017.2.input")->getDifference->Js.log;

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
      row->Belt.Array.reduce([], (tuples1, number1) => {
        row->Belt.Array.reduce(tuples1, (tuples2, number2) => {
          [(number1, number2), ...tuples2]
        })
      })
    )
  ->Belt.Array.map(row =>
      row
      ->Belt.List.keep(((number1, number2)) => number1 > number2)
      ->Belt.List.keep(((number1, number2)) => number1 mod number2 == 0)
      ->Belt.List.map(((number1, number2)) => number1 / number2)
      ->Belt.List.headExn
    )
  ->Belt.Array.reduce(0, (+));

"5	9	2	8
9	4	7	3
3	8	6	5"->getDivide->Js.log;

Node.Fs.readFileAsUtf8Sync("input/2017/2017.2.input")->getDivide->Js.log;
