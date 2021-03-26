let part1 = inputStr =>
  inputStr
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(" "))
  ->Belt.Array.keep(row =>
    row->Belt.Array.size == row->Belt.Set.String.fromArray->Belt.Set.String.size
  )
  ->Belt.Array.size

"aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa"
->part1
->Js.log

Node.Fs.readFileAsUtf8Sync("input/2017/2017.4.input")->part1->Js.log

let part2 = inputStr =>
  inputStr
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row
    ->Js.String2.split(" ")
    ->Belt.Array.map(str =>
      str->Js.String2.split("")->Belt.SortArray.String.stableSort->Js.Array2.joinWith("")
    )
  )
  ->Belt.Array.keep(row =>
    row->Belt.Array.size == row->Belt.Set.String.fromArray->Belt.Set.String.size
  )
  ->Belt.Array.size

"abcde fghij
abcde xyz ecdab
a ab abc abd abf abj
iiii oiii ooii oooi oooo
oiii ioii iioi iiio"
->part2
->Js.log

Node.Fs.readFileAsUtf8Sync("input/2017/2017.4.input")->part2->Js.log
