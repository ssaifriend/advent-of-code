let data = Node.Fs.readFileAsUtf8Sync("input/2018/2018.1.input")

let run = (data, ~op, ()) =>
  data->Js.String2.split("\n")->Belt.Array.keepMap(Belt.Int.fromString)->op->Js.log

let sum = Belt.Array.reduce(_, 0, \"+")

let twiceSearch = ints => {
  let size = ints->Belt.Array.size
  let rec search = (ints, index, current, results) => {
    let current = current + ints->Belt.Array.getExn(mod(index, size))

    results->Belt.List.some(result => result == current)
      ? current
      : ints->search(index + 1, current, list{current, ...results})
  }

  ints->search(0, 0, list{})
}

data->run(~op=sum, ())

// sample
// "+3
// +3
// +4
// -2
// -4"->run(~op=twiceSearch, ())

data->run(~op=twiceSearch, ())
