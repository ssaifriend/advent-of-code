// part 1
Node.Fs.readFileAsUtf8Sync("input/2015/2015.1.input")
->Js.String2.split("")
->Belt.Array.map(input => input == "(" ? 1 : -1)
->Belt.Array.reduce(0, \"+")
->Js.log

// part 2
let rec findBasementPosition = (input, index, sum) => {
  let add = input[index] == "(" ? 1 : -1

  sum + add < 0 ? index + 1 : input->findBasementPosition(index + 1, sum + add)
}

let input =
  Node.Fs.readFileAsUtf8Sync("input/2015/2015.1.input")
  ->Js.String2.split("")
  ->findBasementPosition(0, 0)
  ->Js.log
