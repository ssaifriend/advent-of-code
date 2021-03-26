let data = Node.Fs.readFileSync("input/2020/2020.2.input", #utf8)

module Password = {
  type t = {
    min: int,
    max: int,
    first: int,
    second: int,
    char: string,
    password: string,
  }

  exception InvalidInput

  let make = inputStr => {
    let passwordRe = Js.Re.fromString("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")
    let result = passwordRe->Js.Re.exec_(inputStr)
    switch result {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x)
      let first = captures[1]->Belt.Int.fromString->Belt.Option.getExn
      let second = captures[2]->Belt.Int.fromString->Belt.Option.getExn
      {
        min: first,
        max: second,
        first: first,
        second: second,
        char: captures[3],
        password: captures[4],
      }
    | None => raise(InvalidInput)
    }
  }
}

let passwordSets = Js.String.split("\n", data)->Belt.Array.map(Password.make)

// part1
let isValid = ({char, min, password, max}: Password.t) => {
  let charCount = Js.String.split("", password)->Belt.Array.keep(x => char == x)->Belt.Array.length

  min <= charCount && charCount <= max
}

Js.log(passwordSets->Belt.Array.keep(isValid)->Belt.Array.size)

// part2
let isValid2 = (set: Password.t) =>
  switch (
    String.get(set.password, set.first - 1) == String.get(set.char, 0),
    String.get(set.password, set.second - 1) == String.get(set.char, 0),
  ) {
  | (true, false)
  | (false, true) => true
  | _ => false
  }

Js.log(passwordSets->Belt.Array.keep(isValid2)->Belt.Array.size)
