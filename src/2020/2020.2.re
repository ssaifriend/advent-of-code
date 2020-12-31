let data = Node.Fs.readFileSync("input/2020/2020.2.input", `utf8);

module Password = {
  type t = {
    min: int,
    max: int,
    first: int,
    second: int,
    char: string,
    password: string,
  };

  let make = (inputStr): option(t) => {
    let passwordRe = Js.Re.fromString("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)");
    let result = passwordRe->Js.Re.exec_(inputStr);
    switch (result) {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)
        ->Belt.Array.map(Js.Nullable.toOption)
        ->Belt.Array.keepMap(x => x);
      let first = captures[1]->Belt.Int.fromString->Belt.Option.getExn;
      let second = captures[2]->Belt.Int.fromString->Belt.Option.getExn;
      Some({
        min: first,
        max: second,
        first,
        second,
        char: captures[3],
        password: captures[4],
      });
    | None => None
    };
  };
};

let passwordSets =
  Js.String.split("\n", data)
  ->Belt.Array.keepMap(Password.make)

// part1
let validPasswordSetCount =
  passwordSets
  ->Belt.Array.keep(({char, min, password, max}) => {
      let charCount =
        Js.String.split("", password)
        ->Belt.Array.keep(x => char == x)
        ->Belt.Array.length;

      min <= charCount && charCount <= max;
    })
  ->Belt.Array.length;

Js.log(validPasswordSetCount);

// part2
let validPasswordSetCount2 =
  passwordSets
  ->Belt.Array.keep(set => {
      (set.password.[set.first - 1] == set.char.[0]
      && set.password.[set.second - 1] != set.char.[0])
      || (set.password.[set.first - 1] != set.char.[0]
      && set.password.[set.second - 1] == set.char.[0])
    })
  ->Belt.Array.length;

Js.log(validPasswordSetCount2);
