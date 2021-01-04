let data = Node.Fs.readFileSync("input/2020/2020.7.input", `utf8);

module Bag = {
  type t = {
    name: string,
    count: int,
    contains: list(t),
  };

  // from 2020.4
  let rec extract = (str, re, options) => {
    switch (re->Js.Re.exec_(str)) {
    | Some(r) =>
      let captures =
        r
        ->Js.Re.captures
        ->Belt.Array.map(Js.Nullable.toOption)
        ->Belt.Array.keepMap(x => x);

      extract(str, re, [captures, ...options]);
    | None => options
    };
  };

  let make = inputStr => {
    let str = inputStr->Js.String2.split(" contain ");

    let bag = {name: "", count: 0, contains: []};

    let parentRe = Js.Re.fromString("([a-zA-z ]+) bags");
    let result = parentRe->Js.Re.exec_(str[0]);
    let parentBag =
      switch (result) {
      | Some(r) =>
        let captures =
          Js.Re.captures(r)
          ->Belt.Array.map(Js.Nullable.toOption)
          ->Belt.Array.keepMap(x => x);
        {...bag, name: captures[1]};
      | None => raise(Not_found)
      };

    let containRe =
      Js.Re.fromStringWithFlags(
        "([0-9]+) ([a-zA-z ]+) (bags|bag)",
        ~flags="g",
      );
    let contains =
      extract(str[1], containRe, [])
      ->Belt.List.map(bag => {
          {
            name: bag[2],
            count: bag[1]->Belt.Int.fromString->Belt.Option.getExn,
            contains: [],
          }
        });

    {...parentBag, contains};
  };
};

let bags = data->Js.String2.split("\n")->Belt.Array.map(Bag.make);

// part1
let rec findContainBags = (bag: Bag.t, allBags, findBagName) => {
  // Js.log(bag.name);
  switch (bag.name) {
  | _ when bag.name == findBagName => Some(bag)
  | _ when bag.contains->Belt.List.size > 0 =>
    bag.contains
    ->Belt.List.reduce(None, (findBag, containBag) => {
        switch (findBag) {
        | None =>
          switch (
            allBags->Belt.Array.getBy((b: Bag.t) => b.name == containBag.name)
          ) {
          | Some(b) => b->findContainBags(allBags, findBagName)
          | None => None
          }
        | Some(a) => Some(a)
        }
      })
  | _ => None
  };
};

let findBagName = "shiny gold";
let matchBagCount =
  bags
  ->Belt.Array.map(bag => {
      let filteredBag = {...bag, name: ""};
      switch (filteredBag->findContainBags(bags, findBagName)) {
      | Some(_) => 1
      | None => 0
      };
    })
  ->Belt.Array.reduce(0, (sum, count) => sum + count);

Js.log(matchBagCount);

// part2
// let findBagName = "shiny gold"; // part1 에서 정의
let rec summerizer = (bag: Bag.t, allBags) => {
  switch (allBags->Belt.Array.getBy((b: Bag.t) => b.name == bag.name)) {
  | Some(findBag) when findBag.contains->Belt.List.size > 0 =>
    let containBagCount =
      findBag.contains
      ->Belt.List.reduce(0, (sum, containBag) =>
          sum + containBag->summerizer(allBags)
        );

    (containBagCount + 1) * bag.count
  | Some(_) => bag.count
  | None => 0
  };
};

let allBagCount =
  switch (bags->Belt.Array.getBy(b => b.name == findBagName)) {
  | Some(a) =>
    a.contains
    ->Belt.List.reduce(0, (sum, containBag) =>
        sum + containBag->summerizer(bags)
      );
  | None => 0
  };

Js.log(allBagCount);
