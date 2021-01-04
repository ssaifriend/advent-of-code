let data = Node.Fs.readFileSync("input/2020/2020.6.input", `utf8);

let cases =
  data
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(row => {row->Js.String2.split("\n")});

// part1
let yesCount =
  cases
  ->Belt.Array.map(case => {
      case
      ->Belt.Array.map(row => row->Js.String2.split(""))
      ->Belt.Array.concatMany
      ->Belt.Set.String.fromArray
      ->Belt.Set.String.size
    })
  ->Belt.Array.reduce(0, (sum, size) => sum + size);

Js.log(yesCount);

let reducer = () => 0;
let rec intersectPersons = (persons, compareSet) =>
  switch (persons->Belt.Array.size) {
  | 0 => compareSet
  | _ =>
    let comparedSet =
      compareSet->Belt.Set.String.intersect(
        persons[0]->Belt.Set.String.fromArray,
      );
    intersectPersons(persons->Belt.Array.sliceToEnd(1), comparedSet);
  };

let allYesCount =
  cases
  ->Belt.Array.map(case => {
      let persons = case->Belt.Array.map(row => row->Js.String2.split(""));
      switch (persons->Belt.Array.size) {
      | 0 => Belt.Set.String.fromArray([||])
      | _ => persons->intersectPersons(persons[0]->Belt.Set.String.fromArray)
      };
    })
  ->Belt.Array.reduce(0, (sum, comparedSet) =>
      sum + comparedSet->Belt.Set.String.size
    );

Js.log(allYesCount);
