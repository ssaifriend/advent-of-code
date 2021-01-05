let data = Node.Fs.readFileSync("input/2020/2020.1.input", `utf8);
let years =
  Js.String.split("\n", data)
  ->Belt.Array.map(Belt.Int.fromString)
  ->Belt.Array.keepMap(x => x);

// part1
let currentYear = 2020;
let rec findCurrentYearSet = (years, findYear, set, ~setCount) =>
  if (set->Belt.List.size == setCount) {
    let yearSum = set->Belt.List.reduce(0, (sum, year) => sum + year);
    yearSum == findYear ? Some(set) : None;
  } else {
    years->Belt.Array.reduce(None, (beforeResult, year) => {
      switch (
        beforeResult,
        years->findCurrentYearSet(findYear, [year, ...set], ~setCount),
      ) {
      | (Some(a), _) => Some(a)
      | (None, Some(a)) => Some(a)
      | (None, None) => None
      }
    });
  };

let sumTwoYearSet = years->findCurrentYearSet(currentYear, [], ~setCount=2);
Js.log(
  switch (sumTwoYearSet) {
  | Some(a) => a->Belt.List.reduce(1, (sum, year) => sum * year)
  | None => 0
  },
);

// part2
let sumThreeYearSet = years->findCurrentYearSet(currentYear, [], ~setCount=3);
Js.log(
  switch (sumThreeYearSet) {
  | Some(a) => a->Belt.List.reduce(1, (sum, year) => sum * year)
  | None => 0
  },
);
