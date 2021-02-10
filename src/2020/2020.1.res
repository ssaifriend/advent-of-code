let data = Node.Fs.readFileSync("input/2020/2020.1.input", #utf8)
let years =
  Js.String.split("\n", data)->Belt.Array.map(Belt.Int.fromString)->Belt.Array.keepMap(x => x)

// part1
let currentYear = 2020
let rec findYearSet = (yearSets, years, ~findYear) =>
  switch yearSets->Belt.List.head {
  | Some(yearSet) =>
    let yearSum = yearSet->Belt.List.reduce(0, (sum, index) => sum + years[index])
    let uniqueSize = yearSet->Belt.List.toArray->Belt.Set.Int.fromArray->Belt.Set.Int.size
    let setSize = yearSet->Belt.List.size
    if uniqueSize == setSize && yearSum == findYear {
      Some(yearSet)
    } else {
      switch yearSets->Belt.List.drop(1) {
      | Some(nextYearSets) => nextYearSets->findYearSet(years, ~findYear)
      | None => None
      }
    }
  | None => None
  }

let rec makeYearIndexSets = (indexes, sets, ~setCount) =>
  if sets->Belt.List.size == 0 {
    let sets = indexes->Belt.List.map(index => list{index})
    indexes->makeYearIndexSets(sets, ~setCount=setCount - 1)
  } else {
    let sets = indexes->Belt.List.map(index => sets->Belt.List.map(set => list{index, ...set}))
    let newSets = sets->Belt.List.toArray->Belt.List.concatMany

    setCount == 1 ? newSets : indexes->makeYearIndexSets(newSets, ~setCount=setCount - 1)
  }

let twoYearIndexSets =
  Belt.Array.range(0, years->Belt.Array.size - 1)
  ->Belt.List.fromArray
  ->makeYearIndexSets(list{}, ~setCount=2)

let sumTwoYearIndexSet = twoYearIndexSets->findYearSet(years, ~findYear=currentYear)
Js.log(
  switch sumTwoYearIndexSet {
  | Some(a) => a->Belt.List.reduce(1, (sum, index) => sum * years[index])
  | None => 0
  },
)

// part2
let threeYearIndexSets =
  Belt.Array.range(0, years->Belt.Array.size - 1)
  ->Belt.List.fromArray
  ->makeYearIndexSets(list{}, ~setCount=3)
let sumThreeYearIndexSet = threeYearIndexSets->findYearSet(years, ~findYear=currentYear)
Js.log(
  switch sumThreeYearIndexSet {
  | Some(a) => a->Belt.List.reduce(1, (sum, index) => sum * years[index])
  | None => 0
  },
)
