let data = Node.Fs.readFileAsUtf8Sync("input/2020/2020.7.input")

module Bag = {
  type rec t = {
    name: string,
    count: int,
    contains: list<t>,
  }

  // from 2020.4
  let make = inputStr => {
    let str = inputStr->Js.String2.split(" contain ")

    let bag = {name: "", count: 0, contains: list{}}

    let parentRe = Js.Re.fromString("([a-zA-z ]+) bags")
    let result = parentRe->Js.Re.exec_(str[0])
    let parentBag = switch result {
    | Some(r) =>
      let captures = Js.Re.captures(r)->Belt.Array.keepMap(Js.Nullable.toOption)
      {...bag, name: captures[1]}
    | None => raise(Not_found)
    }

    let containRe = Js.Re.fromStringWithFlags("([0-9]+) ([a-zA-z ]+) (bags|bag)", ~flags="g")
    let contains =
      str[1]
      ->Util.Re.extract(containRe, list{})
      ->Belt.List.map(bag => {
        name: bag[2],
        count: bag[1]->Util.Int.fromStringExn,
        contains: list{},
      })

    {...parentBag, contains: contains}
  }
}

let bags = data->Js.String2.split("\n")->Belt.Array.map(Bag.make)

// part1
let rec findContainBags = (contains, allBags, findBagName) =>
  contains->Belt.List.reduce(false, (exists, containBag: Bag.t) =>
    exists ||
    (containBag.name == findBagName ||
    switch allBags->Belt.Array.getBy((bag: Bag.t) => bag.name == containBag.name) {
    | None => false
    | Some(containBags) => containBags.contains->findContainBags(allBags, findBagName)
    })
  )

let findBagName = "shiny gold"
bags
->Belt.Array.keep(bag => bag.contains->findContainBags(bags, findBagName))
->Belt.Array.size
->Js.log

// part2
// let findBagName = "shiny gold"; // part1 에서 정의
let rec summerizer = (allBags, bagName) =>
  switch allBags->Belt.Array.getBy((b: Bag.t) => b.name == bagName) {
  | None => 0
  | Some(findBag) =>
    findBag.contains->Belt.List.reduce(1, (sum, containBag) =>
      sum + allBags->summerizer(containBag.name) * containBag.count
    )
  }

let allBagCount = bags->summerizer(findBagName) - 1 // 자기 자신이 포함되어 있기에 빼야 함

allBagCount->Js.log
