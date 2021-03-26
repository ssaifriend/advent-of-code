let data = Node.Fs.readFileSync("input/2020/2020.6.input", #utf8)

let cases =
  data
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(row => row->Js.String2.split("\n")->Belt.List.fromArray)

module Answers = Belt.Set.String

/*
 // part1
 let yesCount =
   cases
   ->Belt.Array.map(case => {
       case
       ->Belt.Array.map(row => row->Js.String2.split(""))
       ->Belt.Array.concatMany
       ->Answers.fromArray
       ->Answers.size
     })
   ->Belt.Array.reduce(0, (sum, size) => sum + size);

 Js.log(yesCount);

 // part2
 let rec intersectPersons = (persons, compareSet) =>
   switch (persons->Belt.Array.size) {
   | 0 => compareSet
   | _ =>
     let comparedSet =
       compareSet->Answers.intersect(persons[0]->Answers.fromArray);
     intersectPersons(persons->Belt.Array.sliceToEnd(1), comparedSet);
   };

 let allYesCount =
   cases
   ->Belt.Array.map(case => {
       let persons = case->Belt.Array.map(row => row->Js.String2.split(""));
       switch (persons->Belt.Array.size) {
       | 0 => 0
       | _ =>
         persons->intersectPersons(persons[0]->Answers.fromArray)->Answers.size
       };
     })
   ->Belt.Array.reduce(0, (sum, size) => sum + size);

 Js.log(allYesCount);
 */

// part1, part2 통합
let rec findPersons = (persons, compareSet, compareFunc) => {
  let comparedSet = switch persons->Belt.List.head {
  | Some(person) => compareSet->compareFunc(person->Answers.fromArray)
  | None => compareSet
  }

  switch persons->Belt.List.tail {
  | Some(persons) => persons->findPersons(comparedSet, compareFunc)
  | None => comparedSet
  }
}

let getPersonCount = (case, compareFunc) => {
  let persons = case->Belt.List.map(row => row->Js.String2.split(""))
  let initial = persons->Belt.List.headExn->Answers.fromArray
  persons->findPersons(initial, compareFunc)->Answers.size
}

let someYesCount =
  cases->Belt.Array.map(case => case->getPersonCount(Answers.union))->Belt.Array.reduce(0, \"+")

Js.log(someYesCount)

let allYesCount =
  cases->Belt.Array.map(case => case->getPersonCount(Answers.intersect))->Belt.Array.reduce(0, \"+")

Js.log(allYesCount)
