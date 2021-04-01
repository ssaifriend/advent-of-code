let data = Node.Fs.readFileAsUtf8Sync("input/2020/2020.6.input")

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
  switch persons {
  | list{person, ...tail} =>
    let comparedSet = compareSet->compareFunc(person->Answers.fromArray)
    tail->findPersons(comparedSet, compareFunc)
  | _ => compareSet
  }
}

let getPersonCount = (case, compareFunc) => {
  let persons = case->Belt.List.map(row => row->Js.String2.split(""))
  let initial = persons->Belt.List.headExn->Answers.fromArray
  persons->findPersons(initial, compareFunc)->Answers.size
}

let run = (cs, f) => cs->Belt.Array.map(case => case->getPersonCount(f))->Belt.Array.reduce(0, \"+")

cases->run(Answers.union)->Js.log
cases->run(Answers.intersect)->Js.log
