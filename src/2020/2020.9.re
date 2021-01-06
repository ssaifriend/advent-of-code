let data = Node.Fs.readFileSync("input/2020/2020.9.input", `utf8);
let numbers =
  data->Js.String2.split("\n")->Belt.Array.keepMap(Belt.Float.fromString);

// 더 쉽게 만들어보기

// part1
// form 2020.1
let rec findNumberSet = (numberSets, numbers, ~findNumber) => {
  switch (numberSets->Belt.List.head) {
  | Some(numberSet) =>
    let numberSum = numberSet->Belt.List.reduce(0.0, (sum, index) => sum +. numbers[index]);
    let uniqueSize = numberSet->Belt.List.toArray->Belt.Set.Int.fromArray->Belt.Set.Int.size;
    let setSize = numberSet->Belt.List.size;
    if (uniqueSize == setSize && numberSum == findNumber) {
      Some(numberSet);
    } else {
      switch (numberSets->Belt.List.drop(1)) {
      | Some(nextNumberSets) => nextNumberSets->findNumberSet(numbers, ~findNumber)
      | None => None
      };
    }
  | None => None
  };
};

let rec makeNumberIndexSets = (indexes, sets, ~setCount) =>
  if (sets->Belt.List.size == 0) {
    let sets = indexes->Belt.List.map(index => [index])
    indexes->makeNumberIndexSets(sets, ~setCount=setCount - 1)
  } else {
    let sets = indexes->Belt.List.map(index => {
      sets->Belt.List.map(set => [index, ...set])
    })
    let newSets = sets->Belt.List.toArray->Belt.List.concatMany;

    setCount == 1
      ? newSets : indexes->makeNumberIndexSets(newSets, ~setCount=setCount - 1);
  };


let rec findNotMatchingNumber = (numbers, ~preamble) => {
  let findNumber = numbers[preamble];
  let numberArray = numbers->Belt.Array.slice(~offset=0, ~len=preamble)
  let twoNumberIndexSets =
    Belt.Array.range(0, numberArray->Belt.Array.size - 1)
    ->Belt.List.fromArray
    ->makeNumberIndexSets([], ~setCount=2);
  let result = twoNumberIndexSets->findNumberSet(numberArray, ~findNumber)

  switch (result) {
  | None => Some(findNumber)
  | Some(_) when numbers->Belt.Array.size > 1 => 
    let nextNumbers = Belt.Array.sliceToEnd(numbers, 1);
    nextNumbers->findNotMatchingNumber(~preamble)
  | Some(_) => None
  };
};

Js.log(numbers->findNotMatchingNumber(~preamble=25));

//part2
let findedNumber = 1639024365.0;
let rec findContinueNumberSet = (numbers, startAt, ~findNumber, ~maxContinueNumberCount) => {
  let continueNumbers = numbers->Belt.Array.slice(~offset=startAt, ~len=maxContinueNumberCount);
  let continueSum = continueNumbers->Belt.Array.reduce(0.0, (sum, number) => sum +. number);

  if (continueNumbers->Belt.Array.size != maxContinueNumberCount) {
    None
  } else if (continueSum == findNumber) {
    Some(continueNumbers);
  } else {
    numbers->findContinueNumberSet(startAt + 1, ~findNumber, ~maxContinueNumberCount);
  } 
};

let rec findContinueNumber = (numbers, ~findNumber, ~maxContinueNumberCount) => {
  switch (numbers->findContinueNumberSet(0, ~findNumber, ~maxContinueNumberCount)) {
    | Some(a) => Some(a)
    | None => numbers->findContinueNumber(~findNumber, ~maxContinueNumberCount=maxContinueNumberCount + 1)
  }
}

let result =
  switch (numbers->findContinueNumber(~findNumber = findedNumber, ~maxContinueNumberCount = 2)) {
  | Some(numbers) =>
    let numberSet =
      numbers->Belt.Array.map(Belt.Float.toInt)->Belt.Set.Int.fromArray;

    let max = numberSet->Belt.Set.Int.maximum->Belt.Option.getExn;
    let min = numberSet->Belt.Set.Int.minimum->Belt.Option.getExn;

    max + min;
  | None => 0
  };

Js.log(result);