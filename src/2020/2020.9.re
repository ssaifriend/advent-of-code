let data = Node.Fs.readFileSync("input/2020/2020.9.input", `utf8);
let numbers =
  Js.String.split("\n", data)
  ->Belt.Array.map(Belt.Float.fromString)
  ->Belt.Array.keepMap(x => x);

// part1
// form 2020.1
let rec findNumberSet = (numbers, set, ~findNumber, ~maxSetCount) =>
  if (set->Belt.List.size == maxSetCount) {
    let numberSum =
      set->Belt.List.reduce(0.0, (sum, number) => sum +. number);
    numberSum == findNumber ? Some(set) : None;
  } else {
    numbers->Belt.Array.reduceWithIndex(None, (beforeResult, number, index) => {
      switch (
        beforeResult,
        numbers
        ->Belt.Array.keepWithIndex((_, filterIndex) => index != filterIndex)
        ->findNumberSet([number, ...set], ~findNumber, ~maxSetCount),
      ) {
      | (Some(a), _) => Some(a)
      | (None, Some(a)) => Some(a)
      | (_, None) => None
      }
    });
  };

let findNotMatchingNumber = (numbers, ~preamble) => {
  Belt.Array.range(0, numbers->Belt.Array.size - preamble - 1)
  ->Belt.Array.reduce(
      None,
      (beforeResult, startAt) => {
        let findNumber = numbers[startAt + preamble];
        switch (
          beforeResult,
          numbers
          ->Belt.Array.slice(~offset=startAt, ~len=preamble)
          ->findNumberSet([], ~findNumber, ~maxSetCount=2),
        ) {
        | (Some(a), _) => Some(a)
        | (None, None) => Some(findNumber)
        | (_, Some(b)) => None
        };
      },
    );
};

Js.log(numbers->findNotMatchingNumber(~preamble=25));

//part2
let findedNumber = 1639024365.0;
let findContinueNumberSet = (numbers, ~findNumber, ~maxSetCount) => {
  Belt.Array.range(0, numbers->Belt.Array.size - maxSetCount - 1)
  ->Belt.Array.reduce(
      None,
      (beforeResult, startAt) => {
        let set =
          numbers->Belt.Array.slice(~offset=startAt, ~len=maxSetCount);
        let continueSum =
          set->Belt.Array.reduce(0.0, (sum, number) => sum +. number);

        switch (beforeResult, continueSum == findNumber) {
        | (Some(a), _) => Some(a)
        | (None, true) => Some(set)
        | (_, false) => None
        };
      },
    );
};

let findNumberEachSet =
  Belt.Array.range(2, numbers->Belt.Array.size - 1)
  ->Belt.Array.reduce(None, (beforeResult, maxSetCount) => {
      switch (
        beforeResult,
        numbers->findContinueNumberSet(
          ~findNumber=findedNumber,
          ~maxSetCount,
        ),
      ) {
      | (Some(a), _) => Some(a)
      | (None, Some(a)) => Some(a)
      | (_, None) => None
      }
    });

let result =
  switch (findNumberEachSet) {
  | Some(numbers) =>
    let numberSet =
      numbers
      ->Belt.Array.map(Belt.Float.toInt)
      ->Belt.Set.Int.fromArray;

    let max = numberSet->Belt.Set.Int.maximum->Belt.Option.getExn;
    let min = numberSet->Belt.Set.Int.minimum->Belt.Option.getExn;

    max + min;
  | None => 0
  };

Js.log(result);
