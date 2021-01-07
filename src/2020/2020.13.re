let data = Node.Fs.readFileSync("input/2020/2020.13.input", `utf8);
let rows = data->Js.String2.split("\n");

let time = rows[0]->Belt.Int.fromString->Belt.Option.getExn;
let busIds = rows[1]->Js.String2.split(",");

// part1
let rec findDepartTime = (busIds, time) => {
  let remainTimeEachBus =
    busIds
    ->Belt.Array.map(busId => (busId, time mod busId))
    ->Belt.Array.keep(((_, remainTime)) => remainTime == 0);

  if (remainTimeEachBus->Belt.Array.size == 0) {
    busIds->findDepartTime(time + 1);
  } else {
    let (busId, _) = remainTimeEachBus[0];

    (busId, time);
  };
};

let departBusIds =
  busIds
  ->Belt.Array.keep(busId => busId != "x")
  ->Belt.Array.keepMap(Belt.Int.fromString);
let (busId, departTime) = departBusIds->findDepartTime(time);
Js.log(busId * (departTime - time));

// part2
let rec isValidContestTime = (busIds, time) => {
  switch (busIds->Belt.Array.get(0)) {
  | None => true
  | Some(busId) =>
    if (busId != Int64.zero && Int64.rem(time, busId) != Int64.zero) {
      false;
    } else {
      busIds
      ->Belt.Array.sliceToEnd(1)
      ->isValidContestTime(Int64.add(time, Int64.one));
    }
  };
};

let rec getContentTime = (busIds, startTime, ~maxBusId) => {
  let isAllValidContestTime = busIds->isValidContestTime(startTime);

  isAllValidContestTime
    ? startTime
    : busIds->getContentTime(Int64.add(startTime, maxBusId), ~maxBusId);
};

let busIdSets =
  rows
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(row => {
      row
      ->Js.String2.split(",")
      ->Belt.Array.map(busId =>
          busId == "x" ? 0 : busId->Belt.Int.fromString->Belt.Option.getExn
        )
    })
  ->Belt.Array.forEach(busIds => {
      let maxBusId =
        busIds
        ->Belt.Set.Int.fromArray
        ->Belt.Set.Int.maximum
        ->Belt.Option.getExn;
      let maxBusIdIndex =
        busIds->Belt.Array.reduceWithIndex(0, (maxIndex, busId, index) =>
          maxBusId == busId ? index : maxIndex
        );

      let busIds = busIds->Belt.Array.map(Int64.of_int);
      // 모두 도는 방법
      /*
      Js.log(
        busIds->getContentTime(
          Int64.of_int(maxBusId - maxBusIdIndex),
          ~maxBusId=maxBusId->Int64.of_int,
        ),
      );
      */

      // 앞부분 스킵
      /*
      let startTime = Int64.of_string("99999999984118");
      Js.log(
        busIds->getContentTime(
          startTime,
          ~maxBusId=Int64.mul(maxBusId->Int64.of_int, busIds[0]),
        ),
      );
      */
    });
