let data = Node.Fs.readFileSync("input/2020/2020.10.input", `utf8);
let jolts =
  Js.String.split("\n", data)
  ->Belt.Array.map(Belt.Int.fromString)
  ->Belt.Array.keepMap(x => x)
  ->Belt.Set.Int.fromArray;

// part1
type summary = {
  j1: int,
  j2: int,
  j3: int,
  beforeJolts: int,
};

exception InvalidJolts;

let maxJolts = jolts->Belt.Set.Int.maximum->Belt.Option.getExn;

let initialResult = {j1: 0, j2: 0, j3: 0, beforeJolts:0};
let result = jolts
->Belt.Set.Int.add(maxJolts + 3) // 마지막에 항상 +3?..
->Belt.Set.Int.reduce(initialResult, (result, jolt) => {
  switch (jolt - result.beforeJolts) {
    | 1 => {...result, j1: result.j1 + 1, beforeJolts: jolt}
    | 2 => {...result, j2: result.j2 + 1, beforeJolts: jolt}
    | 3 => {...result, j3: result.j3 + 1, beforeJolts: jolt}
    | _ => raise(InvalidJolts)
  }
});

Js.log(result);
Js.log(result.j1 * result.j3);
