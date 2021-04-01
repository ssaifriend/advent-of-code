module MemoryGame = {
  type t = {
    speakMap: Belt.Map.Int.t<int>,
    turn: int,
    lastSpeakNumber: int,
  }

  module Map = Belt.Map.Int

  let rec next = (state, ~finalTurn) => {
    let speakNumber = switch state.speakMap->Map.get(state.lastSpeakNumber) {
    | Some(lastSpeakTurn) => state.turn - lastSpeakTurn
    | None => 0
    }

    let newState = {
      speakMap: state.speakMap->Map.set(state.lastSpeakNumber, state.turn),
      turn: state.turn + 1,
      lastSpeakNumber: speakNumber,
    }
    // Js.log(newState.turn->Belt.Int.toString ++ "/" ++ newState.lastSpeakNumber->Belt.Int.toString)

    finalTurn != state.turn + 1 ? newState->next(~finalTurn) : newState
  }

  let getNumber = (state, ~finalTurn) => state->next(~finalTurn)

  let make = initialNumbers => {
    let initialSpeakMapMap =
      initialNumbers->Belt.Array.mapWithIndex((index, number) => (number, index + 1))->Map.fromArray

    {
      speakMap: initialSpeakMapMap,
      turn: initialNumbers->Belt.Array.size,
      lastSpeakNumber: initialNumbers->Belt.Array.getExn(initialNumbers->Belt.Array.size - 1),
    }
  }
}

let finalTurn = 2020
let finalTurn2 = 30000000
let testCases = [
  //part1, part2
  [0, 3, 6], // 436, 175594
  [1, 3, 2], // 1, 2578
  [2, 1, 3], // 10, 3544142
  [1, 2, 3], // 27, 261214
  [2, 3, 1], // 78, 6895259
  [3, 2, 1], // 438, 18
  [3, 1, 2], // 1836, 362
  [13, 0, 10, 12, 1, 5, 8], // find!
]

let fn = (cs, fn) =>
  (cs->MemoryGame.make->MemoryGame.getNumber(~finalTurn=fn)).lastSpeakNumber->Js.log
testCases->Belt.Array.forEach(case => case->fn(finalTurn))
testCases->Belt.Array.forEach(case => case->fn(finalTurn2))
