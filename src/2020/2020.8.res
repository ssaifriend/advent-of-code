let data = Node.Fs.readFileSync("input/2020/2020.8.input", #utf8)

module Op = {
  type t =
    | Acc(int)
    | Jmp(int)
    | Nop(int)

  let make = inputStr => {
    let re = Js.Re.fromString("(acc|jmp|nop) (\+|\-)([0-9]+)")
    let result = re->Js.Re.exec_(inputStr)
    switch result {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x)

      let value =
        (captures[2] == "-" ? -1 : 1) * captures[3]->Belt.Int.fromString->Belt.Option.getExn

      switch captures[1] {
      | "acc" => Acc(value)
      | "jmp" => Jmp(value)
      | "nop" => Nop(value)
      | _ => raise(Not_found)
      }
    | None => raise(Not_found)
    }
  }
}

let codes = data->Js.String2.split("\n")->Belt.Array.map(Op.make)

// part1
type loopState =
  | Loop
  | Inf
  | Complete

type summary = {
  sum: int,
  currentIndex: int,
  indexes: list<int>,
  loopState: loopState,
}

let initialState = {sum: 0, currentIndex: 0, indexes: list{}, loopState: Loop}
let rec walk = (codes: array<Op.t>, s): summary => {
  let result = switch codes->Belt.Array.get(s.currentIndex) {
  | None => {...s, loopState: Complete}

  | Some(Nop(_)) => {
      ...s,
      currentIndex: s.currentIndex + 1,
      indexes: list{s.currentIndex, ...s.indexes},
    }
  | Some(Acc(value)) => {
      ...s,
      sum: s.sum + value,
      currentIndex: s.currentIndex + 1,
      indexes: list{s.currentIndex, ...s.indexes},
    }
  | Some(Jmp(value)) => {
      ...s,
      currentIndex: s.currentIndex + value,
      indexes: list{s.currentIndex, ...s.indexes},
    }
  }

  if result.loopState == Complete {
    result
  } else if s.indexes->Belt.List.some(index => index == s.currentIndex) {
    {
      ...s,
      loopState: Inf,
    }
  } else {
    codes->walk(result)
  }
}

Js.log((codes->walk(initialState)).sum)

// part2

let changeCode = (codes, changeIndex) => {
  open Op
  codes->Belt.Array.mapWithIndex((index, code) => {
    let isChangeIndex = index == changeIndex
    switch (isChangeIndex, code) {
    | (true, Jmp(value)) => Nop(value)
    | (true, Nop(value)) => Jmp(value)
    | (_, value) => value
    }
  })
}

exception InvalidLoopState

let rec findNotInfSummary = (codes, index) => {
  let changedCodes = codes->changeCode(index)
  let s = changedCodes->walk(initialState)

  switch s.loopState {
  | Inf => codes->findNotInfSummary(index - 1)
  | Complete => s.sum
  | Loop => raise(InvalidLoopState)
  }
}

Js.log(codes->findNotInfSummary(codes->Belt.Array.size - 1))
