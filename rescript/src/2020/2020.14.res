module MaskBit = {
  type t =
    | Zero(int)
    | One(int)
    | None(int)

  let makeOneMaskBit = (index, maskLen) => {
    let actualBit = maskLen - index - 1
    Int64.one->Int64.shift_left(actualBit)
  }

  let makeZeroMaskBit = (index, maskLen) => index->makeOneMaskBit(maskLen)->Int64.lognot
}

module type MaskOperation = {
  type t

  let converter: array<MaskBit.t> => t
}

module Mask = (Converter: MaskOperation) => {
  type t = Converter.t

  module List = Belt.List

  let make = inputStr =>
    (inputStr->Js.String2.split(" = "))[1]
    ->Js.String2.split("")
    ->Belt.Array.mapWithIndex((index, bit) =>
      switch bit {
      | "0" => MaskBit.Zero(index)
      | "1" => One(index)
      | _ => None(index)
      }
    )
    ->Converter.converter
}

module MemoryOperation = {
  type t = {
    address: int,
    value: Int64.t,
  }

  exception InvalidInput

  let make = inputStr => {
    let re = Js.Re.fromString("mem\[([0-9]+)\] = ([0-9]+)")
    let result = re->Js.Re.exec_(inputStr)
    switch result {
    | Some(r) =>
      let captures = Js.Re.captures(r)->Belt.Array.keepMap(Js.Nullable.toOption)
      {
        address: captures[1]->Belt.Int.fromString->Belt.Option.getExn,
        value: captures[2]->Int64.of_string,
      }
    | None => raise(InvalidInput)
    }
  }
}

module Operation = (Converter: MaskOperation) => {
  type t =
    | Mask(Converter.t)
    | MemoryOperation(MemoryOperation.t)

  exception InvalidOperations

  module MaskConveter = Mask(Converter)
  module List = Belt.List

  let make = inputStrs =>
    inputStrs
    ->Belt.Array.map(inputStr =>
      switch inputStr->Js.String2.slice(~from=0, ~to_=4) {
      | "mask" => Mask(inputStr->MaskConveter.make)
      | "mem[" => MemoryOperation(inputStr->MemoryOperation.make)
      | _ => raise(InvalidOperations)
      }
    )
    ->Belt.List.fromArray
}

// part1
module ValueMaskConverter = {
  type t = {
    one: list<Int64.t>,
    zero: list<Int64.t>,
  }

  let maskLen = 36 // 미리 정의된 상수

  let converter = maskInput => {
    let masks = {one: list{}, zero: list{}}

    maskInput->Belt.Array.reduce(masks, (masks, mask) =>
      switch mask {
      | MaskBit.Zero(index) => {
          ...masks,
          zero: list{index->MaskBit.makeZeroMaskBit(maskLen), ...masks.zero},
        }
      | One(index) => {
          ...masks,
          one: list{index->MaskBit.makeOneMaskBit(maskLen), ...masks.one},
        }
      | None(_) => masks
      }
    )
  }
}

module ValueOperation = Operation(ValueMaskConverter)

module Memory = {
  type t = Belt.Map.Int.t<Int64.t>

  module Map = Belt.Map.Int

  let print = memory =>
    memory->Map.forEach((key, value) =>
      Js.log(key->Belt.Int.toString ++ (" / " ++ value->Int64.to_string))
    )
}

module DockingMemory = {
  type t = {
    memory: Memory.t,
    currentMask: ValueOperation.MaskConveter.t,
  }

  let rec operation = (state, operations) => {
    let makeValue = (op: MemoryOperation.t, mask: ValueOperation.MaskConveter.t) => {
      let zeroProcessedValue =
        mask.zero->ValueOperation.MaskConveter.List.reduce(op.value, (value, maskValue) =>
          value->Int64.logand(maskValue)
        )

      mask.one->ValueOperation.MaskConveter.List.reduce(zeroProcessedValue, (value, maskValue) =>
        value->Int64.logor(maskValue)
      )
    }

    let result = switch operations->ValueOperation.List.head {
    | Some(ValueOperation.Mask(m)) => {...state, currentMask: m}
    | Some(MemoryOperation(op)) =>
      let value = op->makeValue(state.currentMask)

      {...state, memory: state.memory->Memory.Map.set(op.address, value)}
    | None => state
    }

    // result.memory->Memory.print;

    switch operations->ValueOperation.List.drop(1) {
    | Some(newOperations) => result->operation(newOperations)
    | None => result
    }
  }

  let start = (operations: list<ValueOperation.t>) => {
    let initialState = {
      memory: Memory.Map.fromArray([]),
      currentMask: {
        one: list{},
        zero: list{},
      },
    }

    initialState->operation(operations)
  }

  let memorySum = state =>
    state.memory->Memory.Map.reduce(Int64.zero, (sum, _, value) => sum->Int64.add(value))
}

let data = Node.Fs.readFileSync("input/2020/2020.14.sample", #utf8)
let rows = data->Js.String2.split("\n")
let operations = rows->ValueOperation.make
let state = operations->DockingMemory.start
// state.memory->Memory.print;

let sum = state->DockingMemory.memorySum

Js.log(sum->Int64.to_string)

// part2
module AddressMaskConverter = {
  type t = {
    one: list<Int64.t>,
    x: list<int>,
  }

  let maskLen = 36 // 미리 정의된 상수

  let converter = maskInput => {
    let masks = {one: list{}, x: list{}}

    maskInput->Belt.Array.reduce(masks, (masks, mask) =>
      switch mask {
      | MaskBit.None(index) => {...masks, x: list{index, ...masks.x}}
      | One(index) => {
          ...masks,
          one: list{index->MaskBit.makeOneMaskBit(maskLen), ...masks.one},
        }
      | Zero(_) => masks
      }
    )
  }
}

module AddressOperation = Operation(AddressMaskConverter)

module Int64Cmp = Belt.Id.MakeComparable({
  type t = Int64.t
  let cmp = (a, b) => Int64.compare(a, b)
})

module MemoryV2 = {
  type t = Belt.Map.t<Int64.t, Int64.t, Int64Cmp.identity>

  module Map = Belt.Map

  let print = memory =>
    memory->Map.forEach((key, value) =>
      Js.log(key->Int64.to_string ++ (" / " ++ value->Int64.to_string))
    )
}

module DockingMemoryV2 = {
  type t = {
    memory: MemoryV2.t,
    currentMask: AddressOperation.MaskConveter.t,
  }

  let rec makeIndexSets = (indexes, sets) =>
    if sets->Belt.List.size == 0 {
      let newSets = list{list{(indexes[0], 0)}, list{(indexes[0], 1)}}
      indexes->Belt.Array.sliceToEnd(1)->makeIndexSets(newSets)
    } else {
      let appendSet = list{(indexes[0], 0), (indexes[0], 1)}
      let newSets =
        sets
        ->Belt.List.map(set => appendSet->Belt.List.map(indexSet => list{indexSet, ...set}))
        ->Belt.List.toArray
        ->Belt.List.concatMany

      indexes->Belt.Array.size == 1
        ? newSets
        : indexes->Belt.Array.sliceToEnd(1)->makeIndexSets(newSets)
    }

  let rec operation = (state, operations) => {
    let makeAddresses = (op: MemoryOperation.t, mask: AddressOperation.MaskConveter.t) => {
      let oneProcessedAddress =
        mask.one->AddressOperation.MaskConveter.List.reduce(Int64.of_int(op.address), (
          address,
          maskValue,
        ) => address->Int64.logor(maskValue))

      mask.x
      ->AddressOperation.MaskConveter.List.toArray
      ->makeIndexSets(list{})
      ->Belt.List.map(sets =>
        sets->Belt.List.reduce(oneProcessedAddress, (address, (index, bit)) => {
          let maskLen = AddressMaskConverter.maskLen
          bit == 1
            ? address->Int64.logor(index->MaskBit.makeOneMaskBit(maskLen))
            : address->Int64.logand(index->MaskBit.makeZeroMaskBit(maskLen))
        })
      )
      ->Belt.List.toArray
    }

    let result = switch operations->AddressOperation.List.head {
    | Some(AddressOperation.Mask(m)) => {...state, currentMask: m}
    | Some(MemoryOperation(op)) =>
      op
      ->makeAddresses(state.currentMask)
      ->Belt.Array.reduce(state, (oldState, address) => {
        ...oldState,
        memory: oldState.memory->MemoryV2.Map.set(address, op.value),
      })
    | None => state
    }

    // result.memory->MemoryV2.print;
    switch operations->AddressOperation.List.drop(1) {
    | Some(newOperations) => result->operation(newOperations)
    | None => result
    }
  }

  let start = (operations: list<AddressOperation.t>) => {
    let initialState = {
      memory: Belt.Map.make(~id=module(Int64Cmp)),
      currentMask: {
        one: list{},
        x: list{},
      },
    }

    initialState->operation(operations)
  }

  let memorySum = state =>
    state.memory->MemoryV2.Map.reduce(Int64.zero, (sum, _, value) => sum->Int64.add(value))
}

let data2 = Node.Fs.readFileSync("input/2020/2020.14.2.sample", #utf8)
let data2 = Node.Fs.readFileSync("input/2020/2020.14.input", #utf8)
let rows2 = data2->Js.String2.split("\n")
let operations = rows2->AddressOperation.make
let state = operations->DockingMemoryV2.start
// state.memory->MemoryV2.print;

let sum = state->DockingMemoryV2.memorySum

Js.log(sum->Int64.to_string)
