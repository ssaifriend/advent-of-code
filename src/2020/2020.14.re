module Memory = {
  type t = Belt.Map.Int.t(Int64.t);

  module Map = Belt.Map.Int;

  let print = memory => {
    memory->Map.forEach((key, value) => {
      Js.log(key->Belt.Int.toString ++ " / " ++ value->Int64.to_string)
    });
  };
};

module Mask = {
  type t = {
    one: list(Int64.t),
    zero: list(Int64.t),
  };

  let maskLen = 36; // 미리 정의된 상수

  module List = Belt.List;

  type bit =
    | Zero(int)
    | One(int)
    | None;

  let make = inputStr => {
    let masks = {one: [], zero: []};

    let makeZeroMaskBit = index => {
      let actualBit = maskLen - index;
      Int64.max_int
      ->Int64.shift_right(actualBit)
      ->Int64.shift_left(actualBit)
      ->Int64.add(
          Int64.one
          ->Int64.shift_left(actualBit)
          ->Int64.sub(Int64.one)
          ->Int64.shift_right(1)
        );
    };

    inputStr->Js.String2.split(" = ")[1]
    ->Js.String2.split("")
    ->Belt.Array.mapWithIndex((index, bit) => {
        switch (bit) {
        | "0" => Zero(index)
        | "1" => One(index)
        | _ => None
        }
      })
    ->Belt.Array.reduce(masks, (masks, mask) => {
        switch (mask) {
        | Zero(index) => {
            ...masks,
            zero: [index->makeZeroMaskBit, ...masks.zero],
          }
        | One(index) =>
          let actualBit = maskLen - index - 1;
          {
            ...masks,
            one: [Int64.one->Int64.shift_left(actualBit), ...masks.one],
          };
        | None => masks
        }
      });
  };
};

module MemoryOperation = {
  type t = {
    address: int,
    value: int,
  };

  exception InvalidInput;

  let make = inputStr => {
    let re = Js.Re.fromString("mem\[([0-9]+)\] = ([0-9]+)");
    let result = re->Js.Re.exec_(inputStr);
    switch (result) {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)
        ->Belt.Array.keepMap(Js.Nullable.toOption)
        ->Belt.Array.keepMap(Belt.Int.fromString);
      {address: captures[0], value: captures[1]}; // 전체 문자인 0번은 빠짐
    | None => raise(InvalidInput)
    };
  };
};

module Operation = {
  type t =
    | Mask(Mask.t)
    | MemoryOperation(MemoryOperation.t);

  exception InvalidOperations;

  let make = inputStr => {
    switch (inputStr->Js.String2.slice(~from=0, ~to_=4)) {
    | "mask" => Mask(inputStr->Mask.make)
    | "mem[" => MemoryOperation(inputStr->MemoryOperation.make)
    | _ => raise(InvalidOperations)
    };
  };
};

module Operations = {
  type t = array(Operation.t);

  module Array = Belt.Array;

  let make = inputStrs => {
    inputStrs->Belt.Array.map(Operation.make);
  };
};

// part1
module DockingMemory = {
  open Operation;

  type t = {
    memory: Memory.t,
    currentMask: Mask.t,
  };

  let rec operation = (state, operations) => {
    let makeValue = (op: MemoryOperation.t, mask: Mask.t) => {
      let zeroProcessedValue =
        mask.zero
        ->Mask.List.reduce(Int64.of_int(op.value), (value, maskValue) => {
            value->Int64.logand(maskValue)
          });

      mask.one
      ->Mask.List.reduce(zeroProcessedValue, (value, maskValue) => {
          value->Int64.logor(maskValue)
        });
    };

    let result =
      switch (operations[0]) {
      | Mask(m) => {...state, currentMask: m}
      | MemoryOperation(op) =>
        let value = op->makeValue(state.currentMask);

        {...state, memory: state.memory->Memory.Map.set(op.address, value)};
      };

    // state.memory->Memory.print;
    if (operations->Operations.Array.size == 1) {
      result;
    } else {
      result->operation(operations->Operations.Array.sliceToEnd(1));
    };
  };

  let start = (operations: Operations.t) => {
    let initialState = {
      memory: Memory.Map.fromArray([||]),
      currentMask: {
        one: [],
        zero: [],
      },
    };

    initialState->operation(operations);
  };

  let memorySum = state => {
    state.memory
    ->Memory.Map.reduce(Int64.zero, (sum, _, value) =>
        sum->Int64.add(value)
      );
  };
};

let data = Node.Fs.readFileSync("input/2020/2020.14.input", `utf8);
let rows = data->Js.String2.split("\n");
let operations = rows->Operations.make;
let state = operations->DockingMemory.start;
// state.memory->Memory.print;

let sum = state->DockingMemory.memorySum;

Js.log(sum->Int64.to_string);
