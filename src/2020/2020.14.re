module MaskBit = {
  type t =
    | Zero(int)
    | One(int)
    | None(int);

  let makeZeroMaskBit = (index, maskLen) => {
    let actualBit = maskLen - index;
    Int64.max_int
    ->Int64.shift_right(actualBit)
    ->Int64.shift_left(actualBit)
    ->Int64.add(
        Int64.one
        ->Int64.shift_left(actualBit)
        ->Int64.sub(Int64.one)
        ->Int64.shift_right(1),
      );
  };
};

module type MaskOperation = {
  type t;

  let converter: array(MaskBit.t) => t;
};

module Mask = (Converter: MaskOperation) => {
  type t = Converter.t;

  module List = Belt.List;

  let make = inputStr => {
    inputStr->Js.String2.split(" = ")[1]
    ->Js.String2.split("")
    ->Belt.Array.mapWithIndex((index, bit) => {
        switch (bit) {
        | "0" => MaskBit.Zero(index)
        | "1" => One(index)
        | _ => None(index)
        }
      })
    ->Converter.converter;
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

module Operation = (Converter: MaskOperation) => {
  type t =
    | Mask(Converter.t)
    | MemoryOperation(MemoryOperation.t);

  exception InvalidOperations;

  module MaskConveter = Mask(Converter);
  module Array = Belt.Array;

  let make = inputStrs => {
    inputStrs->Belt.Array.map(inputStr =>
      switch (inputStr->Js.String2.slice(~from=0, ~to_=4)) {
      | "mask" => Mask(inputStr->MaskConveter.make)
      | "mem[" => MemoryOperation(inputStr->MemoryOperation.make)
      | _ => raise(InvalidOperations)
      }
    );
  };
};

// part1
module ValueMaskConverter = {
  type t = {
    one: list(Int64.t),
    zero: list(Int64.t),
  };

  let maskLen = 36; // 미리 정의된 상수

  let converter = maskInput => {
    let masks = {one: [], zero: []};

    maskInput->Belt.Array.reduce(masks, (masks, mask) => {
      switch (mask) {
      | MaskBit.Zero(index) => {
          ...masks,
          zero: [index->MaskBit.makeZeroMaskBit(maskLen), ...masks.zero],
        }
      | One(index) =>
        let actualBit = maskLen - index - 1;
        {
          ...masks,
          one: [Int64.one->Int64.shift_left(actualBit), ...masks.one],
        };
      | None(_) => masks
      }
    });
  };
};

module ValueOperation = Operation(ValueMaskConverter);

module Memory = {
  type t = Belt.Map.Int.t(Int64.t);

  module Map = Belt.Map.Int;

  let print = memory => {
    memory->Map.forEach((key, value) => {
      Js.log(key->Belt.Int.toString ++ " / " ++ value->Int64.to_string)
    });
  };
};

module DockingMemory = {
  type t = {
    memory: Memory.t,
    currentMask: ValueOperation.MaskConveter.t,
  };

  let rec operation = (state, operations) => {
    let makeValue =
        (op: MemoryOperation.t, mask: ValueOperation.MaskConveter.t) => {
      let zeroProcessedValue =
        mask.zero
        ->ValueOperation.MaskConveter.List.reduce(
            Int64.of_int(op.value), (value, maskValue) => {
            value->Int64.logand(maskValue)
          });

      mask.one
      ->ValueOperation.MaskConveter.List.reduce(
          zeroProcessedValue, (value, maskValue) => {
          value->Int64.logor(maskValue)
        });
    };

    let result =
      switch (operations[0]) {
      | ValueOperation.Mask(m) => {...state, currentMask: m}
      | MemoryOperation(op) =>
        let value = op->makeValue(state.currentMask);

        {...state, memory: state.memory->Memory.Map.set(op.address, value)};
      };

    // result.memory->Memory.print;
    if (operations->ValueOperation.Array.size == 1) {
      result;
    } else {
      result->operation(operations->ValueOperation.Array.sliceToEnd(1));
    };
  };

  let start = (operations: array(ValueOperation.t)) => {
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

let data = Node.Fs.readFileSync("input/2020/2020.14.sample", `utf8);
let rows = data->Js.String2.split("\n");
let operations = rows->ValueOperation.make;
let state = operations->DockingMemory.start;
// state.memory->Memory.print;

let sum = state->DockingMemory.memorySum;

Js.log(sum->Int64.to_string);

// part2
module AddressMaskConverter = {
  type t = {
    one: list(Int64.t),
    x: list(int),
  };

  let maskLen = 36; // 미리 정의된 상수

  let converter = maskInput => {
    let masks = {one: [], x: []};

    maskInput->Belt.Array.reduce(masks, (masks, mask) => {
      switch (mask) {
      | MaskBit.None(index) =>
        {...masks, x: [index, ...masks.x]};
      | One(index) =>
        let actualBit = maskLen - index - 1;
        {
          ...masks,
          one: [Int64.one->Int64.shift_left(actualBit), ...masks.one],
        };
      | Zero(_) => masks
      }
    });
  };
};

module AddressOperation = Operation(AddressMaskConverter);

module Int64Cmp =
  Belt.Id.MakeComparable({
    type t = Int64.t;
    let cmp = (a, b) => Int64.compare(a, b);
  });

module MemoryV2 = {
  type t = Belt.Map.t(Int64.t, int, Int64Cmp.identity);

  module Map = Belt.Map;

  let print = memory => {
    memory->Map.forEach((key, value) => {
      Js.log(key->Int64.to_string ++ " / " ++ value->Belt.Int.toString)
    });
  };
};

module DockingMemoryV2 = {
  type t = {
    memory: MemoryV2.t,
    currentMask: AddressOperation.MaskConveter.t,
  };

  let rec makeIndexSets = (indexes, sets) =>
    if (sets->Belt.List.size == 0) {
      let newSets = [[(indexes[0], 0)], [(indexes[0], 1)]];
      indexes->Belt.Array.sliceToEnd(1)->makeIndexSets(newSets);
    } else {
      let appendSet = [(indexes[0], 0), (indexes[0], 1)];
      let newSets =
        sets
        ->Belt.List.map(set => {
            appendSet->Belt.List.map(indexSet => [indexSet, ...set])
          })
        ->Belt.List.toArray
        ->Belt.List.concatMany;

      indexes->Belt.Array.size == 1
        ? newSets : indexes->Belt.Array.sliceToEnd(1)->makeIndexSets(newSets);
    };

  let rec operation = (state, operations) => {
    let makeAddresses =
        (op: MemoryOperation.t, mask: AddressOperation.MaskConveter.t) => {
      let oneProcessedAddress =
        mask.one
        ->AddressOperation.MaskConveter.List.reduce(
            Int64.of_int(op.address), (address, maskValue) => {
            address->Int64.logor(maskValue)
          });

      mask.x
      ->AddressOperation.MaskConveter.List.toArray
      ->makeIndexSets([])
      ->Belt.List.map(sets => {
          sets->Belt.List.reduce(
            oneProcessedAddress, (address, (index, bit)) => {
            let rightLen = AddressMaskConverter.maskLen - index - 1;
            bit == 1
              ? address->Int64.logor(Int64.one->Int64.shift_left(rightLen))
              : address->Int64.logand(
                  index->MaskBit.makeZeroMaskBit(
                    AddressMaskConverter.maskLen,
                  ),
                )
          })
        })
      ->Belt.List.toArray;
    };

    let result =
      switch (operations[0]) {
      | AddressOperation.Mask(m) => {...state, currentMask: m}
      | MemoryOperation(op) =>
        op
        ->makeAddresses(state.currentMask)
        ->AddressOperation.Array.reduce(state, (oldState, address) => {
            {
              ...oldState,
              memory: oldState.memory->MemoryV2.Map.set(address, op.value),
            }
          })
      };

    // result.memory->MemoryV2.print;
    if (operations->AddressOperation.Array.size == 1) {
      result;
    } else {
      result->operation(operations->AddressOperation.Array.sliceToEnd(1));
    };
  };

  let start = (operations: array(AddressOperation.t)) => {
    let initialState = {
      memory: Belt.Map.make(~id=(module Int64Cmp)),
      currentMask: {
        one: [],
        x: [],
      },
    };

    initialState->operation(operations);
  };

  let memorySum = state => {
    state.memory
    ->MemoryV2.Map.reduce(Int64.zero, (sum, _, value) =>
        sum->Int64.add(value->Int64.of_int)
      );
  };
};

let data2 = Node.Fs.readFileSync("input/2020/2020.14.2.sample", `utf8);
// let data2 = Node.Fs.readFileSync("input/2020/2020.14.input", `utf8);
let rows2 = data2->Js.String2.split("\n");
let operations = rows2->AddressOperation.make;
let state = operations->DockingMemoryV2.start;
// state.memory->MemoryV2.print;

let sum = state->DockingMemoryV2.memorySum;

Js.log(sum->Int64.to_string);
