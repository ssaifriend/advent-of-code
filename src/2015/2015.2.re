module Dimension = {
  type t = {
    l: int,
    w: int,
    h: int,
  };

  let make = input => {
    let inputs =
      input->Js.String2.split("x")->Belt.Array.keepMap(Belt.Int.fromString);

    {l: inputs[0], w: inputs[1], h: inputs[2]};
  };
};

// part1
module WrappingPaper = {
  let calculate = (dimension: Dimension.t) => {
    let calculates = [|
      dimension.h * dimension.l,
      dimension.l * dimension.w,
      dimension.w * dimension.h,
    |];
    let extra =
      calculates
      ->Belt.Set.Int.fromArray
      ->Belt.Set.Int.minimum
      ->Belt.Option.getExn;

    calculates->Belt.Array.reduce(0, (+)) * 2 + extra;
  };
};

// part1 sample
"2x3x4
1x1x10"
->Js.String2.split("\n")
->Belt.Array.map(Dimension.make)
->Belt.Array.map(WrappingPaper.calculate)
->Belt.Array.forEach(Js.log);

Node.Fs.readFileAsUtf8Sync("input/2015/2015.2.input")
->Js.String2.split("\n")
->Belt.Array.map(Dimension.make)
->Belt.Array.map(WrappingPaper.calculate)
->Belt.Array.reduce(0, (+))
->Js.log;


// part2
module WrappingRibbon = {
  let makePresent = (dimension: Dimension.t) => {
    let dimensions =
      [dimension.l, dimension.w, dimension.h]
      ->Belt.List.sort((a, b) => a - b)
      ->Belt.List.toArray;

    (dimensions[0] + dimensions[1]) * 2;
  };

  let calculate = (dimension: Dimension.t) => {
    let present = dimension->makePresent;
    let bow = dimension.l * dimension.w * dimension.h;

    present + bow;
  };
};

"2x3x4
1x1x10"
->Js.String2.split("\n")
->Belt.Array.map(Dimension.make)
->Belt.Array.map(WrappingRibbon.calculate)
->Belt.Array.forEach(Js.log);

Node.Fs.readFileAsUtf8Sync("input/2015/2015.2.input")
->Js.String2.split("\n")
->Belt.Array.map(Dimension.make)
->Belt.Array.map(WrappingRibbon.calculate)
->Belt.Array.reduce(0, (+))
->Js.log;

