let sample = "2x3x4
1x1x10"

type dimension = {
    l: option(int),
    w: option(int),
    h: option(int),
};

let eachSample = Js.String.split("\n", sample)
let dimensionStrs = Belt.Array.forEach(eachSample, (str) => Js.String.split("x", str))
let dimensions = Belt.Array.forEach(dimensionStrs, (dimensionStr) => {l: Belt.Int.fromString(dimensionStr[0]), w: Belt.Int.fromString(dimensionStr[1]), h: Belt.Int.fromString(dimensionStr[2])});

let size = Array.fold_left(
    ()
);
